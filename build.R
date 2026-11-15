# Build danieldsjoberg.com: assembles src/ + data/ into docs/ (served by GitHub Pages).
# Usage: Rscript build.R
# Requires: scholar (optional at build time; falls back to data/publications.json), yaml, jsonlite

base_url <- "https://www.danieldsjoberg.com"
scholar_id <- "4empyKkAAAAJ"
src <- "src"
out <- "docs"

# ---- helpers -----------------------------------------------------------------
read_file <- function(path) paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
write_file <- function(x, path) writeLines(enc2utf8(x), path, useBytes = TRUE)

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

# escape, then allow `code` and **bold**
inline_md <- function(x) {
  x <- esc(trimws(x))
  x <- gsub("`([^`]+)`", "<code>\\1</code>", x)
  gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", x)
}

fill <- function(template, values) {
  for (key in names(values)) {
    template <- gsub(paste0("{{", key, "}}"), values[[key]], template, fixed = TRUE)
  }
  template
}

# ---- software cards ------------------------------------------------------------
software <- yaml::read_yaml("data/software.yml")

render_card <- function(p) {
  logo <- if (!is.null(p$logo)) {
    sprintf(
      '<img class="card-logo" src="img/logos/%s" alt="" width="%d" height="%d" loading="lazy" decoding="async">',
      p$logo$file, p$logo$width, p$logo$height
    )
  } else ""
  tags <- paste0("<li>", esc(unlist(p$languages)), "</li>", collapse = "")
  details <- if (length(p$details)) {
    paste0(
      "<details><summary>Features</summary><ul>",
      paste0("<li>", vapply(p$details, inline_md, ""), "</li>", collapse = ""),
      "</ul></details>"
    )
  } else ""
  links <- if (length(p$links)) {
    paste0(
      vapply(p$links, function(l) sprintf('<a href="%s">%s</a>', esc(l$url), esc(l$text)), ""),
      collapse = " · "
    )
  } else ""
  if (nzchar(links)) links <- paste0("<p>", links, "</p>")
  install <- if (!is.null(p$install)) {
    sprintf('<pre class="install"><code>%s</code></pre>', esc(p$install))
  } else "<div></div>"

  sprintf(
    paste(
      '      <article class="card">',
      '        <div class="card-head">%s<div><h3 class="card-title"><a href="%s">%s</a></h3>',
      '          <ul class="tags" role="list" aria-label="Languages">%s</ul></div></div>',
      '        <p class="card-summary">%s</p>',
      '        <div class="card-extra">%s%s</div>',
      '        %s',
      '      </article>',
      sep = "\n"
    ),
    logo, esc(p$url), esc(p$name), tags, inline_md(p$summary), details, links, install
  )
}

cards_for <- function(filter) paste(vapply(Filter(filter, software), render_card, ""), collapse = "\n")

# ---- publications ---------------------------------------------------------------
pubs_cache <- "data/publications.json"

# Google Scholar keeps many unmerged variants of the same work (e.g. 26 copies of the
# gtsummary R Journal paper, each with citation junk appended to the title). Reduce each
# title to a key so variants collapse to one entry.
publication_key <- function(title, journal) {
  t <- title
  # "Treis" + venue "T. ggsurvfit: ..." -> use the venue as the title
  split <- !grepl(" ", trimws(t)) & grepl("^[A-Z]\\. [A-Za-z][A-Za-z0-9.]*: ", journal)
  t[split] <- sub("^[A-Z]\\. ", "", journal[split])
  t <- sub("^[A-Z][a-z]+ [A-Z]\\. ", "", t)                      # stray author: "Fei T. "
  pkg <- "^([A-Za-z][A-Za-z0-9.]*): "                             # "gtsummary: ..."
  prefix <- ifelse(grepl(pkg, t), tolower(sub(paste0(pkg, ".*$"), "\\1", t)), "")
  # corrections and "Re:" commentaries are separate works, so keep their prefix in the key
  strip <- nzchar(prefix) & !prefix %in% c("correction", "erratum", "re", "reply", "comment")
  t[strip] <- sub(pkg, "", t[strip])
  t <- sub("(\\. | \\(| \\[|, (19|20)\\d\\d| R package).*$", "", t)   # citation junk
  gsub("[^a-z0-9]", "", tolower(t))
}

# Same journal, volume, issue and first page = same paper, even when the title is worded
# differently ("magnetic resonance imaging" vs "MRI"). NA when the citation can't be parsed.
citation_key <- function(journal, number) {
  m <- regmatches(number, regexec("^\\s*(\\d+)\\s*\\((\\d+)\\),\\s*(\\d+)", number))
  vol <- vapply(m, function(x) if (length(x)) paste(x[2:4], collapse = "-") else NA_character_, "")
  ifelse(is.na(vol) | trimws(journal) == "", NA, paste(gsub("[^a-z]", "", tolower(journal)), vol))
}

fetch_publications <- function() {
  df <- scholar::get_publications(scholar_id)
  if (!is.data.frame(df) || nrow(df) == 0) stop("Scholar returned no publications")
  df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)
  df$number[is.na(df$number)] <- ""
  df <- df[
    !is.na(df$year) &
      df$title != toupper(df$title) &
      !grepl("Supplement", df$number, fixed = TRUE) &
      !grepl("_suppl", df$number, fixed = TRUE) &
      !(df$journal %in% "European Urology Supplements"),
  ]
  # keep the most-cited variant of each work (Scholar's main, merged entry)
  df <- df[order(-df$cites), ]
  df <- df[!duplicated(publication_key(df$title, df$journal)), ]
  ck <- citation_key(df$journal, df$number)
  df <- df[is.na(ck) | !duplicated(ck), ]
  first_author <- startsWith(df$author, "DD Sjoberg") | startsWith(df$author, "D Sjoberg")
  good_journal <- tolower(df$journal) %in%
    c("journal of clinical oncology", "the new england journal of medicine", "new england journal of medicine")

  author <- gsub("...", "and others", df$author, fixed = TRUE)
  has_dd <- grepl("DD Sjoberg", author, fixed = TRUE)
  has_d <- !has_dd & grepl("D Sjoberg", author, fixed = TRUE)
  missing <- !grepl("Sjoberg", author, fixed = TRUE)
  author[has_d] <- sub("D Sjoberg", "DD Sjoberg", author[has_d], fixed = TRUE)
  author[missing] <- sub("and others", "DD Sjoberg, and others", author[missing], fixed = TRUE)

  ord <- order(-first_author, -good_journal, -df$year)
  items <- data.frame(
    author = author, year = df$year, title = df$title,
    journal = df$journal, number = df$number, pubid = df$pubid,
    stringsAsFactors = FALSE
  )[ord, ]
  list(updated = format(Sys.Date()), items = items)
}

pubs <- tryCatch(
  {
    p <- fetch_publications()
    jsonlite::write_json(p, pubs_cache, pretty = TRUE, auto_unbox = TRUE)
    message("Fetched ", nrow(p$items), " publications from Google Scholar")
    p
  },
  error = function(e) {
    message("Google Scholar fetch failed (", conditionMessage(e), "); using ", pubs_cache)
    if (!file.exists(pubs_cache)) stop("No cached publications available")
    jsonlite::read_json(pubs_cache, simplifyVector = TRUE)
  }
)

# "D Sjöberg" (with an umlaut) is a different researcher whose papers Scholar attributes
# to this profile. Match both composed and decomposed forms of "ö".
# A few of his papers are spelled without the umlaut, so they are excluded by Scholar ID.
other_sjoberg_ids <- c(
  "u0Mu_IsstPMC", # Coherent effects in single scattering and random errors in antenna technology (2005)
  "WgvcDLhf7hwC", # Characteristic Modes and Nonreciprocity (2024)
  "inmFHauC9wsC"  # Optical theorem and forward scattering sum rule for periodic structures (2012)
)
other_sjoberg <- grepl("Sjöberg|Sjöberg", enc2utf8(pubs$items$author), ignore.case = TRUE) |
  pubs$items$pubid %in% other_sjoberg_ids
pubs$items <- pubs$items[!other_sjoberg, ]
message("Excluded ", sum(other_sjoberg), " publications by D Sjöberg (a different author)")

# Scholar sometimes merges a degree into the initials ("MSDD Sjoberg"); restore "DD Sjoberg".
pubs$items$author <- gsub("\\bMSDD Sjoberg\\b", "DD Sjoberg", pubs$items$author, perl = TRUE)

# Software citations are never sorted to the top: they move below the papers. They are
# entries with a CRAN URL, an "R package version" note, a bare version number like
# "(1.7. 0)", a venue of just "R package"/"Github", or no venue and a "pkg: Description"
# title (optionally prefixed by a stray author such as "Fei T. "). order() is stable, so
# both groups keep the original sort.
pub_text <- paste(pubs$items$title, pubs$items$journal, pubs$items$number)
no_venue <- trimws(pubs$items$journal) == "" & trimws(pubs$items$number) == ""
is_software <-
  grepl("CRAN|R-project|package version|\\[Internet\\]", pub_text, ignore.case = TRUE) |
  grepl("\\(\\d+\\.\\s?\\d+(\\.\\s?\\d+)?\\)", pubs$items$title, perl = TRUE) |
  tolower(trimws(pubs$items$journal)) %in% c("r package", "github") |
  (no_venue & grepl("^([A-Z][a-z]+ [A-Z]\\. )?[A-Za-z][A-Za-z0-9.]*: ", pubs$items$title, perl = TRUE)) |
  # Scholar sometimes splits a citation so the venue reads like "T. ggsurvfit: flexible ..."
  grepl("^([A-Z]\\. )?[a-z][A-Za-z0-9.]*: ", trimws(pubs$items$journal), perl = TRUE)
pubs$items <- pubs$items[order(is_software), ]
message("Moved ", sum(is_software), " software citations below the papers")

render_pub <- function(i) {
  r <- pubs$items[i, ]
  authors <- gsub("DD Sjoberg", "<strong>DD Sjoberg</strong>", esc(r$author), fixed = TRUE)
  link <- sprintf(
    "https://scholar.google.com/citations?view_op=view_citation&amp;hl=en&amp;user=%s&amp;citation_for_view=%s:%s",
    scholar_id, scholar_id, esc(r$pubid)
  )
  venue <- esc(r$journal)
  if (nzchar(r$number)) venue <- paste0('<span class="venue">', venue, "</span>, ", esc(r$number))
  else venue <- paste0('<span class="venue">', venue, "</span>")
  sprintf(
    '    <li><p><span class="authors">%s</span> (%s). <cite><a href="%s">%s</a></cite>. %s</p></li>',
    authors, r$year, link, esc(r$title), venue
  )
}
publications_html <- paste(vapply(seq_len(nrow(pubs$items)), render_pub, ""), collapse = "\n")

# ---- assemble pages -----------------------------------------------------------
partials <- lapply(
  c(head = "head", header = "header", footer = "footer", social = "social"),
  function(n) read_file(file.path(src, "partials", paste0(n, ".html")))
)

unlink(out, recursive = TRUE)
dir.create(out)
for (d in c("css", "fonts", "img")) file.copy(file.path(src, d), out, recursive = TRUE)
invisible(file.copy("CNAME", out))
invisible(file.create(file.path(out, ".nojekyll")))

pages <- c("index.html", "software.html", "publications.html", "contact.html")
for (page in pages) {
  canonical <- if (page == "index.html") paste0(base_url, "/") else paste0(base_url, "/", page)
  header <- sub(
    sprintf('<a href="%s">', page),
    sprintf('<a href="%s" aria-current="page">', page),
    partials$header, fixed = TRUE
  )
  html <- read_file(file.path(src, page))
  html <- fill(html, list(head = partials$head, header = header, footer = partials$footer))
  html <- fill(html, list(
    social = partials$social,
    canonical = canonical,
    base_url = base_url,
    year = format(Sys.Date(), "%Y"),
    software_maintainer = cards_for(function(p) identical(p$section, "maintainer")),
    software_author = cards_for(function(p) identical(p$section, "author")),
    software_contributions = cards_for(function(p) identical(p$section, "contributor")),
    publications = publications_html,
    pubs_updated = format(as.Date(pubs$updated), "%B %Y")
  ))
  leftover <- regmatches(html, gregexpr("\\{\\{[a-z_]+\\}\\}", html))[[1]]
  if (length(leftover)) stop(page, " has unfilled placeholders: ", paste(leftover, collapse = ", "))
  write_file(html, file.path(out, page))
}

sitemap <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n',
  paste0("  <url><loc>", base_url, c("/", "/software.html", "/publications.html"), "</loc></url>", collapse = "\n"),
  "\n</urlset>"
)
write_file(sitemap, file.path(out, "sitemap.xml"))
message("Built ", length(pages), " pages into ", out, "/")
