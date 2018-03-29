library(tidyverse)
library(ggplot2)
library(tweenr)
library(gganimate)
library(plotly)

set.seed(234234)
sd = 0.1
x.seq = seq(0, 1, length.out = 50)
y.max = max(dnorm(x.seq, mean = 0, sd = sd))
y.seq = seq(0, y.max, length.out = 50)
m.seq = seq(min(x.seq) + sd*2.8, 
            max(x.seq) - sd*2.8, 
            length.out = 5)
m.seq = c(m.seq, rev(m.seq[c(-1, -length(m.seq))]))

xy = 
  tibble(
    x = x.seq,
    y = y.seq
  ) %>%
  complete(x, y)

df = 
  tibble(mean = m.seq) %>%
  mutate(time = row_number(),
         xy = rep(list(xy), nrow(.))) %>%
  unnest(xy) %>% # this makes on full xy set for each time/mean
  mutate(
    y.line = dnorm(x, mean = mean, sd = sd),
    rand = runif(nrow(.))
  ) %>%
  filter(y <= y.line) %>% # removing points above distrbution line
  arrange(time, rand) %>% #randomly sorting and assigning IDs
  group_by(time) %>%
  mutate(
    ease="linear",
    id = row_number()
  ) %>%
  ungroup %>%
  mutate(
    x = x + runif(nrow(.), x - 1/(length(x.seq) - 1)/2, x + 1/(length(x.seq) - 1)/2),
    y = y + runif(nrow(.), y - 1/(length(y.seq) - 1)/2, y + 1/(length(y.seq) - 1)/2)
  ) %>%
  arrange(time, mean)

## need the animation to end where it started
df = df %>%
  bind_rows(df %>% filter(time == 1) %>% mutate(time = length(m.seq) + 1))

ggplot(df %>% filter(time==.$time[40]), aes(x=x, y=y)) + geom_point()  

df_tween = tween_elements(df, "time", "id", "ease", nframes = 500) %>%
  mutate(color.id = .group)

p1 <- ggplot(df, aes(x = x, y = y, frame = time)) +
  theme_bw() + 
  geom_point(aes(color = id), size = 3) +
  theme(legend.position="none", 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())


# ggplotly(p1, height = 800, width = 1800) %>%
#   animation_opts(frame = 1500, 
#                  easing = "linear",
#                  redraw = F) %>%
#   animation_slider(p, hide = T) 



p2 <- ggplot(df_tween, aes(x = x, y = y, frame = .frame)) +
  theme_bw() + 
  geom_point(aes(color = color.id), size = 3) +
  theme(legend.position="none", 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) 




animation::ani.options(convert = "C:/Progra~1/ImageMagick-7.0.7-Q16/convert.exe")
gganimate(p2, filename="images/Normal Dist Bounce.gif",  title_frame = FALSE, interval = 0.06, 
          ani.width=1600, ani.height=275)

# gganimate(p2, filename="Normal Dist Bounce.html", title_frame = FALSE, interval = 0.1, 
#           ani.width=1600, ani.height=800)
