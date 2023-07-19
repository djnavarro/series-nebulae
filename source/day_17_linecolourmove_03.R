library(tidyverse)

nits <- 200000
bg <- "black"
version <- "03"
seed <- 106
fname <- paste0("day_17_linecolourmove_", version, "_", seed, ".jpg")

set.seed(seed)

dat <- tibble(
  x0 = .5,
  y0 = .5,
  x1 = .5,
  y1 = .6,
  hue = .5,
  sat = .5,
  val = .5,
  opc = .5
)

update <- function(x, w, a, b) {
  x <- x + w + rnorm(n = 1, mean = a, sd = b)
  x <- min(1, x)
  x <- max(0, x)
  return(x)
}

iterate <- function(df, it, w = 0, a = 0, b = .003) {
  if(it %% 500 == 0) cat(".")
  if(it %% 10000 == 0) cat(it, "\n")
  tibble(
    x0 = update(df$x1, w, a, b),
    y0 = update(df$y1, w, a, b),
    x1 = update(df$x1, w, a, b),
    y1 = update(df$y1, w, a, b),
    hue = update(df$hue, w, a, b),
    sat = update(df$sat, w, a, b),
    val = update(df$val, w, a, b),
    opc = df$opc #update(df$opc, w, a, b)
  )
}

dat <- dat %>%
  accumulate(1:nits, iterate, .init = .) %>%
  bind_rows() %>%
  slice(-(1:100)) %>%
  mutate(clr = hsv(hue, sat, val, opc))

pic <- dat %>%
  ggplot(aes(
    x = x0,
    y = y0,
    xend = x1,
    yend = y1,
    colour = clr
  )) +
  geom_segment(show.legend = FALSE, size = .1) +
  theme_void() +
  theme(panel.background = element_rect(fill = bg, colour = bg)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  NULL

ggsave(
  filename = here::here("image", fname),
  plot = pic,
  width = 5000/300,
  height = 5000/300,
  dpi = 300
)
