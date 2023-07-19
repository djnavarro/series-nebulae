library(tidyverse)
library(Rcpp)

sys_id <- "01"
sys_name <- "nebula"
sourceCpp(here::here("source", paste0(sys_name, "_", sys_id, ".cpp")))

nits <- 200000
bg <- "black"
seed <- 107
npix <- 2000
fname <- paste0(sys_name, "_", sys_id, "_", seed, "_", npix, ".jpg")

set.seed(seed)

dat <- nebula(nits) %>%
  as_tibble(.name_repair = "universal") %>%
  rename(
    x0 = ...1,
    y0 = ...2,
    x1 = ...3,
    y1 = ...4,
    hue = ...5,
    sat = ...6,
    val = ...7,
    opc = ...8
  ) %>%
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
  width = npix/300,
  height = npix/300,
  dpi = 300
)
