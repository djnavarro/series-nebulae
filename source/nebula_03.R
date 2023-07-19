library(tidyverse)
library(Rcpp)

sys_id <- "03"
sys_name <- "nebula"
sourceCpp(here::here("source", paste0(sys_name, "_", sys_id, ".cpp")))

nits <- 5000000
bg <- "black"
resolutions <- 5000

seeds <- 132:140

for(seed in seeds) {

  cat("seed", seed, "\n")
  set.seed(seed)

  shades <- sample(colours(distinct = TRUE), 4)

  dat <- nebula(nits) %>%
    as_tibble(.name_repair = "universal") %>%
    rename(
      x0 = ...1,
      y0 = ...2,
      x1 = ...3,
      y1 = ...4,
      shade = ...5
    ) %>%
    slice(-(1:100))

  pic <- dat %>%
    ggplot(aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      colour = shade
    )) +
    geom_segment(show.legend = FALSE, size = .1, alpha = .1) +
    theme_void() +
    theme(panel.background = element_rect(fill = bg, colour = bg)) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_color_gradientn(colours = shades) +
    NULL


  for(npix in resolutions) {

    fname <- paste0(sys_name, "_", sys_id, "_", seed, "_", npix, ".jpg")

    ggsave(
      filename = here::here("image", fname),
      plot = pic,
      width = npix/300,
      height = npix/300,
      dpi = 300
    )
  }

}
