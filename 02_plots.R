library(tidyverse)

xrf_long <- read_rds("data/xrf_long.rds")

xrf_box_rep_plot_small_window <- xrf_long %>% 
  filter(!treatment == "standard" & !window_settings == "1 x 15 mm") %>% 
  ggplot() +
  geom_boxplot(aes(x = concentration, y = element, color = treatment)) +
  facet_wrap(.~standard_ID, scales = "free")