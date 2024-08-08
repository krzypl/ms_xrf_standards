library(tidyverse)

xrf_long <- read_rds("data/xrf_long.rds")

xrf_box_rep_plot_small_window <- xrf_long %>% 
  filter(!treatment == "standard" & !window_settings == "10 x ") %>% 
  group_by(standard_ID, element) %>% 
  