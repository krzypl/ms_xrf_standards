library(tidyverse)
library(gridExtra)
theme_set(theme_bw())

xrf_long <- read_rds("data/xrf_long.rds")


scan_var_ws_l_1 <- xrf_long %>% 
  filter(
    standard_ID == "IAEA-457" & treatment == "wet" & window_settings == "10 x 15 mm" & replication == 1) %>%
  group_by(element) %>% 
  mutate(position = 1:10) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(y = concentration, x = position)) +
  #  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  geom_point(aes(y = concentration, x = position), size = 0.8) +
  facet_wrap(.~element, scales = "free", ncol = 4) +
  labs(y = "Concentration (ppm)",
       x = "Position (mm)") +
  ggtitle("(A) IAEA-457, WS: 1 x 15 mm, wet sample, replication 1") +
  theme(plot.title = element_text(size = 10))
  

scan_var_ws_l_2 <- xrf_long %>% 
  filter(
    standard_ID == "Carbonatite" & treatment == "dry" & window_settings == "10 x 15 mm" & replication == 1) %>%
  group_by(element) %>% 
  mutate(position = 1:10) %>% 
  ungroup() %>%
  filter(!element == "S") %>% 
  ggplot() +
  geom_line(aes(y = concentration, x = position)) +
  #  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  geom_point(aes(y = concentration, x = position), size = 0.8) +
  facet_wrap(.~element, scales = "free", ncol = 4) +
  labs(y = "Concentration (ppm)",
       x = "Position (mm)") +
  ggtitle("(B) Carbonatite, WS: 10 x 15 mm, dry sample, replication 1") +
  theme(plot.title = element_text(size = 10))

scan_var_plot <- grid.arrange(scan_var_ws_l_1,
                              scan_var_ws_l_2,
                              ncol = 1,
                              heights = c(1.5, 2.2))


ggsave(filename = "figures/fig_s1_var_along_sample.svg",
       plot = scan_var_plot,
       width = 6.5,
       height = 9,
       device = "svg")
ggsave(filename = "figures/fig_s1_var_along_sample.pdf",
       plot = scan_var_plot,
       width = 6.5,
       height = 9,
       device = "pdf")
ggsave(filename = "figures/fig_s1_var_along_sample.jpg",
       plot = scan_var_plot,
       width = 6.5,
       height = 9,
       device = "jpg")
