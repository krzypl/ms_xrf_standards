library(tidyverse)
theme_set(theme_bw())

xrf_long <- read_rds("data/xrf_long.rds")

## Fig. S2a ----
standard_conc <- xrf_long %>% 
  filter(treatment == "standard") %>%
  select(standard_ID, element, concentration) %>% 
  rename(standard_conc = concentration)

rap_vs_meas_dry_ws_s <- xrf_long %>% 
  filter(treatment == "dry" & window_settings == "1 x 15 mm") %>% 
  filter(!is.na(concentration) & !concentration == 0) %>% 
  group_by(standard_ID, element, replication) %>% 
  summarise(med_conc = median(concentration)) %>% 
  ungroup() %>% 
  group_by(standard_ID, element) %>% 
  summarise(measured_conc = mean(med_conc)) %>% 
  left_join(standard_conc) %>% 
  mutate(configuration = "Dry, WS 1 x 15 mm")

write_csv(rap_vs_meas_dry_ws_s, "data/rap_vs_meas_dry_ws_s.csv")

rap_vs_meas_dry_ws_s_plot <- ggplot(rap_vs_meas_dry_ws_s, 
                                    aes(x = standard_conc, y = measured_conc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ element, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(A) Dry, WS 1 x 15 mm") +
  labs(x = "True concentration (ppm)", y = "Averaged measured concentration (ppm)")# +
  theme(plot.title = element_text(size = 10))

ggsave(filename = "figures/fig_s2a_true_vs_measured_conc_dry_small.svg",
       plot = rap_vs_meas_dry_ws_s_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/fig_s2a_true_vs_measured_conc_dry_small.pdf",
       plot = rap_vs_meas_dry_ws_s_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
ggsave(filename = "figures/fig_s2a_true_vs_measured_conc_dry_small.jpg",
       plot = rap_vs_meas_dry_ws_s_plot,
       width = 6.5,
       height = 7,
#       dpi = 900,
       device = "jpg")

## Fig. S2b ----

rap_vs_meas_dry_ws_l <- xrf_long %>% 
  filter(treatment == "dry" & window_settings == "10 x 15 mm") %>% 
  filter(!is.na(concentration) & !concentration == 0) %>% 
  group_by(standard_ID, element, replication) %>% 
  summarise(med_conc = median(concentration)) %>% 
  ungroup() %>% 
  group_by(standard_ID, element) %>% 
  summarise(measured_conc = mean(med_conc)) %>% 
  left_join(standard_conc) %>% 
  mutate(configuration = "Dry, WS 10 x 15 mm")

write_csv(rap_vs_meas_dry_ws_l, "data/rap_vs_meas_dry_ws_l.csv")

rap_vs_meas_dry_ws_l_plot <- ggplot(rap_vs_meas_dry_ws_l, 
                                    aes(x = standard_conc, y = measured_conc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ element, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(B) Dry, WS 10 x 15 mm") +
  labs(x = "True concentration (ppm)", y = "Averaged measured concentration (ppm)") +
  theme(plot.title = element_text(size = 10))

ggsave(filename = "figures/fig_s2b_true_vs_measured_conc_dry_large.svg",
       plot = rap_vs_meas_dry_ws_l_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/fig_s2b_true_vs_measured_conc_dry_large.pdf",
       plot = rap_vs_meas_dry_ws_l_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
ggsave(filename = "figures/fig_s2b_true_vs_measured_conc_dry_large.png",
       plot = rap_vs_meas_dry_ws_l_plot,
       width = 6.5,
       height = 7,
       device = "png")

## Fig. S2C ----

rap_vs_meas_wet_ws_s <- xrf_long %>% 
  filter(treatment == "wet" & window_settings == "1 x 15 mm") %>% 
  filter(!is.na(concentration) & !concentration == 0) %>% 
  group_by(standard_ID, element, replication) %>% 
  summarise(med_conc = median(concentration)) %>% 
  ungroup() %>% 
  group_by(standard_ID, element) %>% 
  summarise(measured_conc = mean(med_conc)) %>% 
  left_join(standard_conc) %>% 
  mutate(configuration = "Wet, WS 1 x 15 mm")

write_csv(rap_vs_meas_wet_ws_s, "data/rap_vs_meas_wet_ws_s.csv")

rap_vs_meas_wet_ws_s_plot <- ggplot(rap_vs_meas_wet_ws_s, 
                                    aes(x = standard_conc, y = measured_conc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ element, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(C) Wet, WS 1 x 15 mm") +
  labs(x = "True concentration (ppm)", y = "Averaged measured concentration (ppm)") +
  theme(plot.title = element_text(size = 10))

ggsave(filename = "figures/fig_s2c_true_vs_measured_conc_wet_small.svg",
       plot = rap_vs_meas_wet_ws_s_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/fig_s2c_true_vs_measured_conc_wet_small.pdf",
       plot = rap_vs_meas_wet_ws_s_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
ggsave(filename = "figures/fig_s2c_true_vs_measured_conc_wet_small.png",
       plot = rap_vs_meas_wet_ws_s_plot,
       width = 6.5,
       height = 7,
       device = "png")

## Fig. S2D ----

rap_vs_meas_wet_ws_l <- xrf_long %>% 
  filter(treatment == "wet" & window_settings == "10 x 15 mm") %>% 
  filter(!is.na(concentration) & !concentration == 0) %>% 
  group_by(standard_ID, element, replication) %>% 
  summarise(med_conc = median(concentration)) %>% 
  ungroup() %>% 
  group_by(standard_ID, element) %>% 
  summarise(measured_conc = mean(med_conc)) %>% 
  left_join(standard_conc) %>% 
  mutate(configuration = "Wet, WS 10 x 15 mm")

write_csv(rap_vs_meas_wet_ws_l, "data/rap_vs_meas_wet_ws_l.csv")

rap_vs_meas_wet_ws_l_plot <- ggplot(rap_vs_meas_wet_ws_l, 
                                    aes(x = standard_conc, y = measured_conc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  facet_wrap(.~ element, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  theme(legend.position = "bottom") +
  ggtitle("(D) Wet, WS 10 x 15 mm") +
  labs(x = "True concentration (ppm)", y = "Averaged measured concentration (ppm)") +
  theme(plot.title = element_text(size = 10))

ggsave(filename = "figures/fig_s2d_true_vs_measured_conc_wet_large.svg",
       plot = rap_vs_meas_wet_ws_l_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/fig_s2d_true_vs_measured_conc_wet_large.pdf",
       plot = rap_vs_meas_wet_ws_l_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
ggsave(filename = "figures/fig_s2d_true_vs_measured_conc_wet_large.png",
       plot = rap_vs_meas_wet_ws_l_plot,
       width = 6.5,
       height = 7,
       device = "png")