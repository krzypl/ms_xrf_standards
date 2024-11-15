library(tidyverse)
library(gridExtra)
theme_set(theme_bw())

# Read data ----
xrf_long <- read_rds("data/xrf_long.rds")

# Fig. 2. Elemental concentrations in standards -----

xrf_boxplot_prep <- xrf_long %>% 
  filter(treatment == "standard")

xrf_n_obs <- xrf_boxplot_prep %>% 
  group_by(element) %>% 
  summarise(n = n())

xrf_boxplot <- xrf_boxplot_prep %>% 
  left_join(xrf_n_obs, by = "element") %>% 
  mutate(id = paste(element, " (", n, ")", sep = "")) %>% 
  ggplot() +
  geom_boxplot(aes(y = concentration)) +
  facet_wrap(.~id, scales = "free") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(y = "Concentration (ppm)")

xrf_boxplot #numbers in parentheses indicate number of observations

ggsave(filename = "figures/fig_2_elemental_concentrations.svg",
       plot = xrf_boxplot,
       width = 6.5,
       height = 5,
       device = "svg")
ggsave(filename = "figures/fig_2_elemental_concentrations.pdf",
       plot = xrf_boxplot,
       width = 6.5,
       height = 5,
       device = "pdf")
ggsave(filename = "figures/fig_2_elemental_concentrations.jpg",
       plot = xrf_boxplot,
       width = 6.5,
       height = 5,
       device = "jpg")

# Fig. S1. Variability of concentrations along a sample ----
scan_var_ws_s_1 <- xrf_long %>% 
  filter(standard_ID == "IAEA-457" & treatment == "wet" & window_settings == "1 x 15 mm") %>%
  group_by(element, replication) %>% 
  mutate(step = as.integer(1:10)) %>%
  ungroup() %>% 
  group_by(element, step) %>% 
  summarise(mean_conc_4step = mean(concentration)) %>% 
  ggplot() +
  geom_line(aes(y = mean_conc_4step, x = step)) +
#  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  geom_point(aes(y = mean_conc_4step, x = step), size = 0.8) +
  facet_wrap(.~element, scales = "free", ncol = 8) +
  labs(y = "Concentration (ppm)",
       x = "Step") +
  ggtitle("(A) IAEA-457, WS: 1 x 15 mm") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 4),
        axis.text.y = element_text(size = 4),
        strip.text = element_text(size = 5),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        plot.title = element_text(size = 6))

scan_var_ws_l_1 <- xrf_long %>% 
  filter(standard_ID == "IAEA-457" & treatment == "wet" & window_settings == "10 x 15 mm") %>%
  group_by(element, replication) %>% 
  mutate(step = as.integer(1:10)) %>% 
  ungroup() %>% 
  group_by(element, step) %>% 
  summarise(mean_conc_4step = mean(concentration)) %>% 
  ggplot() +
  geom_line(aes(y = mean_conc_4step, x = step)) +
#  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  geom_point(aes(y = mean_conc_4step, x = step), size = 0.8) +
  facet_wrap(.~element, scales = "free", ncol = 8) +
  labs(y = "Concentration (ppm)",
       x = "Step") +
  ggtitle("(B) IAEA-457, WS: 10 x 15 mm") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 4),
        axis.text.y = element_text(size = 4),
        strip.text = element_text(size = 5),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        plot.title = element_text(size = 6))

scan_var_ws_s_2 <- xrf_long %>% 
  filter(standard_ID == "Carbonatite" & treatment == "wet" & window_settings == "1 x 15 mm") %>%
  group_by(element, replication) %>% 
  mutate(step = as.integer(1:10)) %>% 
  ungroup() %>% 
  group_by(element, step) %>% 
  summarise(mean_conc_4step = mean(concentration)) %>%
  filter(!element == "K" & !element == "S") %>% #values for each step = 0
  ggplot() +
  geom_line(aes(y = mean_conc_4step, x = step)) +
#  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  geom_point(aes(y = mean_conc_4step, x = step), size = 0.8) +
  facet_wrap(.~element, scales = "free", ncol = 8) +
  labs(y = "Concentration (ppm)",
       x = "Step") +
  ggtitle("(C) Carbonatite, WS: 1 x 15 mm") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 4),
        axis.text.y = element_text(size = 4),
        strip.text = element_text(size = 5),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        plot.title = element_text(size = 6))

scan_var_ws_l_2 <- xrf_long %>% 
  filter(standard_ID == "Carbonatite" & treatment == "wet" & window_settings == "10 x 15 mm") %>%
  group_by(element, replication) %>% 
  mutate(step = as.integer(1:10)) %>% 
  ungroup() %>% 
  group_by(element, step) %>% 
  summarise(mean_conc_4step = mean(concentration)) %>% 
  filter(!element == "S") %>% #values in each step = 0
  ggplot() +
  geom_line(aes(y = mean_conc_4step, x = step)) +
#  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  geom_point(aes(y = mean_conc_4step, x = step), size = 0.8) +
  facet_wrap(.~element, scales = "free", ncol = 8) +
  labs(y = "Concentration (ppm)",
       x = "Step") +
  ggtitle("(D) Carbonatite, WS: 10 x 15 mm") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 4),
        axis.text.y = element_text(size = 4),
        strip.text = element_text(size = 5),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        plot.title = element_text(size = 6))

scan_var_plot <- grid.arrange(scan_var_ws_s_1,
                              scan_var_ws_l_1,
                              scan_var_ws_s_2,
                              scan_var_ws_l_2,
                              ncol = 1,
                              heights = c(1, 1, 2, 2.5))

ggsave(filename = "figures/fig_s1_var_along_sample.svg",
       plot = scan_var_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/fig_s1_var_along_sample.pdf",
       plot = scan_var_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
ggsave(filename = "figures/fig_s1_var_along_sample.jpg",
       plot = scan_var_plot,
       width = 6.5,
       height = 7,
       device = "jpg")

# Fig. S2 True concetration vs. measured concentration ----

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

rap_vs_meas_dry_ws_s_plot <- ggplot(rap_vs_meas_dry_ws_s, 
                                    aes(x = standard_conc, y = measured_conc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ element, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(A) Dry, WS 1 x 15 mm") +
  labs(x = "True concentration (ppm)", y = "Measured concentration (ppm)")

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

rap_vs_meas_dry_ws_l_plot <- ggplot(rap_vs_meas_dry_ws_l, 
                                    aes(x = standard_conc, y = measured_conc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ element, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(B) Dry, WS 10 x 15 mm") +
  labs(x = "True concentration (ppm)", y = "Measured concentration (ppm)")

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
ggsave(filename = "figures/fig_s2b_true_vs_measured_conc_dry_large.jpg",
       plot = rap_vs_meas_dry_ws_l_plot,
       width = 6.5,
       height = 7,
       device = "jpg")

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

rap_vs_meas_wet_ws_s_plot <- ggplot(rap_vs_meas_wet_ws_s, 
                                    aes(x = standard_conc, y = measured_conc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ element, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(C) Wet, WS 1 x 15 mm") +
  labs(x = "True concentration (ppm)", y = "Measured concentration (ppm)")

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
ggsave(filename = "figures/fig_s2c_true_vs_measured_conc_wet_small.jpg",
       plot = rap_vs_meas_wet_ws_s_plot,
       width = 6.5,
       height = 7,
       device = "jpg")

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

rap_vs_meas_wet_ws_l_plot <- ggplot(rap_vs_meas_wet_ws_l, 
                                    aes(x = standard_conc, y = measured_conc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  facet_wrap(.~ element, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  theme(legend.position = "bottom") +
  ggtitle("Wet, WS 10 x 15 mm") +
  labs(x = "True concentration (ppm)", y = "Measured concentration (ppm)")

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
ggsave(filename = "figures/fig_s2d_true_vs_measured_conc_wet_large.jpg",
       plot = rap_vs_meas_wet_ws_l_plot,
       width = 6.5,
       height = 7,
       device = "jpg")

# Fig. S3 True ratio vs. measured ratio ----
## Fig. S3a ----

ratio_val_dry_ws_s <- rap_vs_meas_dry_ws_s %>% 
  filter(element == "Ca") %>% 
  select(!element) %>% 
  rename(measured_ca = measured_conc,
         standard_ca = standard_conc)

rap_vs_meas_dry_ws_s_ratio <- rap_vs_meas_dry_ws_s %>% 
  left_join(ratio_val_dry_ws_s) %>% 
  mutate(measured_ratio = measured_conc/measured_ca,
         standard_ratio = standard_conc/standard_ca,
         measured_ratio_log = log(measured_conc/measured_ca),
         standard_ratio_log = log(standard_conc/standard_ca)) %>% 
  filter(!element == "Ca") %>% 
  mutate(ratio_id = paste(element, "/Ca", sep = ""))


rap_vs_meas_dry_ws_s_ratio_plot <- ggplot(rap_vs_meas_dry_ws_s_ratio, 
                                          aes(x = standard_ratio, y = measured_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ ratio_id, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(A) Dry, WS 1 x 15 mm") +
  labs(x = "True ratio", y = "Measured ratio")

ggsave(filename = "figures/fig_s3a_true_vs_measured_ratio_dry_small.svg",
       plot = rap_vs_meas_dry_ws_s_ratio_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/fig_s3a_true_vs_measured_ratio_dry_small.pdf",
       plot = rap_vs_meas_dry_ws_s_ratio_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
ggsave(filename = "figures/fig_s3a_true_vs_measured_ratio_dry_small.jpg",
       plot = rap_vs_meas_dry_ws_s_ratio_plot,
       width = 6.5,
       height = 7,
       device = "jpg")

## Fig. S3b ----

ratio_val_dry_ws_l <- rap_vs_meas_dry_ws_l %>% 
  filter(element == "Ca") %>% 
  select(!element) %>% 
  rename(measured_ca = measured_conc,
         standard_ca = standard_conc)

rap_vs_meas_dry_ws_l_ratio <- rap_vs_meas_dry_ws_l %>% 
  left_join(ratio_val_dry_ws_l) %>% 
  mutate(measured_ratio = measured_conc/measured_ca,
         standard_ratio = standard_conc/standard_ca,
         measured_ratio_log = log(measured_conc/measured_ca),
         standard_ratio_log = log(standard_conc/standard_ca)) %>% 
  filter(!element == "Ca") %>% 
  mutate(ratio_id = paste(element, "/Ca", sep = ""))


rap_vs_meas_dry_ws_l_ratio_plot <- ggplot(rap_vs_meas_dry_ws_l_ratio, 
                                          aes(x = standard_ratio, y = measured_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ ratio_id, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(B) Dry, WS 10 x 15 mm") +
  labs(x = "True ratio", y = "Measured ratio")

ggsave(filename = "figures/fig_s3b_true_vs_measured_ratio_dry_large.svg",
       plot = rap_vs_meas_dry_ws_l_ratio_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/fig_s3b_true_vs_measured_ratio_dry_large.pdf",
       plot = rap_vs_meas_dry_ws_l_ratio_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
ggsave(filename = "figures/fig_s3b_true_vs_measured_ratio_dry_large.jpg",
       plot = rap_vs_meas_dry_ws_l_ratio_plot,
       width = 6.5,
       height = 7,
       device = "jpg")

## Fig. S3c ----

ratio_val_wet_ws_s <- rap_vs_meas_wet_ws_s %>% 
  filter(element == "Ca") %>% 
  select(!element) %>% 
  rename(measured_ca = measured_conc,
         standard_ca = standard_conc)

rap_vs_meas_wet_ws_s_ratio <- rap_vs_meas_wet_ws_s %>% 
  left_join(ratio_val_wet_ws_s) %>% 
  mutate(measured_ratio = measured_conc/measured_ca,
         standard_ratio = standard_conc/standard_ca,
         measured_ratio_log = log(measured_conc/measured_ca),
         standard_ratio_log = log(standard_conc/standard_ca)) %>% 
  filter(!element == "Ca") %>% 
  mutate(ratio_id = paste(element, "/Ca", sep = ""))


rap_vs_meas_wet_ws_s_ratio_plot <- ggplot(rap_vs_meas_wet_ws_s_ratio, 
                                          aes(x = standard_ratio, y = measured_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ ratio_id, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(C) Wet, WS 1 x 15 mm") +
  labs(x = "True ratio", y = "Measured ratio")

ggsave(filename = "figures/fig_s3c_true_vs_measured_ratio_wet_small.svg",
       plot = rap_vs_meas_wet_ws_s_ratio_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/fig_s3c_true_vs_measured_ratio_wet_small.pdf",
       plot = rap_vs_meas_wet_ws_s_ratio_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
ggsave(filename = "figures/fig_s3c_true_vs_measured_ratio_wet_small.jpg",
       plot = rap_vs_meas_wet_ws_s_ratio_plot,
       width = 6.5,
       height = 7,
       device = "jpg")

## Fig. S3d ----

ratio_val_wet_ws_l <- rap_vs_meas_wet_ws_l %>% 
  filter(element == "Ca") %>% 
  select(!element) %>% 
  rename(measured_ca = measured_conc,
         standard_ca = standard_conc)

rap_vs_meas_wet_ws_l_ratio <- rap_vs_meas_wet_ws_l %>% 
  left_join(ratio_val_wet_ws_l) %>% 
  mutate(measured_ratio = measured_conc/measured_ca,
         standard_ratio = standard_conc/standard_ca,
         measured_ratio_log = log(measured_conc/measured_ca),
         standard_ratio_log = log(standard_conc/standard_ca)) %>% 
  filter(!element == "Ca") %>% 
  mutate(ratio_id = paste(element, "/Ca", sep = ""))


rap_vs_meas_wet_ws_l_ratio_plot <- ggplot(rap_vs_meas_wet_ws_l_ratio, 
                                          aes(x = standard_ratio, y = measured_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ ratio_id, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(D) Wet, WS 10 x 15 mm") +
  labs(x = "True ratio", y = "Measured ratio")

ggsave(filename = "figures/fig_s3d_true_vs_measured_ratio_wet_large.svg",
       plot = rap_vs_meas_wet_ws_l_ratio_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/fig_s3d_true_vs_measured_ratio_wet_large.pdf",
       plot = rap_vs_meas_wet_ws_l_ratio_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
ggsave(filename = "figures/fig_s3d_true_vs_measured_ratio_wet_large.jpg",
       plot = rap_vs_meas_wet_ws_l_ratio_plot,
       width = 6.5,
       height = 7,
       device = "jpg")

# Fig. S4 True log ratio vs. measured log ratio ----
## Fig. S4a ----

ratio_val_dry_ws_s <- rap_vs_meas_dry_ws_s %>% 
  filter(element == "Ca") %>% 
  select(!element) %>% 
  rename(measured_ca = measured_conc,
         standard_ca = standard_conc)

rap_vs_meas_dry_ws_s_ratio <- rap_vs_meas_dry_ws_s %>% 
  left_join(ratio_val_dry_ws_s) %>% 
  mutate(measured_ratio = measured_conc/measured_ca,
         standard_ratio = standard_conc/standard_ca,
         measured_ratio_log = log(measured_conc/measured_ca),
         standard_ratio_log = log(standard_conc/standard_ca)) %>% 
  filter(!element == "Ca") %>% 
  mutate(ratio_id_log = paste("log(", element, "/Ca)", sep = ""))


rap_vs_meas_dry_ws_s_log_ratio_plot <- ggplot(rap_vs_meas_dry_ws_s_ratio, 
                                              aes(x = standard_ratio_log, y = measured_ratio_log)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ ratio_id_log, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(A) Dry, WS 1 x 15 mm") +
  labs(x = "True log ratio", y = "Measured log ratio")

ggsave(filename = "figures/fig_s4a_true_vs_measured_log_ratio_dry_small.svg",
       plot = rap_vs_meas_dry_ws_s_log_ratio_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/fig_s4a_true_vs_measured_log_ratio_dry_small.pdf",
       plot = rap_vs_meas_dry_ws_s_log_ratio_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
ggsave(filename = "figures/fig_s4a_true_vs_measured_log_ratio_dry_small.jpg",
       plot = rap_vs_meas_dry_ws_s_log_ratio_plot,
       width = 6.5,
       height = 7,
       device = "jpg")

## Fig. S4b ----

ratio_val_dry_ws_l <- rap_vs_meas_dry_ws_l %>% 
  filter(element == "Ca") %>% 
  select(!element) %>% 
  rename(measured_ca = measured_conc,
         standard_ca = standard_conc)

rap_vs_meas_dry_ws_l_ratio <- rap_vs_meas_dry_ws_l %>% 
  left_join(ratio_val_dry_ws_l) %>% 
  mutate(measured_ratio = measured_conc/measured_ca,
         standard_ratio = standard_conc/standard_ca,
         measured_ratio_log = log(measured_conc/measured_ca),
         standard_ratio_log = log(standard_conc/standard_ca)) %>% 
  filter(!element == "Ca") %>% 
  mutate(ratio_id_log = paste("log(", element, "/Ca)", sep = ""))


rap_vs_meas_dry_ws_l_log_ratio_plot <- ggplot(rap_vs_meas_dry_ws_l_ratio, 
                                              aes(x = standard_ratio_log, y = measured_ratio_log)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ ratio_id_log, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(B) Dry, WS 10 x 15 mm") +
  labs(x = "True log ratio", y = "Measured log ratio")

ggsave(filename = "figures/fig_s4b_true_vs_measured_log_ratio_dry_large.svg",
       plot = rap_vs_meas_dry_ws_l_log_ratio_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/fig_s4b_true_vs_measured_log_ratio_dry_large.pdf",
       plot = rap_vs_meas_dry_ws_l_log_ratio_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
ggsave(filename = "figures/fig_s4b_true_vs_measured_log_ratio_dry_large.jpg",
       plot = rap_vs_meas_dry_ws_l_log_ratio_plot,
       width = 6.5,
       height = 7,
       device = "jpg")

## Fig. S4c ----

ratio_val_wet_ws_s <- rap_vs_meas_wet_ws_s %>% 
  filter(element == "Ca") %>% 
  select(!element) %>% 
  rename(measured_ca = measured_conc,
         standard_ca = standard_conc)

rap_vs_meas_wet_ws_s_ratio <- rap_vs_meas_wet_ws_s %>% 
  left_join(ratio_val_wet_ws_s) %>% 
  mutate(measured_ratio = measured_conc/measured_ca,
         standard_ratio = standard_conc/standard_ca,
         measured_ratio_log = log(measured_conc/measured_ca),
         standard_ratio_log = log(standard_conc/standard_ca)) %>% 
  filter(!element == "Ca") %>% 
  mutate(ratio_id_log = paste("log(", element, "/Ca)", sep = ""))


rap_vs_meas_wet_ws_s_log_ratio_plot <- ggplot(rap_vs_meas_wet_ws_s_ratio, 
                                              aes(x = standard_ratio_log, y = measured_ratio_log)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ ratio_id_log, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(C) Wet, WS 1 x 15 mm") +
  labs(x = "True log ratio", "Measured log ratio")

ggsave(filename = "figures/fig_s4c_true_vs_measured_log_ratio_wet_small.svg",
       plot = rap_vs_meas_dry_ws_l_log_ratio_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/fig_s4c_true_vs_measured_log_ratio_wet_small.pdf",
       plot = rap_vs_meas_dry_ws_l_log_ratio_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
ggsave(filename = "figures/fig_s4c_true_vs_measured_log_ratio_wet_small.jpg",
       plot = rap_vs_meas_dry_ws_l_log_ratio_plot,
       width = 6.5,
       height = 7,
       device = "jpg")

## Fig. S4d ----

ratio_val_wet_ws_l <- rap_vs_meas_wet_ws_l %>% 
  filter(element == "Ca") %>% 
  select(!element) %>% 
  rename(measured_ca = measured_conc,
         standard_ca = standard_conc)

rap_vs_meas_wet_ws_l_ratio <- rap_vs_meas_wet_ws_l %>% 
  left_join(ratio_val_wet_ws_l) %>% 
  mutate(measured_ratio = measured_conc/measured_ca,
         standard_ratio = standard_conc/standard_ca,
         measured_ratio_log = log(measured_conc/measured_ca),
         standard_ratio_log = log(standard_conc/standard_ca)) %>% 
  filter(!element == "Ca") %>% 
  mutate(ratio_id_log = paste("log(", element, "/Ca)", sep = ""))



rap_vs_meas_wet_ws_l_log_ratio_plot <- ggplot(rap_vs_meas_wet_ws_l_ratio, 
                                              aes(x = standard_ratio_log, y = measured_ratio_log)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  facet_wrap(.~ ratio_id_log, scales = "free", ncol = 4) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("(D) Wet, WS 10 x 15 mm") +
  labs(x = "True log ratio", y = "Measured log ratio")

ggsave(filename = "figures/fig_s4d_true_vs_measured_log_ratio_wet_large.svg",
       plot = rap_vs_meas_dry_ws_l_log_ratio_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/fig_s4d_true_vs_measured_log_ratio_wet_large.pdf",
       plot = rap_vs_meas_dry_ws_l_log_ratio_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
ggsave(filename = "figures/fig_s4d_true_vs_measured_log_ratio_wet_large.jpg",
       plot = rap_vs_meas_dry_ws_l_log_ratio_plot,
       width = 6.5,
       height = 7,
       device = "jpg")

# Fig. S5 Model performance and parameters ----

rap_vs_meas_dry_mod <- 
  rap_vs_meas_dry_ws_s %>% 
  ungroup() %>% 
  add_row(rap_vs_meas_dry_ws_l) %>% 
  add_row(rap_vs_meas_wet_ws_s) %>% 
  add_row(rap_vs_meas_wet_ws_l) %>% 
  group_by(element, configuration) %>% 
  summarise(r_squared = summary(lm(measured_conc ~ standard_conc))$r.squared,
            intercept = coef(lm(measured_conc ~ standard_conc))["(Intercept)"],
            slope = coef(lm(measured_conc ~ standard_conc))["standard_conc"],
            n = n())

rap_vs_meas_dry_mod_r2_plot <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod,
           aes(x = r_squared, y = element, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  labs(title = "R squared", y = "Element", x = NULL)

rap_vs_meas_dry_mod_intercept_plot <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod,
           aes(x = intercept, y = element, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10)
  ) +
  labs(title = "Intercept", x = NULL)

rap_vs_meas_dry_mod_slope_plot <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod,
           aes(x = slope, y = element, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10)
  ) +
  labs(title = "Slope", x = NULL)

rap_vs_meas_dry_mod_n_plot <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod,
           aes(x = n, y = element, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10)
  ) +
  labs(title = "N of observations", x = NULL)

rap_vs_meas_dry_mod_plot <- grid.arrange(rap_vs_meas_dry_mod_r2_plot,
                                         rap_vs_meas_dry_mod_intercept_plot,
                                         rap_vs_meas_dry_mod_slope_plot,
                                         rap_vs_meas_dry_mod_n_plot, 
                                         ncol = 4,
                                         widths = c(1.7,1,1,1),
                                         top = "(A) Regression of measured concentration on true concentration")


rap_vs_meas_dry_mod_ratio <- 
  rap_vs_meas_dry_ws_s_ratio %>% 
  ungroup() %>% 
  add_row(rap_vs_meas_dry_ws_l_ratio) %>% 
  add_row(rap_vs_meas_wet_ws_s_ratio) %>% 
  add_row(rap_vs_meas_wet_ws_l_ratio) %>%
  mutate(ratio_id = paste(element, "/Ca", sep = "")) %>% 
  group_by(ratio_id, configuration) %>% 
  summarise(r_squared = summary(lm(measured_ratio ~ standard_ratio))$r.squared,
            intercept = coef(lm(measured_ratio ~ standard_ratio))["(Intercept)"],
            slope = coef(lm(measured_ratio ~ standard_ratio))["standard_ratio"],
            n = n())

rap_vs_meas_dry_mod_r2_plot_ratio <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod_ratio,
           aes(x = r_squared, y = ratio_id, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  labs(title = "R squared", y = "Element", x = NULL)

rap_vs_meas_dry_mod_intercept_plot_ratio <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod_ratio,
           aes(x = intercept, y = ratio_id, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10)
  ) +
  labs(title = "Intercept", x = NULL)

rap_vs_meas_dry_mod_slope_plot_ratio <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod_ratio,
           aes(x = slope, y = ratio_id, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10)
  ) +
  labs(title = "Slope", x = NULL)

rap_vs_meas_dry_mod_n_plot_ratio <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod_ratio,
           aes(x = n, y = ratio_id, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10)
  ) +
  labs(title = "N of observations", x = NULL)

rap_vs_meas_dry_mod_plot_ratio <- grid.arrange(rap_vs_meas_dry_mod_r2_plot_ratio,
                                               rap_vs_meas_dry_mod_intercept_plot_ratio,
                                               rap_vs_meas_dry_mod_slope_plot_ratio,
                                               rap_vs_meas_dry_mod_n_plot_ratio, 
                                               ncol = 4,
                                               widths = c(1.7,1,1,1),
                                               top = "(B) Regression of measured ratio on true ratio")

rap_vs_meas_dry_mod_ratio_log <- 
  rap_vs_meas_dry_ws_s_ratio %>% 
  ungroup() %>% 
  add_row(rap_vs_meas_dry_ws_l_ratio) %>% 
  add_row(rap_vs_meas_wet_ws_s_ratio) %>% 
  add_row(rap_vs_meas_wet_ws_l_ratio) %>%
  group_by(ratio_id_log, configuration) %>% 
  summarise(r_squared = summary(lm(measured_ratio_log ~ standard_ratio_log))$r.squared,
            intercept = coef(lm(measured_ratio_log ~ standard_ratio_log))["(Intercept)"],
            slope = coef(lm(measured_ratio_log ~ standard_ratio_log))["standard_ratio_log"],
            n = n())

rap_vs_meas_dry_mod_r2_plot_ratio_log <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod_ratio_log,
           aes(x = r_squared, y = ratio_id_log, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  labs(title = "R squared", y = "Element", x = NULL)

rap_vs_meas_dry_mod_intercept_plot_ratio_log <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod_ratio_log,
           aes(x = intercept, y = ratio_id_log, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10)
  ) +
  labs(title = "Intercept", x = NULL)

rap_vs_meas_dry_mod_slope_plot_ratio_log <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod_ratio_log,
           aes(x = slope, y = ratio_id_log, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10)
  ) +
  labs(title = "Slope", x = NULL)

rap_vs_meas_dry_mod_n_plot_ratio_log <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod_ratio_log,
           aes(x = n, y = ratio_id_log, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10)
  ) +
  labs(title = "N of observations", x = NULL)

rap_vs_meas_dry_mod_plot_ratio_log <- grid.arrange(rap_vs_meas_dry_mod_r2_plot_ratio_log,
                                                   rap_vs_meas_dry_mod_intercept_plot_ratio_log,
                                                   rap_vs_meas_dry_mod_slope_plot_ratio_log,
                                                   rap_vs_meas_dry_mod_n_plot_ratio_log, 
                                                   ncol = 4,
                                                   widths = c(1.7,1,1,1),
                                                   top = "(C) Regression of measured log ratio on true log ratio")

rap_vs_meas_merged <- grid.arrange(rap_vs_meas_dry_mod_plot,
                                   rap_vs_meas_dry_mod_plot_ratio,
                                   rap_vs_meas_dry_mod_plot_ratio_log,
                                   ncol = 1,
                                   heights = c(1, 1, 1))

ggsave(filename = "figures/fig_s5_model_performance_and_parameters.svg",
       plot = rap_vs_meas_merged,
       width = 6.5,
       height = 12,
       device = "svg")
ggsave(filename = "figures/fig_s5_model_performance_and_parameters.pdf",
       plot = rap_vs_meas_merged,
       width = 6.5,
       height = 12,
       device = "pdf")

legend_for_rap_vs_meas_merged <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod,
           aes(x = r_squared, y = element, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  labs(title = "R squared", y = "Element", x = NULL, fill = "Configuration")

ggsave(filename = "figures/fig_s5_legend.svg",
       plot = legend_for_rap_vs_meas_merged,
       width = 9,
       height = 4,
       device = "svg")
ggsave(filename = "figures/fig_s5_legend.pdf",
       plot = legend_for_rap_vs_meas_merged,
       width = 9,
       height = 4,
       device = "pdf")

# Fig. 3. Summary of the performance and parameters of regression models ----

raw_4boxplots <- rap_vs_meas_dry_mod %>% 
  mutate(approach = "Raw concentrations") %>% 
  rename(id = element)

ratios_4boxplots <- rap_vs_meas_dry_mod_ratio %>% 
  mutate(approach = "Element ratios") %>% 
  rename(id = ratio_id)

log_ratios_4boxplots <- rap_vs_meas_dry_mod_ratio_log %>% 
  mutate(approach = "Element log ratios") %>% 
  rename(id = ratio_id_log)

mod_boxplots <- raw_4boxplots %>% 
  ungroup() %>% 
  add_row(ratios_4boxplots) %>% 
  add_row(log_ratios_4boxplots)

mod_boxplots$approach <- factor(mod_boxplots$approach, levels = c("Raw concentrations", "Element ratios", "Element log ratios"))

mod_boxplots_r2 <- ggplot(mod_boxplots) +
  geom_boxplot(aes(y = r_squared,
                   color = configuration)) +
  geom_hline(yintercept = 1, color = "red") +
  facet_wrap(.~ approach, scales = "fixed") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "(A) R squared", y = "R squared")

mod_boxplots_r2_xAl_Ba <- mod_boxplots %>% 
  filter(!id == "Al" & !id == "Ba") %>% 
  ggplot() +
  geom_boxplot(aes(y = r_squared,
                   color = configuration)) +
  facet_wrap(.~ approach, scales = "free") +
  theme(legend.position = "bottom") # solution with removal of variables for which, in certain configurations, there are only 2 observations, resulting in R² = 1. The general trend remains the same: a larger measurement window provides a better model fit for each configuration.

mod_boxplots_intercept <- ggplot(mod_boxplots) +
  geom_boxplot(aes(y = intercept,
                   color = configuration)) +
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(.~ approach, scales = "free") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "(B) Intercept", y = "Intercept")

mod_boxplots_intercept_zoom <- ggplot(mod_boxplots) +
  geom_boxplot(aes(y = intercept,
                   color = configuration)) +
  scale_y_continuous(limits = c(-10, 10)) +
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(.~ approach, scales = "fixed") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "(B) Intercept", y = "Intercept")

mod_boxplots_slope <- ggplot(mod_boxplots) +
  geom_boxplot(aes(y = slope,
                   color = configuration)) +
  geom_hline(yintercept = 1, color = "red") +
  facet_wrap(.~ approach, scales = "fixed") +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "(C) Slope", y = "Slope", color = "Configuration")

mod_boxplots_slope_zoom <- ggplot(mod_boxplots) +
  geom_boxplot(aes(y = slope,
                   color = configuration)) +
  scale_y_continuous(limits = c(-0.1, 1.5)) +
  geom_hline(yintercept = 1, color = "red") +
  facet_wrap(.~ approach, scales = "fixed") +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "(C) Slope", y = "Slope", color = "Configuration")

mod_boxplots_all_with_outliers <- grid.arrange(mod_boxplots_r2,
                                 mod_boxplots_intercept,
                                 mod_boxplots_slope,
                                 ncol = 1,
                                 heights = c(1, 1, 1.2))

mod_boxplots_all_zoom <- grid.arrange(mod_boxplots_r2,
                                               mod_boxplots_intercept_zoom,
                                               mod_boxplots_slope_zoom,
                                               ncol = 1,
                                               heights = c(1, 1, 1.2))

ggsave(filename = "figures/fig_s6_summary_for_models_concentrations_with_outliers.svg",
       plot = mod_boxplots_all_with_outliers,
       width = 6.5,
       height = 9,
       device = "svg")
ggsave(filename = "figures/fig_s6_summary_for_models_concentrations_with_outliers.pdf",
       plot = mod_boxplots_all_with_outliers,
       width = 6.5,
       height = 9,
       device = "pdf")
ggsave(filename = "figures/fig_s6_summary_for_models_concentrations_with_outliers.jpg",
       plot = mod_boxplots_all_with_outliers,
       width = 6.5,
       height = 9,
       device = "jpg")

ggsave(filename = "figures/fig_3_summary_for_models_concentrations.svg",
       plot = mod_boxplots_all_zoom,
       width = 6.5,
       height = 9,
       device = "svg")
ggsave(filename = "figures/fig_3_summary_for_models_concentrations.pdf",
       plot = mod_boxplots_all_zoom,
       width = 6.5,
       height = 9,
       device = "pdf")
ggsave(filename = "figures/fig_3_summary_for_models_concentrations.jpg",
       plot = mod_boxplots_all_zoom,
       width = 6.5,
       height = 9,
       device = "jpg")

# Fig. S7. Percent deviations from true element concentrations ----

error_vs_range_standard <- xrf_long %>% 
  filter(treatment == "standard") %>% 
  select(standard_ID, element, concentration) %>% 
  rename(true_concentration = concentration)

error_vs_range_dry_ws_s <- xrf_long %>% 
  filter(treatment == "dry" & window_settings == "1 x 15 mm") %>% 
  filter(!is.na(concentration) & !concentration == 0) %>% 
  group_by(standard_ID, element, replication) %>% 
  summarise(med_conc = median(concentration),
            med_error = median(error)) %>% 
  ungroup() %>% 
  group_by(standard_ID, element) %>% 
  summarise(mean_med_error = mean(med_error),
            conc_range = max(med_conc) - min(med_conc),
            mean_med_conc = mean(med_conc)) %>%
  pivot_longer(mean_med_error:conc_range,
               names_to = "var",
               values_to = "value") %>% 
  mutate(perc_dev = (value/mean_med_conc)*100) %>% 
  left_join(error_vs_range_standard)

error_vs_range_dry_ws_l <- xrf_long %>% 
  filter(treatment == "dry" & window_settings == "10 x 15 mm") %>% 
  filter(!is.na(concentration) & !concentration == 0) %>% 
  group_by(standard_ID, element, replication) %>% 
  summarise(med_conc = median(concentration),
            med_error = median(error)) %>% 
  ungroup() %>% 
  group_by(standard_ID, element) %>% 
  summarise(mean_med_error = mean(med_error),
            conc_range = max(med_conc) - min(med_conc),
            mean_med_conc = mean(med_conc)) %>%
  pivot_longer(mean_med_error:conc_range,
               names_to = "var",
               values_to = "value") %>% 
  mutate(perc_dev = (value/mean_med_conc)*100) %>% 
  left_join(error_vs_range_standard)

error_vs_range_wet_ws_s <- xrf_long %>% 
  filter(treatment == "wet" & window_settings == "1 x 15 mm") %>% 
  filter(!is.na(concentration) & !concentration == 0) %>% 
  group_by(standard_ID, element, replication) %>% 
  summarise(med_conc = median(concentration),
            med_error = median(error)) %>% 
  ungroup() %>% 
  group_by(standard_ID, element) %>% 
  summarise(mean_med_error = mean(med_error),
            conc_range = max(med_conc) - min(med_conc),
            mean_med_conc = mean(med_conc)) %>%
  pivot_longer(mean_med_error:conc_range,
               names_to = "var",
               values_to = "value") %>% 
  mutate(perc_dev = (value/mean_med_conc)*100) %>%
  left_join(error_vs_range_standard)

error_vs_range_wet_ws_l <- xrf_long %>% 
  filter(treatment == "wet" & window_settings == "10 x 15 mm") %>% 
  filter(!is.na(concentration) & !concentration == 0) %>% 
  group_by(standard_ID, element, replication) %>% 
  summarise(med_conc = median(concentration),
            med_error = median(error)) %>% 
  ungroup() %>% 
  group_by(standard_ID, element) %>% 
  summarise(mean_med_error = mean(med_error),
            conc_range = max(med_conc) - min(med_conc),
            mean_med_conc = mean(med_conc)) %>%
  pivot_longer(mean_med_error:conc_range,
               names_to = "var",
               values_to = "value") %>% 
  mutate(perc_dev = (value/mean_med_conc)*100) %>%
  left_join(error_vs_range_standard)

error_vs_range_dry_ws_s_2box <- error_vs_range_dry_ws_s %>% 
  mutate(configuration = "Dry, WS 1 x 15 mm")

error_vs_range_dry_ws_l_2box <- error_vs_range_dry_ws_l %>% 
  mutate(configuration = "Dry, WS 10 x 15 mm")

error_vs_range_wet_ws_s_2box <- error_vs_range_wet_ws_s %>% 
  mutate(configuration = "Wet, WS 1 x 15 mm")

error_vs_range_wet_ws_l_2box <- error_vs_range_wet_ws_l %>% 
  mutate(configuration = "Wet, WS 10 x 15 mm")

error_vs_range_merged <- error_vs_range_dry_ws_s_2box %>% 
  ungroup() %>% 
  add_row(error_vs_range_dry_ws_l_2box) %>% 
  add_row(error_vs_range_wet_ws_s_2box) %>% 
  add_row(error_vs_range_wet_ws_l_2box) %>% 
  mutate(var = factor(var, levels = c("mean_med_error", "conc_range")))

error_vs_range_boxplot <- error_vs_range_merged %>% 
  ggplot() +
  geom_boxplot(aes(y = perc_dev, x = var, color = configuration)) +
  facet_wrap(.~element, scales = "free", ncol = 4) +
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = c(
    "conc_range" = "CME", #CME - calculated measurement error
    "mean_med_error" = "RME" #RME - reported measurement error
  )) +
  labs(x = NULL, y = "Deviation from measured concentration (%)", color = "Configuration")

error_vs_range_boxplot

ggsave(filename = "figures/fig_s7_error_vs_range_boxplot.svg",
       plot = error_vs_range_boxplot,
       width = 6.5,
       height = 8,
       device = "svg")
ggsave(filename = "figures/fig_s7_error_vs_range_boxplot.pdf",
       plot = error_vs_range_boxplot,
       width = 6.5,
       height = 8,
       device = "pdf")
ggsave(filename = "figures/fig_s7_error_vs_range_boxplot.jpg",
       plot = error_vs_range_boxplot,
       width = 6.5,
       height = 8,
       device = "jpg")

# Fig. 4. Concentration vs uncertainity ----

error_vs_value_plot <- error_vs_range_merged %>% 
  filter(var == "mean_med_error") %>% 
  ggplot() +
  geom_point(aes(x = true_concentration, y = perc_dev, color = configuration), size = 0.8) +
  facet_wrap(.~element, scales = "free") +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6)
        ) +
  labs(title = "(A) Reported measurement error",
       x = "True concentration (%)",
       y = "Deviation from measured concentration (%)")

range_vs_value_plot <- error_vs_range_merged %>% 
  filter(var == "conc_range") %>% 
  ggplot() +
  geom_point(aes(x = true_concentration, y = perc_dev, color = configuration), size = 0.8) +
  scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(.~element, scales = "free") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6)) +
  labs(title = "(B) Calculated measurement error",
       x = "True concentration (%)",
       y = "Deviation from measured concentration (%)",
       color = "Configuration")

uncertainity_vs_value_plot <- grid.arrange(error_vs_value_plot,
                                           range_vs_value_plot,
                                           ncol = 1,
                                           heights = c(1, 1.2))

ggsave(filename = "figures/fig_4_uncertainity_vs_concentration.svg",
       plot = uncertainity_vs_value_plot,
       width = 6.5,
       height = 9,
       device = "svg")
ggsave(filename = "figures/fig_4_uncertainity_vs_concentration.pdf",
       plot = uncertainity_vs_value_plot,
       width = 6.5,
       height = 9,
       device = "pdf")
ggsave(filename = "figures/fig_4_uncertainity_vs_concentration.jpg",
       plot = uncertainity_vs_value_plot,
       width = 6.5,
       height = 9,
       device = "jpg")
