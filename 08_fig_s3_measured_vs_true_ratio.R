library(tidyverse)
theme_set(theme_bw())

# Read data ----
xrf_long <- read_rds("data/xrf_long.rds")
#Below data from script 07
rap_vs_meas_dry_ws_s <- read_csv("data/rap_vs_meas_dry_ws_s.csv")
rap_vs_meas_dry_ws_l <- read_csv("data/rap_vs_meas_dry_ws_l.csv")
rap_vs_meas_wet_ws_s <- read_csv("data/rap_vs_meas_wet_ws_s.csv")
rap_vs_meas_wet_ws_l <- read_csv("data/rap_vs_meas_wet_ws_l.csv")


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
  labs(x = "True elemental ratio", y = "Measured elemental ratio") +
  theme(plot.title = element_text(size = 10))

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
  labs(x = "True elemental ratio", y = "Measured elemental ratio") +
  theme(plot.title = element_text(size = 10))

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
  labs(x = "True elemental ratio", y = "Measured elemental ratio") +
  theme(plot.title = element_text(size = 10))

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
  labs(x = "True elemental ratio", y = "Measured elemental ratio") +
  theme(plot.title = element_text(size = 10))

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