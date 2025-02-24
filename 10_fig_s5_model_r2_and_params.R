library(tidyverse)
library(gridExtra)
theme_set(theme_bw())

# Read data ----
xrf_long <- read_rds("data/xrf_long.rds")
#Below data from script 07
rap_vs_meas_dry_ws_s <- read_csv("data/rap_vs_meas_dry_ws_s.csv")
rap_vs_meas_dry_ws_l <- read_csv("data/rap_vs_meas_dry_ws_l.csv")
rap_vs_meas_wet_ws_s <- read_csv("data/rap_vs_meas_wet_ws_s.csv")
rap_vs_meas_wet_ws_l <- read_csv("data/rap_vs_meas_wet_ws_l.csv")
#Below data from script 09
rap_vs_meas_dry_ws_s_ratio <- read_csv("data/rap_vs_meas_dry_ws_s_ratio.csv")
rap_vs_meas_dry_ws_l_ratio <- read_csv("data/rap_vs_meas_dry_ws_l_ratio.csv")
rap_vs_meas_wet_ws_s_ratio <- read_csv("data/rap_vs_meas_wet_ws_s_ratio.csv")
rap_vs_meas_wet_ws_l_ratio <- read_csv("data/rap_vs_meas_wet_ws_l_ratio.csv")

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

write_csv(rap_vs_meas_dry_mod, "data/rap_vs_meas_dry_mod.csv")

rap_vs_meas_dry_mod_r2_plot <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod,
           aes(x = r_squared, y = element, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  labs(title = expression("R"^2),
       y = "Element",
       x = NULL)

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
                                         top = "(A) Averaged measured concentration vs. true concentration")


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

write_csv(rap_vs_meas_dry_mod_ratio, "data/rap_vs_meas_dry_mod_ratio.csv")

rap_vs_meas_dry_mod_r2_plot_ratio <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod_ratio,
           aes(x = r_squared, y = ratio_id, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  labs(title = expression("R"^2),
       y = "Elemental ratio",
       x = NULL)

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
                                               top = "(B) Measured elemental ratio vs. true elemental ratio")

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

write_csv(rap_vs_meas_dry_mod_ratio_log, "data/rap_vs_meas_dry_mod_ratio_log.csv")

rap_vs_meas_dry_mod_r2_plot_ratio_log <- ggplot() +
  geom_col(data = rap_vs_meas_dry_mod_ratio_log,
           aes(x = r_squared, y = ratio_id_log, fill = configuration),
           position = position_dodge()) +
  theme(legend.position = "none",
        plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  labs(title = expression("R"^2),
       y = "Elemental log ratio",
       x = NULL)

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
                                                   top = "(C) Measured elemental log ratio vs. true elemental log ratio")

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
  labs(title = "R squared", y = "Element", x = NULL, fill = "Setup")

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