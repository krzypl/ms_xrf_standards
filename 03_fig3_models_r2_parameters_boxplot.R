library(tidyverse)
library(gridExtra)
theme_set(theme_bw())

# Read data ----
xrf_long <- read_rds("data/xrf_long.rds")

#Below data from script 10
rap_vs_meas_dry_mod <- read_csv("data/rap_vs_meas_dry_mod.csv")
rap_vs_meas_dry_mod_ratio <- read_csv("data/rap_vs_meas_dry_mod_ratio.csv")
rap_vs_meas_dry_mod_ratio_log <- read_csv("data/rap_vs_meas_dry_mod_ratio_log.csv")

# Fig. 3. Summary of the performance and parameters of regression models ----

raw_4boxplots <- rap_vs_meas_dry_mod %>% 
  mutate(approach = "Elemental concentrations") %>% 
  rename(id = element)

ratios_4boxplots <- rap_vs_meas_dry_mod_ratio %>% 
  mutate(approach = "Elemental ratios") %>% 
  rename(id = ratio_id)

log_ratios_4boxplots <- rap_vs_meas_dry_mod_ratio_log %>% 
  mutate(approach = "Elemental log ratios") %>% 
  rename(id = ratio_id_log)

mod_boxplots <- raw_4boxplots %>% 
  ungroup() %>% 
  add_row(ratios_4boxplots) %>% 
  add_row(log_ratios_4boxplots)

mod_boxplots$approach <- factor(mod_boxplots$approach, levels = c("Elemental concentrations", "Elemental ratios", "Elemental log ratios"))

mod_boxplots <- mod_boxplots %>% 
  mutate(intercept = abs(intercept),
         slope = abs(1 - slope)) %>% 
  filter(!n < 4)

write_rds(mod_boxplots, "data/mod_boxplots.rds")

mod_boxplots_r2 <- ggplot(mod_boxplots) +
  geom_boxplot(aes(y = r_squared,
                   color = configuration)) +
  #  geom_hline(yintercept = 1, color = "red") +
  facet_wrap(.~ approach, scales = "fixed") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = expression("(A) R"^2),
       y = expression("R"^2)) +
  theme(plot.title = element_text(size = 10))

mod_boxplots_intercept <- ggplot(mod_boxplots) +
  geom_boxplot(aes(y = intercept,
                   color = configuration)) +
  #  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(.~ approach, scales = "free") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "(B) Intercept", y = "Intercept (deviation from 0)") +
  theme(plot.title = element_text(size = 10))

mod_boxplots_intercept_zoom <- ggplot(mod_boxplots) +
  geom_boxplot(aes(y = intercept,
                   color = configuration)) +
  coord_cartesian(ylim = c(0.1, 10)) +
  #  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(.~ approach, scales = "fixed") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "(B) Intercept", y = "Intercept (absolute deviation from 0)") +
  theme(plot.title = element_text(size = 10))

mod_boxplots_slope <- ggplot(mod_boxplots) +
  geom_boxplot(aes(y = slope,
                   color = configuration)) +
  #  geom_hline(yintercept = 1, color = "red") +
  facet_wrap(.~ approach, scales = "fixed") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "(C) Slope", y = "Slope (absolute deviation from 1)", color = "Setup") +
  theme(plot.title = element_text(size = 10))

mod_boxplots_slope_zoom <- ggplot(mod_boxplots) +
  geom_boxplot(aes(y = slope,
                   color = configuration)) +
  coord_cartesian(ylim = c(-0.1, 1.5)) +
  #  geom_hline(yintercept = 1, color = "red") +
  facet_wrap(.~ approach, scales = "fixed") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "(C) Slope", y = "Slope (absolute deviation from 1)", color = "Setup") +
  theme(plot.title = element_text(size = 10))

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
