library(tidyverse)
library(gridExtra)
theme_set(theme_bw())

#Read data from script 07
rap_vs_meas_dry_ws_l <- read_csv("data/rap_vs_meas_dry_ws_l.csv")

#Read data from script 09
rap_vs_meas_wet_ws_l_ratio <- read_csv("data/rap_vs_meas_wet_ws_l_ratio.csv")

plot_a <- rap_vs_meas_dry_ws_l %>% 
  filter(element == "K") %>% 
  ggplot(aes(x = standard_conc, y = measured_conc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  theme(legend.position = "bottom") +
  ggtitle(label = "K") +
  labs(x = "Certified concentration (ppm)", y = "Averaged measured concentration (ppm)") +
  theme(plot.title = element_text(hjust = 0.5))

plot_b <- rap_vs_meas_wet_ws_l_ratio %>% 
  filter(ratio_id_log == "log(K/Ca)") %>% 
  ggplot(aes(x = standard_ratio_log, y = measured_ratio_log)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.3) +
  theme(legend.position = "bottom") +
  ggtitle("log(K/Ca)") +
  labs(x = "Certified elemental log ratio", y = "Measured elemental log ratio") +
  theme(plot.title = element_text(hjust = 0.5))

combined_ab <- grid.arrange(plot_a, 
                            plot_b,
                            ncol = 1)

ggsave(filename = "figures/fig_plots_for_ga.svg",
       plot = combined_ab,
       width = 6.5,
       height = 3,
       device = "svg")
ggsave(filename = "figures/fig_plots_for_ga.pdf",
       plot = combined_ab,
       width = 6.5,
       height = 3,
       device = "pdf")
ggsave(filename = "figures/fig_plots_for_ga.jpg",
       plot = combined_ab,
       width = 6.5,
       height = 3,
       device = "jpg")
