library(tidyverse)
library(gridExtra)
theme_set(theme_bw())

# Read data -----
error_vs_range_merged <- read_csv("data/error_vs_range_merged.csv")

# Figure 5 -----

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
  labs(title = "(A) Reported measurement error (RME)",
       x = "True concentration (ppm)",
       y = "Deviation from averaged measured concentration (%)")

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
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  labs(title = "(B) Calculated measurement error (CME)",
       x = "True concentration (ppm)",
       y = "Deviation from averaged measured concentration (%)",
       color = "Setup")

uncertainity_vs_value_plot <- grid.arrange(error_vs_value_plot,
                                           range_vs_value_plot,
                                           ncol = 1,
                                           heights = c(1, 1.2))

ggsave(filename = "figures/fig_5_uncertainity_vs_concentration.svg",
       plot = uncertainity_vs_value_plot,
       width = 6.5,
       height = 9,
       device = "svg")
ggsave(filename = "figures/fig_5_uncertainity_vs_concentration.pdf",
       plot = uncertainity_vs_value_plot,
       width = 6.5,
       height = 9,
       device = "pdf")
ggsave(filename = "figures/fig_5_uncertainity_vs_concentration.jpg",
       plot = uncertainity_vs_value_plot,
       width = 6.5,
       height = 9,
       device = "jpg")