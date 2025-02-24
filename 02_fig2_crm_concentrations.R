library(tidyverse)
theme_set(theme_bw())

xrf_long <- read_rds("data/xrf_long.rds")

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