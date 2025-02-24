library(tidyverse)
library(gridExtra)
theme_set(theme_bw())

# Read data ----
xrf_long <- read_rds("data/xrf_long.rds")

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

write_csv(error_vs_range_merged, "data/error_vs_range_merged.csv")

error_vs_range_boxplot <- error_vs_range_merged %>% 
  ggplot() +
  geom_boxplot(aes(y = perc_dev, x = var, color = configuration)) +
  facet_wrap(.~element, scales = "free", ncol = 4) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        plot.title = element_text(size = 10)) +
  scale_x_discrete(labels = c(
    "conc_range" = "CME", #CME - calculated measurement error
    "mean_med_error" = "RME" #RME - reported measurement error
  )) +
  labs(x = NULL, y = "Deviation from averaged measured concentration (%)", color = "Setup")

error_vs_range_boxplot

ggsave(filename = "figures/fig_4_error_vs_range_boxplot.svg",
       plot = error_vs_range_boxplot,
       width = 6.5,
       height = 8,
       device = "svg")
ggsave(filename = "figures/fig_4_error_vs_range_boxplot.pdf",
       plot = error_vs_range_boxplot,
       width = 6.5,
       height = 8,
       device = "pdf")
ggsave(filename = "figures/fig_4_error_vs_range_boxplot.jpg",
       plot = error_vs_range_boxplot,
       width = 6.5,
       height = 8,
       device = "jpg")