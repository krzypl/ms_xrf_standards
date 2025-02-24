library(tidyverse)
library(gridExtra)
library(coin)
library(kableExtra)
library(webshot)
theme_set(theme_bw())

# Read data ----

#Below data from script 03
mod_boxplots <- read_rds("data/mod_boxplots.rds")

mod_boxplots$configuration <- factor(mod_boxplots$configuration)

# Statistical tests for model performances ----

# We use the Kruskal-Wallis test with permutations, as it is suitable for non-normally distributed data and does not assume equal variances across groups

## R squared ----
### differences between various data representations ----
rsq_test_data_representation_dry_s_ws_prep <- mod_boxplots %>% 
  filter(configuration == "Dry, WS 1 x 15 mm")

set.seed(12)
rsq_test_data_representation_dry_s_ws_fk <- fligner_test(r_squared~approach,
                                                         data = rsq_test_data_representation_dry_s_ws_prep,
                                                         distribution = approximate(nresample = 9999))

xtract_p <- function(x) {
  p <- x@distribution@pvalue(x@statistic@teststatistic)[1]
}

rsq_test_data_representation_dry_s_ws_fk_pval <- xtract_p(rsq_test_data_representation_dry_s_ws_fk)

set.seed(12)
rsq_test_data_representation_dry_s_ws <- kruskal_test(r_squared~approach,
                                                      data = rsq_test_data_representation_dry_s_ws_prep,
                                                      distribution = approximate(nresample = 9999))

rsq_test_data_representation_dry_s_ws_pval <- xtract_p(rsq_test_data_representation_dry_s_ws)


rsq_test_data_representation_dry_l_ws_prep <- mod_boxplots %>% 
  filter(configuration == "Dry, WS 10 x 15 mm")

set.seed(12)
rsq_test_data_representation_dry_l_ws_fk <- fligner_test(r_squared~approach,
                                                         data = rsq_test_data_representation_dry_l_ws_prep,
                                                         distribution = approximate(nresample = 9999))

rsq_test_data_representation_dry_l_ws_fk_pval <- xtract_p(rsq_test_data_representation_dry_l_ws_fk)

set.seed(12)
rsq_test_data_representation_dry_l_ws <- kruskal_test(r_squared~approach,
                                                      data = rsq_test_data_representation_dry_l_ws_prep,
                                                      distribution = approximate(nresample = 9999))

rsq_test_data_representation_dry_l_ws_pval <- xtract_p(rsq_test_data_representation_dry_l_ws)


rsq_test_data_representation_wet_s_ws_prep <- mod_boxplots %>% 
  filter(configuration == "Wet, WS 1 x 15 mm")

set.seed(12)
rsq_test_data_representation_wet_s_ws_fk <- fligner_test(r_squared~approach,
                                                         data = rsq_test_data_representation_wet_s_ws_prep,
                                                         distribution = approximate(nresample = 9999))

rsq_test_data_representation_wet_s_ws_fk_pval <- xtract_p(rsq_test_data_representation_wet_s_ws_fk)

set.seed(12)
rsq_test_data_representation_wet_s_ws <- kruskal_test(r_squared~approach,
                                                      data = rsq_test_data_representation_wet_s_ws_prep,
                                                      distribution = approximate(nresample = 9999))

rsq_test_data_representation_wet_s_ws_pval <- xtract_p(rsq_test_data_representation_wet_s_ws)

rsq_test_data_representation_wet_l_ws_prep <- mod_boxplots %>% 
  filter(configuration == "Wet, WS 10 x 15 mm")

set.seed(12)
rsq_test_data_representation_wet_l_ws_fk <- fligner_test(r_squared~approach,
                                                         data = rsq_test_data_representation_wet_l_ws_prep,
                                                         distribution = approximate(nresample = 9999))

rsq_test_data_representation_wet_l_ws_fk_pval <- xtract_p(rsq_test_data_representation_wet_l_ws_fk)

set.seed(12)
rsq_test_data_representation_wet_l_ws <- kruskal_test(r_squared~approach,
                                                      data = rsq_test_data_representation_wet_l_ws_prep,
                                                      distribution = approximate(nresample = 9999))

rsq_test_data_representation_wet_l_ws_pval <- xtract_p(rsq_test_data_representation_wet_l_ws)

rsq_rep_val <- c(
  rsq_test_data_representation_dry_s_ws_fk_pval,
  rsq_test_data_representation_dry_s_ws_pval,
  rsq_test_data_representation_dry_l_ws_fk_pval,
  rsq_test_data_representation_dry_l_ws_pval, 
  rsq_test_data_representation_wet_s_ws_fk_pval,
  rsq_test_data_representation_wet_s_ws_pval,
  rsq_test_data_representation_wet_l_ws_fk_pval,
  rsq_test_data_representation_wet_l_ws_pval
)

rsq_rep_names <- c(
  "rsq_test_data_representation_dry_s_ws_fk_pval",
  "rsq_test_data_representation_dry_s_ws_pval",
  "rsq_test_data_representation_dry_l_ws_fk_pval",
  "rsq_test_data_representation_dry_l_ws_pval",
  "rsq_test_data_representation_wet_s_ws_fk_pval",
  "rsq_test_data_representation_wet_s_ws_pval",
  "rsq_test_data_representation_wet_l_ws_fk_pval",
  "rsq_test_data_representation_wet_l_ws_pval"
)

rsq_rep <- tibble(id = rsq_rep_names, p_value = rsq_rep_val)

### differences between wetness ----

mod_boxplots_xconfs <- mod_boxplots %>% 
  separate(configuration, into = c("wetness", "ws"), sep = ",") %>% 
  mutate(ws = gsub(" WS", "WS", ws),
         ws = factor(ws),
         wetness = factor(wetness))

rsq_raw_wetness_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental concentrations")

set.seed(12)
rsq_raw_wetness_test_fk <- fligner_test(r_squared~wetness,
                                        data = rsq_raw_wetness_test_prep,
                                        distribution = approximate(nresample = 9999))

rsq_raw_wetness_test_fk_pval <- xtract_p(rsq_raw_wetness_test_fk)

set.seed(12)
rsq_raw_wetness_test <- kruskal_test(r_squared~wetness,
                                     data = rsq_raw_wetness_test_prep,
                                     distribution = approximate(nresample = 9999))

rsq_raw_wetness_test_pval <- xtract_p(rsq_raw_wetness_test)


rsq_ratio_wetness_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental ratios")

rsq_ratio_wetness_test_fk <- fligner_test(r_squared~wetness,
                                          data = rsq_ratio_wetness_test_prep,
                                          distribution = approximate(nresample = 9999))

rsq_ratio_wetness_test_fk_pval <- xtract_p(rsq_ratio_wetness_test_fk)

set.seed(12)
rsq_ratio_wetness_test <- kruskal_test(r_squared~wetness,
                                       data = rsq_ratio_wetness_test_prep,
                                       distribution = approximate(nresample = 9999))

rsq_ratio_wetness_test_pval <- xtract_p(rsq_ratio_wetness_test)

rsq_log_ratio_wetness_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental log ratios")

set.seed(12)
rsq_log_ratio_wetness_test_fk <- fligner_test(r_squared~wetness,
                                              data = rsq_log_ratio_wetness_test_prep,
                                              distribution = approximate(nresample = 9999))

rsq_log_ratio_wetness_test_fk_pval <- xtract_p(rsq_log_ratio_wetness_test_fk)

set.seed(12)
rsq_log_ratio_wetness_test <- kruskal_test(r_squared~wetness,
                                           data = rsq_log_ratio_wetness_test_prep,
                                           distribution = approximate(nresample = 9999))

rsq_log_ratio_wetness_test_pval <- xtract_p(rsq_log_ratio_wetness_test)

rsq_wetness_val <- c(
  rsq_raw_wetness_test_fk_pval,
  rsq_raw_wetness_test_pval,
  rsq_ratio_wetness_test_fk_pval,
  rsq_ratio_wetness_test_pval,
  rsq_log_ratio_wetness_test_fk_pval,
  rsq_log_ratio_wetness_test_pval
)

rsq_wetness_names <- c(
  "rsq_raw_wetness_test_fk_pval",
  "rsq_raw_wetness_test_pval",
  "rsq_ratio_wetness_test_fk_pval",
  "rsq_ratio_wetness_test_pval",
  "rsq_log_ratio_wetness_test_fk_pval",
  "rsq_log_ratio_wetness_test_pval"
)

rsq_wetness <- tibble(id = rsq_wetness_names, p_value = rsq_wetness_val)

### differences between ws ----

rsq_raw_ws_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental concentrations")

set.seed(12)
rsq_raw_ws_test_fk <- fligner_test(r_squared~ws,
                                   data = rsq_raw_ws_test_prep,
                                   distribution = approximate(nresample = 9999))

rsq_raw_ws_test_fk_pval <- xtract_p(rsq_raw_ws_test_fk)

set.seed(12)
rsq_raw_ws_test <- kruskal_test(r_squared~ws,
                                data = rsq_raw_ws_test_prep,
                                distribution = approximate(nresample = 9999))

rsq_raw_ws_test_pval <- xtract_p(rsq_raw_ws_test)

rsq_ratio_ws_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental ratios")

set.seed(12)
rsq_ratio_ws_test_fk <- fligner_test(r_squared~ws,
                                     data = rsq_ratio_ws_test_prep,
                                     distribution = approximate(nresample = 9999))

rsq_ratio_ws_test_fk_pval <- xtract_p(rsq_ratio_ws_test_fk)

set.seed(12)
rsq_ratio_ws_test <- kruskal_test(r_squared~ws,
                                  data = rsq_ratio_ws_test_prep,
                                  distribution = approximate(nresample = 9999))

rsq_ratio_ws_test_pval <- xtract_p(rsq_ratio_ws_test)

rsq_log_ratio_ws_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental log ratios")

set.seed(12)
rsq_log_ratio_ws_test_fk <- fligner_test(r_squared~ws,
                                         data = rsq_log_ratio_ws_test_prep,
                                         distribution = approximate(nresample = 9999))

rsq_log_ratio_ws_test_fk_pval <- xtract_p(rsq_log_ratio_ws_test_fk)

set.seed(12)
rsq_log_ratio_ws_test <- kruskal_test(r_squared~ws,
                                      data = rsq_log_ratio_ws_test_prep,
                                      distribution = approximate(nresample = 9999))

rsq_log_ratio_ws_test_pval <- xtract_p(rsq_log_ratio_ws_test)

rsq_ws_val <- c(
  rsq_raw_ws_test_fk_pval,
  rsq_raw_ws_test_pval,
  rsq_ratio_ws_test_fk_pval,
  rsq_ratio_ws_test_pval,
  rsq_log_ratio_ws_test_fk_pval,
  rsq_log_ratio_ws_test_pval
)

rsq_ws_names <- c(
  "rsq_raw_ws_test_fk_pval",
  "rsq_raw_ws_test_pval",
  "rsq_ratio_ws_test_fk_pval",
  "rsq_ratio_ws_test_pval",
  "rsq_log_ratio_ws_test_fk_pval",
  "rsq_log_ratio_ws_test_pval"
)

rsq_ws <- tibble(id = rsq_ws_names, p_value = rsq_ws_val)


## intercept ----
### differences between various data representations ----
intc_test_data_representation_dry_s_ws_prep <- mod_boxplots %>% 
  filter(configuration == "Dry, WS 1 x 15 mm")

set.seed(12)
intc_test_data_representation_dry_s_ws_fk <- fligner_test(intercept~approach,
                                                          data = intc_test_data_representation_dry_s_ws_prep,
                                                          distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_s_ws_fk_pval <- xtract_p(intc_test_data_representation_dry_s_ws_fk)

intc_test_data_representation_dry_s_ws_raw_and_ratio <- intc_test_data_representation_dry_s_ws_prep %>% 
  filter(!approach == "Elemental log ratios")

set.seed(12)
intc_test_data_representation_dry_s_ws_raw_and_ratio_fk <- fligner_test(intercept~approach,
                                                                        data = intc_test_data_representation_dry_s_ws_raw_and_ratio,
                                                                        distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_s_ws_raw_and_ratio_fk_pval <- 
  xtract_p(intc_test_data_representation_dry_s_ws_raw_and_ratio_fk)

intc_test_data_representation_dry_s_ws_raw_and_log_ratio <- intc_test_data_representation_dry_s_ws_prep %>% 
  filter(!approach == "Elemental ratios")

set.seed(12)
intc_test_data_representation_dry_s_ws_raw_and_log_ratio_fk <- 
  fligner_test(intercept~approach,
               data = intc_test_data_representation_dry_s_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_s_ws_raw_and_log_ratio_fk_pval <- 
  xtract_p(intc_test_data_representation_dry_s_ws_raw_and_log_ratio_fk)

intc_test_data_representation_dry_s_ws_ratio_and_log_ratio <- intc_test_data_representation_dry_s_ws_prep %>% 
  filter(!approach == "Elemental concentrations")

set.seed(12)
intc_test_data_representation_dry_s_ws_ratio_and_log_ratio_fk <- 
  fligner_test(intercept~approach,
               data = intc_test_data_representation_dry_s_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_s_ws_ratio_and_log_ratio_fk_pval <- 
  xtract_p(intc_test_data_representation_dry_s_ws_ratio_and_log_ratio_fk)


set.seed(12)
intc_test_data_representation_dry_s_ws <- kruskal_test(intercept~approach,
                                                       data = intc_test_data_representation_dry_s_ws_prep,
                                                       distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_s_ws_pval <- 
  xtract_p(intc_test_data_representation_dry_s_ws)

set.seed(12)
intc_test_data_representation_dry_s_ws_raw_and_ratio_ks <- 
  kruskal_test(intercept~approach,
               data = intc_test_data_representation_dry_s_ws_raw_and_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_s_ws_raw_and_ratio_ks_pval <- 
  xtract_p(intc_test_data_representation_dry_s_ws_raw_and_ratio_ks)


set.seed(12)
intc_test_data_representation_dry_s_ws_raw_and_log_ratio_ks <- 
  kruskal_test(intercept~approach,
               data = intc_test_data_representation_dry_s_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_s_ws_raw_and_log_ratio_ks_pval <- 
  xtract_p(intc_test_data_representation_dry_s_ws_raw_and_log_ratio_ks)

set.seed(12)
intc_test_data_representation_dry_s_ws_ratio_and_log_ratio_ks <- 
  kruskal_test(intercept~approach,
               data = intc_test_data_representation_dry_s_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_s_ws_ratio_and_log_ratio_ks_pval <- 
  xtract_p(intc_test_data_representation_dry_s_ws_ratio_and_log_ratio_ks)


intc_test_data_representation_dry_l_ws_prep <- mod_boxplots %>% 
  filter(configuration == "Dry, WS 10 x 15 mm")

set.seed(12)
intc_test_data_representation_dry_l_ws_fk <- fligner_test(intercept~approach,
                                                          data = intc_test_data_representation_dry_l_ws_prep,
                                                          distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_l_ws_fk_pval <- 
  xtract_p(intc_test_data_representation_dry_l_ws_fk)

intc_test_data_representation_dry_l_ws_raw_and_ratio <- intc_test_data_representation_dry_l_ws_prep %>% 
  filter(!approach == "Elemental log ratios")

set.seed(12)
intc_test_data_representation_dry_l_ws_raw_and_ratio_fk <- fligner_test(intercept~approach,
                                                                        data = intc_test_data_representation_dry_l_ws_raw_and_ratio,
                                                                        distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_l_ws_raw_and_ratio_fk_pval <- 
  xtract_p(intc_test_data_representation_dry_l_ws_raw_and_ratio_fk)

set.seed(12)
intc_test_data_representation_dry_l_ws_raw_and_ratio_ks <- 
  kruskal_test(intercept~approach,
               data = intc_test_data_representation_dry_l_ws_raw_and_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_l_ws_raw_and_ratio_ks_pval <- 
  xtract_p(intc_test_data_representation_dry_l_ws_raw_and_ratio_ks)

intc_test_data_representation_dry_l_ws_raw_and_log_ratio <- intc_test_data_representation_dry_l_ws_prep %>% 
  filter(!approach == "Elemental ratios")

set.seed(12)
intc_test_data_representation_dry_l_ws_raw_and_log_ratio_fk <- 
  fligner_test(intercept~approach,
               data = intc_test_data_representation_dry_l_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_l_ws_raw_and_log_ratio_fk_pval <- 
  xtract_p(intc_test_data_representation_dry_l_ws_raw_and_log_ratio_fk)

set.seed(12)
intc_test_data_representation_dry_l_ws_raw_and_log_ratio_ks <- 
  kruskal_test(intercept~approach,
               data = intc_test_data_representation_dry_l_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_l_ws_raw_and_log_ratio_ks_pval <- 
  xtract_p(intc_test_data_representation_dry_l_ws_raw_and_log_ratio_ks)

intc_test_data_representation_dry_l_ws_ratio_and_log_ratio <- intc_test_data_representation_dry_l_ws_prep %>% 
  filter(!approach == "Elemental concentrations")

set.seed(12)
intc_test_data_representation_dry_l_ws_ratio_and_log_ratio_fk <- 
  fligner_test(intercept~approach,
               data = intc_test_data_representation_dry_l_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_l_ws_ratio_and_log_ratio_fk_pval <- 
  xtract_p(intc_test_data_representation_dry_l_ws_ratio_and_log_ratio_fk)

set.seed(12)
intc_test_data_representation_dry_l_ws_ratio_and_log_ratio_ks <- 
  kruskal_test(intercept~approach,
               data = intc_test_data_representation_dry_l_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_l_ws_ratio_and_log_ratio_ks_pval <- 
  xtract_p(intc_test_data_representation_dry_l_ws_ratio_and_log_ratio_ks)

set.seed(12)
intc_test_data_representation_dry_l_ws <- kruskal_test(intercept~approach,
                                                       data = intc_test_data_representation_dry_l_ws_prep,
                                                       distribution = approximate(nresample = 9999))

intc_test_data_representation_dry_l_ws_pval <- 
  xtract_p(intc_test_data_representation_dry_l_ws)

intc_test_data_representation_wet_s_ws_prep <- mod_boxplots %>% 
  filter(configuration == "Wet, WS 1 x 15 mm")

set.seed(12)
intc_test_data_representation_wet_s_ws_fk <- fligner_test(intercept~approach,
                                                          data = intc_test_data_representation_wet_s_ws_prep,
                                                          distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_s_ws_fk_pval <- 
  xtract_p(intc_test_data_representation_wet_s_ws_fk)

intc_test_data_representation_wet_s_ws_raw_and_ratio <- intc_test_data_representation_wet_s_ws_prep %>% 
  filter(!approach == "Elemental log ratios")

set.seed(12)
intc_test_data_representation_wet_s_ws_raw_and_ratio_fk <- 
  fligner_test(intercept~approach,
               data = intc_test_data_representation_wet_s_ws_raw_and_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_s_ws_raw_and_ratio_fk_pval <- 
  xtract_p(intc_test_data_representation_wet_s_ws_raw_and_ratio_fk)

intc_test_data_representation_wet_s_ws_raw_and_log_ratio <- intc_test_data_representation_wet_s_ws_prep %>% 
  filter(!approach == "Elemental ratios")

set.seed(12)
intc_test_data_representation_wet_s_ws_raw_and_log_ratio_fk <- 
  fligner_test(intercept~approach,
               data = intc_test_data_representation_wet_s_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_s_ws_raw_and_log_ratio_fk_pval <- 
  xtract_p(intc_test_data_representation_wet_s_ws_raw_and_log_ratio_fk)

intc_test_data_representation_wet_s_ws_ratio_and_log_ratio <- intc_test_data_representation_wet_s_ws_prep %>% 
  filter(!approach == "Elemental concentrations")

set.seed(12)
intc_test_data_representation_wet_s_ws_ratio_and_log_ratio_fk <- 
  fligner_test(intercept~approach,
               data = intc_test_data_representation_wet_s_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_s_ws_ratio_and_log_ratio_fk_pval <- 
  xtract_p(intc_test_data_representation_wet_s_ws_ratio_and_log_ratio_fk)

set.seed(12)
intc_test_data_representation_wet_s_ws <- kruskal_test(intercept~approach,
                                                       data = intc_test_data_representation_wet_s_ws_prep,
                                                       distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_s_ws_pval <- 
  xtract_p(intc_test_data_representation_wet_s_ws)

set.seed(12)
intc_test_data_representation_wet_s_ws_raw_and_ratio_kr <- kruskal_test(intercept~approach,
                                                                        data = intc_test_data_representation_wet_s_ws_raw_and_ratio,
                                                                        distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_s_ws_raw_and_ratio_kr_pval <- 
  xtract_p(intc_test_data_representation_wet_s_ws_raw_and_ratio_kr)

set.seed(12)
intc_test_data_representation_wet_s_ws_raw_and_log_ratio_kr <- 
  kruskal_test(intercept~approach,
               data = intc_test_data_representation_wet_s_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_s_ws_raw_and_log_ratio_kr_pval <- 
  xtract_p(intc_test_data_representation_wet_s_ws_raw_and_log_ratio_kr)

set.seed(12)
intc_test_data_representation_wet_s_ws_ratio_and_log_ratio_kr <- 
  kruskal_test(intercept~approach,
               data = intc_test_data_representation_wet_s_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_s_ws_ratio_and_log_ratio_kr_pval <- 
  xtract_p(intc_test_data_representation_wet_s_ws_ratio_and_log_ratio_kr)

intc_test_data_representation_wet_l_ws_prep <- mod_boxplots %>% 
  filter(configuration == "Wet, WS 10 x 15 mm")

set.seed(12)
intc_test_data_representation_wet_l_ws_fk <- fligner_test(intercept~approach,
                                                          data = intc_test_data_representation_wet_l_ws_prep,
                                                          distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_l_ws_fk_pval <- 
  xtract_p(intc_test_data_representation_wet_l_ws_fk)

intc_test_data_representation_wet_l_ws_raw_and_ratio <- intc_test_data_representation_wet_l_ws_prep %>% 
  filter(!approach == "Elemental log ratios")

set.seed(12)
intc_test_data_representation_wet_l_ws_raw_and_ratio_fk <- 
  fligner_test(intercept~approach,
               data = intc_test_data_representation_wet_l_ws_raw_and_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_l_ws_raw_and_ratio_fk_pval <- 
  xtract_p(intc_test_data_representation_wet_l_ws_raw_and_ratio_fk)

intc_test_data_representation_wet_l_ws_raw_and_log_ratio <- intc_test_data_representation_wet_l_ws_prep %>% 
  filter(!approach == "Elemental ratios")

set.seed(12)
intc_test_data_representation_wet_l_ws_raw_and_log_ratio_fk <- 
  fligner_test(intercept~approach,
               data = intc_test_data_representation_wet_l_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_l_ws_raw_and_log_ratio_fk_pval <- 
  xtract_p(intc_test_data_representation_wet_l_ws_raw_and_log_ratio_fk)

intc_test_data_representation_wet_l_ws_ratio_and_log_ratio <- intc_test_data_representation_wet_l_ws_prep %>% 
  filter(!approach == "Elemental concentrations")

set.seed(12)
intc_test_data_representation_wet_l_ws_ratio_and_log_ratio_fk <- 
  fligner_test(intercept~approach,
               data = intc_test_data_representation_wet_l_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_l_ws_ratio_and_log_ratio_fk_pval <- 
  xtract_p(intc_test_data_representation_wet_l_ws_ratio_and_log_ratio_fk)

set.seed(12)
intc_test_data_representation_wet_l_ws <- kruskal_test(intercept~approach,
                                                       data = intc_test_data_representation_wet_l_ws_prep,
                                                       distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_l_ws_pval <- 
  xtract_p(intc_test_data_representation_wet_l_ws)

set.seed(12)
intc_test_data_representation_wet_l_ws_raw_and_ratio_kr <- kruskal_test(intercept~approach,
                                                                        data = intc_test_data_representation_wet_l_ws_raw_and_ratio,
                                                                        distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_l_ws_raw_and_ratio_kr_pval <- 
  xtract_p(intc_test_data_representation_wet_l_ws_raw_and_ratio_kr)

set.seed(12)
intc_test_data_representation_wet_l_ws_raw_and_log_ratio_kr <- 
  kruskal_test(intercept~approach,
               data = intc_test_data_representation_wet_l_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_l_ws_raw_and_log_ratio_kr_pval <- 
  xtract_p(intc_test_data_representation_wet_l_ws_raw_and_log_ratio_kr)

set.seed(12)
intc_test_data_representation_wet_l_ws_ratio_and_log_ratio_kr <- 
  kruskal_test(intercept~approach,
               data = intc_test_data_representation_wet_l_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

intc_test_data_representation_wet_l_ws_ratio_and_log_ratio_kr_pval <- 
  xtract_p(intc_test_data_representation_wet_l_ws_ratio_and_log_ratio_kr)

intc_rep_val <- c(
  intc_test_data_representation_dry_s_ws_fk_pval,
  intc_test_data_representation_dry_s_ws_pval,
  intc_test_data_representation_dry_l_ws_fk_pval,
  intc_test_data_representation_dry_l_ws_pval,
  intc_test_data_representation_wet_s_ws_fk_pval,
  intc_test_data_representation_wet_s_ws_pval,
  intc_test_data_representation_wet_l_ws_fk_pval,
  intc_test_data_representation_wet_l_ws_pval
)

intc_rep_names <- c(
  "intc_test_data_representation_dry_s_ws_fk_pval",
  "intc_test_data_representation_dry_s_ws_pval",
  "intc_test_data_representation_dry_l_ws_fk_pval",
  "intc_test_data_representation_dry_l_ws_pval",
  "intc_test_data_representation_wet_s_ws_fk_pval",
  "intc_test_data_representation_wet_s_ws_pval",
  "intc_test_data_representation_wet_l_ws_fk_pval",
  "intc_test_data_representation_wet_l_ws_pval"
)

intc_rep <- tibble(id = intc_rep_names, p_value = intc_rep_val)

intc_rep_val_pairwise <- c(
  intc_test_data_representation_dry_s_ws_raw_and_ratio_fk_pval,
  intc_test_data_representation_dry_s_ws_raw_and_log_ratio_fk_pval,
  intc_test_data_representation_dry_s_ws_ratio_and_log_ratio_fk_pval,
  intc_test_data_representation_dry_s_ws_raw_and_ratio_ks_pval,
  intc_test_data_representation_dry_s_ws_raw_and_log_ratio_ks_pval,
  intc_test_data_representation_dry_s_ws_ratio_and_log_ratio_ks_pval,
  intc_test_data_representation_dry_l_ws_raw_and_ratio_fk_pval,
  intc_test_data_representation_dry_l_ws_raw_and_log_ratio_fk_pval,
  intc_test_data_representation_dry_l_ws_ratio_and_log_ratio_fk_pval,
  intc_test_data_representation_dry_l_ws_raw_and_ratio_ks_pval,
  intc_test_data_representation_dry_l_ws_raw_and_log_ratio_ks_pval,
  intc_test_data_representation_dry_l_ws_ratio_and_log_ratio_ks_pval,
  intc_test_data_representation_wet_s_ws_raw_and_ratio_fk_pval,
  intc_test_data_representation_wet_s_ws_raw_and_log_ratio_fk_pval,
  intc_test_data_representation_wet_s_ws_ratio_and_log_ratio_fk_pval,
  intc_test_data_representation_wet_s_ws_raw_and_ratio_kr_pval,
  intc_test_data_representation_wet_s_ws_raw_and_log_ratio_kr_pval,
  intc_test_data_representation_wet_s_ws_ratio_and_log_ratio_kr_pval,
  intc_test_data_representation_wet_l_ws_raw_and_ratio_fk_pval,
  intc_test_data_representation_wet_l_ws_raw_and_log_ratio_fk_pval,
  intc_test_data_representation_wet_l_ws_ratio_and_log_ratio_fk_pval,
  intc_test_data_representation_wet_l_ws_raw_and_ratio_kr_pval,
  intc_test_data_representation_wet_l_ws_raw_and_log_ratio_kr_pval,
  intc_test_data_representation_wet_l_ws_ratio_and_log_ratio_kr_pval
)

intc_rep_names_pairwise <- c(
  "intc_test_data_representation_dry_s_ws_raw_and_ratio_fk_pval",
  "intc_test_data_representation_dry_s_ws_raw_and_log_ratio_fk_pval",
  "intc_test_data_representation_dry_s_ws_ratio_and_log_ratio_fk_pval",
  "intc_test_data_representation_dry_s_ws_raw_and_ratio_ks_pval",
  "intc_test_data_representation_dry_s_ws_raw_and_log_ratio_ks_pval",
  "intc_test_data_representation_dry_s_ws_ratio_and_log_ratio_ks_pval",
  "intc_test_data_representation_dry_l_ws_raw_and_ratio_fk_pval",
  "intc_test_data_representation_dry_l_ws_raw_and_log_ratio_fk_pval",
  "intc_test_data_representation_dry_l_ws_ratio_and_log_ratio_fk_pval",
  "intc_test_data_representation_dry_l_ws_raw_and_ratio_ks_pval",
  "intc_test_data_representation_dry_l_ws_raw_and_log_ratio_ks_pval",
  "intc_test_data_representation_dry_l_ws_ratio_and_log_ratio_ks_pval",
  "intc_test_data_representation_wet_s_ws_raw_and_ratio_fk_pval",
  "intc_test_data_representation_wet_s_ws_raw_and_log_ratio_fk_pval",
  "intc_test_data_representation_wet_s_ws_ratio_and_log_ratio_fk_pval",
  "intc_test_data_representation_wet_s_ws_raw_and_ratio_kr_pval",
  "intc_test_data_representation_wet_s_ws_raw_and_log_ratio_kr_pval",
  "intc_test_data_representation_wet_s_ws_ratio_and_log_ratio_kr_pval",
  "intc_test_data_representation_wet_l_ws_raw_and_ratio_fk_pval",
  "intc_test_data_representation_wet_l_ws_raw_and_log_ratio_fk_pval",
  "intc_test_data_representation_wet_l_ws_ratio_and_log_ratio_fk_pval",
  "intc_test_data_representation_wet_l_ws_raw_and_ratio_kr_pval",
  "intc_test_data_representation_wet_l_ws_raw_and_log_ratio_kr_pval",
  "intc_test_data_representation_wet_l_ws_ratio_and_log_ratio_kr_pval"
)

intc_rep_pairwise <- tibble(id = intc_rep_names_pairwise, p_vaue = intc_rep_val_pairwise)

### differences between wetness ----

intc_raw_wetness_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental concentrations")

set.seed(12)
intc_raw_wetness_test_fk <- fligner_test(intercept~wetness,
                                         data = intc_raw_wetness_test_prep,
                                         distribution = approximate(nresample = 9999))

intc_raw_wetness_test_fk_pval <- 
  xtract_p(intc_raw_wetness_test_fk)

set.seed(12)
intc_raw_wetness_test <- kruskal_test(intercept~wetness,
                                      data = intc_raw_wetness_test_prep,
                                      distribution = approximate(nresample = 9999))

intc_raw_wetness_test_pval <- 
  xtract_p(intc_raw_wetness_test)


intc_ratio_wetness_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental ratios")

intc_ratio_wetness_test_fk <- fligner_test(intercept~wetness,
                                           data = intc_ratio_wetness_test_prep,
                                           distribution = approximate(nresample = 9999))

intc_ratio_wetness_test_fk_pval <- 
  xtract_p(intc_ratio_wetness_test_fk)

set.seed(12)
intc_ratio_wetness_test <- kruskal_test(intercept~wetness,
                                        data = intc_ratio_wetness_test_prep,
                                        distribution = approximate(nresample = 9999))

intc_ratio_wetness_test_pval <- 
  xtract_p(intc_ratio_wetness_test)

intc_log_ratio_wetness_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental log ratios")

set.seed(12)
intc_log_ratio_wetness_test_fk <- fligner_test(intercept~wetness,
                                               data = intc_log_ratio_wetness_test_prep,
                                               distribution = approximate(nresample = 9999))

intc_log_ratio_wetness_test_fk_pval <- 
  xtract_p(intc_log_ratio_wetness_test_fk)

set.seed(12)
intc_log_ratio_wetness_test <- kruskal_test(intercept~wetness,
                                            data = intc_log_ratio_wetness_test_prep,
                                            distribution = approximate(nresample = 9999))

intc_log_ratio_wetness_test_pval <- 
  xtract_p(intc_log_ratio_wetness_test)

intc_wetness_val <- c(
  intc_raw_wetness_test_fk_pval,
  intc_raw_wetness_test_pval,
  intc_ratio_wetness_test_fk_pval,
  intc_ratio_wetness_test_pval,
  intc_log_ratio_wetness_test_fk_pval,
  intc_log_ratio_wetness_test_pval
)

intc_wetness_names <- c(
  "intc_raw_wetness_test_fk_pval",
  "intc_raw_wetness_test_pval",
  "intc_ratio_wetness_test_fk_pval",
  "intc_ratio_wetness_test_pval",
  "intc_log_ratio_wetness_test_fk_pval",
  "intc_log_ratio_wetness_test_pval"
)

intc_wetness <- tibble(id = intc_wetness_names, p_value = intc_wetness_val)


### differences between ws ----

intc_raw_ws_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental concentrations")

set.seed(12)
intc_raw_ws_test_fk <- fligner_test(intercept~ws,
                                    data = intc_raw_ws_test_prep,
                                    distribution = approximate(nresample = 9999))

intc_raw_ws_test_fk_pval <- 
  xtract_p(intc_raw_ws_test_fk)

set.seed(12)
intc_raw_ws_test <- kruskal_test(intercept~ws,
                                 data = intc_raw_ws_test_prep,
                                 distribution = approximate(nresample = 9999))

intc_raw_ws_test_pval <- 
  xtract_p(intc_raw_ws_test)

intc_ratio_ws_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental ratios")

set.seed(12)
intc_ratio_ws_test_fk <- fligner_test(intercept~ws,
                                      data = intc_ratio_ws_test_prep,
                                      distribution = approximate(nresample = 9999))

intc_ratio_ws_test_fk_pval <- 
  xtract_p(intc_ratio_ws_test_fk)

set.seed(12)
intc_ratio_ws_test <- kruskal_test(intercept~ws,
                                   data = intc_ratio_ws_test_prep,
                                   distribution = approximate(nresample = 9999))

intc_ratio_ws_test_pval <- 
  xtract_p(intc_ratio_ws_test)

intc_log_ratio_ws_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental log ratios")

set.seed(12)
intc_log_ratio_ws_test_fk <- fligner_test(intercept~ws,
                                          data = intc_log_ratio_ws_test_prep,
                                          distribution = approximate(nresample = 9999))

intc_log_ratio_ws_test_fk_pval <- 
  xtract_p(intc_log_ratio_ws_test_fk)

set.seed(12)
intc_log_ratio_ws_test <- kruskal_test(intercept~ws,
                                       data = intc_log_ratio_ws_test_prep,
                                       distribution = approximate(nresample = 9999))

intc_log_ratio_ws_test_pval <- 
  xtract_p(intc_log_ratio_ws_test)

intc_ws_val <- c(
  intc_raw_ws_test_fk_pval,
  intc_raw_ws_test_pval,
  intc_ratio_ws_test_fk_pval,
  intc_ratio_ws_test_pval,
  intc_log_ratio_ws_test_fk_pval,
  intc_log_ratio_ws_test_pval
)

intc_ws_names <- c(
  "intc_raw_ws_test_fk_pval",
  "intc_raw_ws_test_pval",
  "intc_ratio_ws_test_fk_pval",
  "intc_ratio_ws_test_pval",
  "intc_log_ratio_ws_test_fk_pval",
  "intc_log_ratio_ws_test_pval"
)

intc_ws <- tibble(id = intc_ws_names, p_value = intc_ws_val)

## Slope ----
### differences between various data representations ----
slp_test_data_representation_dry_s_ws_prep <- mod_boxplots %>% 
  filter(configuration == "Dry, WS 1 x 15 mm")

set.seed(12)
slp_test_data_representation_dry_s_ws_fk <- fligner_test(slope~approach,
                                                         data = slp_test_data_representation_dry_s_ws_prep,
                                                         distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_s_ws_fk_pval <- 
  xtract_p(slp_test_data_representation_dry_s_ws_fk)

slp_test_data_representation_dry_s_ws_raw_and_ratio <- slp_test_data_representation_dry_s_ws_prep %>% 
  filter(!approach == "Elemental log ratios")

set.seed(12)
slp_test_data_representation_dry_s_ws_raw_and_ratio_fk <- fligner_test(slope~approach,
                                                                       data = slp_test_data_representation_dry_s_ws_raw_and_ratio,
                                                                       distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_s_ws_raw_and_ratio_fk_pval <- 
  xtract_p(slp_test_data_representation_dry_s_ws_raw_and_ratio_fk)

slp_test_data_representation_dry_s_ws_raw_and_log_ratio <- slp_test_data_representation_dry_s_ws_prep %>% 
  filter(!approach == "Elemental ratios")

set.seed(12)
slp_test_data_representation_dry_s_ws_raw_and_log_ratio_fk <- 
  fligner_test(slope~approach,
               data = slp_test_data_representation_dry_s_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_s_ws_raw_and_log_ratio_fk_pval <- 
  xtract_p(slp_test_data_representation_dry_s_ws_raw_and_log_ratio_fk)

slp_test_data_representation_dry_s_ws_ratio_and_log_ratio <- slp_test_data_representation_dry_s_ws_prep %>% 
  filter(!approach == "Elemental concentrations")

set.seed(12)
slp_test_data_representation_dry_s_ws_ratio_and_log_ratio_fk <- 
  fligner_test(slope~approach,
               data = slp_test_data_representation_dry_s_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_s_ws_ratio_and_log_ratio_fk_pval <- 
  xtract_p(slp_test_data_representation_dry_s_ws_ratio_and_log_ratio_fk)

set.seed(12)
slp_test_data_representation_dry_s_ws <- kruskal_test(slope~approach,
                                                      data = slp_test_data_representation_dry_s_ws_prep,
                                                      distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_s_ws_pval <- 
  xtract_p(slp_test_data_representation_dry_s_ws)

set.seed(12)
slp_test_data_representation_dry_s_ws_raw_and_ratio_kr <- 
  kruskal_test(slope~approach,
               data = slp_test_data_representation_dry_s_ws_raw_and_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_s_ws_raw_and_ratio_kr_pval <- 
  xtract_p(slp_test_data_representation_dry_s_ws_raw_and_ratio_kr)

set.seed(12)
slp_test_data_representation_dry_s_ws_raw_and_log_ratio_kr <- 
  kruskal_test(slope~approach,
               data = slp_test_data_representation_dry_s_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_s_ws_raw_and_log_ratio_kr_pval <- 
  xtract_p(slp_test_data_representation_dry_s_ws_raw_and_log_ratio_kr)

set.seed(12)
slp_test_data_representation_dry_s_ws_ratio_and_log_ratio_kr <- 
  kruskal_test(slope~approach,
               data = slp_test_data_representation_dry_s_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_s_ws_ratio_and_log_ratio_kr_pval <- 
  xtract_p(slp_test_data_representation_dry_s_ws_ratio_and_log_ratio_kr)

slp_test_data_representation_dry_l_ws_prep <- mod_boxplots %>% 
  filter(configuration == "Dry, WS 10 x 15 mm")

set.seed(12)
slp_test_data_representation_dry_l_ws_fk <- fligner_test(slope~approach,
                                                         data = slp_test_data_representation_dry_l_ws_prep,
                                                         distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_l_ws_fk_pval <- 
  xtract_p(slp_test_data_representation_dry_l_ws_fk)

set.seed(12)
slp_test_data_representation_dry_l_ws <- kruskal_test(slope~approach,
                                                      data = slp_test_data_representation_dry_l_ws_prep,
                                                      distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_l_ws_pval <- 
  xtract_p(slp_test_data_representation_dry_l_ws)

slp_test_data_representation_dry_l_ws_raw_and_ratio <- slp_test_data_representation_dry_l_ws_prep %>% 
  filter(!approach == "Elemental log ratios")

set.seed(12)
slp_test_data_representation_dry_l_ws_raw_and_ratio_fk <- fligner_test(slope~approach,
                                                                       data = slp_test_data_representation_dry_l_ws_raw_and_ratio,
                                                                       distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_l_ws_raw_and_ratio_fk_pval <- 
  xtract_p(slp_test_data_representation_dry_l_ws_raw_and_ratio_fk)

slp_test_data_representation_dry_l_ws_raw_and_log_ratio <- slp_test_data_representation_dry_l_ws_prep %>% 
  filter(!approach == "Elemental ratios")

set.seed(12)
slp_test_data_representation_dry_l_ws_raw_and_log_ratio_fk <- fligner_test(slope~approach,
                                                                           data = slp_test_data_representation_dry_l_ws_raw_and_log_ratio,
                                                                           distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_l_ws_raw_and_log_ratio_fk_pval <- 
  xtract_p(slp_test_data_representation_dry_l_ws_raw_and_log_ratio_fk)

slp_test_data_representation_dry_l_ws_ratio_and_log_ratio <- slp_test_data_representation_dry_l_ws_prep %>% 
  filter(!approach == "Elemental concentrations")

set.seed(12)
slp_test_data_representation_dry_l_ws_ratio_and_log_ratio_fk <- fligner_test(slope~approach,
                                                                             data = slp_test_data_representation_dry_l_ws_ratio_and_log_ratio,
                                                                             distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_l_ws_ratio_and_log_ratio_fk_pval <- 
  xtract_p(slp_test_data_representation_dry_l_ws_ratio_and_log_ratio_fk)


set.seed(12)
slp_test_data_representation_dry_l_ws_raw_and_ratio_kr <- 
  kruskal_test(slope~approach,
               data = slp_test_data_representation_dry_l_ws_raw_and_ratio,
               distribution = approximate(nresample = 9999))


slp_test_data_representation_dry_l_ws_raw_and_ratio_kr_pval <- 
  xtract_p(slp_test_data_representation_dry_l_ws_raw_and_ratio_kr)

set.seed(12)
slp_test_data_representation_dry_l_ws_raw_and_log_ratio_kr <- 
  kruskal_test(slope~approach,
               data = slp_test_data_representation_dry_l_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_l_ws_raw_and_log_ratio_kr_pval <- 
  xtract_p(slp_test_data_representation_dry_l_ws_raw_and_log_ratio_kr)

set.seed(12)
slp_test_data_representation_dry_l_ws_ratio_and_log_ratio_kr <- 
  kruskal_test(slope~approach,
               data = slp_test_data_representation_dry_l_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_dry_l_ws_ratio_and_log_ratio_kr_pval <- 
  xtract_p(slp_test_data_representation_dry_l_ws_ratio_and_log_ratio_kr)

slp_test_data_representation_wet_s_ws_prep <- mod_boxplots %>% 
  filter(configuration == "Wet, WS 1 x 15 mm")

set.seed(12)
slp_test_data_representation_wet_s_ws_fk <- fligner_test(slope~approach,
                                                         data = slp_test_data_representation_wet_s_ws_prep,
                                                         distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_s_ws_fk_pval <- 
  xtract_p(slp_test_data_representation_wet_s_ws_fk)

slp_test_data_representation_wet_s_ws_raw_and_ratio <- slp_test_data_representation_wet_s_ws_prep %>% 
  filter(!approach == "Elemental log ratios")

set.seed(12)
slp_test_data_representation_wet_s_ws_raw_and_ratio_fk <- fligner_test(slope~approach,
                                                                       data = slp_test_data_representation_wet_s_ws_raw_and_ratio,
                                                                       distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_s_ws_raw_and_ratio_fk_pval <- 
  xtract_p(slp_test_data_representation_wet_s_ws_raw_and_ratio_fk)

slp_test_data_representation_wet_s_ws_raw_and_log_ratio <- slp_test_data_representation_wet_s_ws_prep %>% 
  filter(!approach == "Elemental ratios")

set.seed(12)
slp_test_data_representation_wet_s_ws_raw_and_log_ratio_fk <- 
  fligner_test(slope~approach,
               data = slp_test_data_representation_wet_s_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_s_ws_raw_and_log_ratio_fk_pval <- 
  xtract_p(slp_test_data_representation_wet_s_ws_raw_and_log_ratio_fk)

slp_test_data_representation_wet_s_ws_ratio_and_log_ratio <- slp_test_data_representation_wet_s_ws_prep %>% 
  filter(!approach == "Elemental concentrations")

set.seed(12)
slp_test_data_representation_wet_s_ws_ratio_and_log_ratio_fk <- 
  fligner_test(slope~approach,
               data = slp_test_data_representation_wet_s_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_s_ws_ratio_and_log_ratio_fk_pval <- 
  xtract_p(slp_test_data_representation_wet_s_ws_ratio_and_log_ratio_fk)

set.seed(12)
slp_test_data_representation_wet_s_ws <- kruskal_test(slope~approach,
                                                      data = slp_test_data_representation_wet_s_ws_prep,
                                                      distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_s_ws_pval <- 
  xtract_p(slp_test_data_representation_wet_s_ws)

set.seed(12)
slp_test_data_representation_wet_s_ws_raw_and_ratio_kr <- 
  kruskal_test(slope~approach,
               data = slp_test_data_representation_wet_s_ws_raw_and_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_s_ws_raw_and_ratio_kr_pval <- 
  xtract_p(slp_test_data_representation_wet_s_ws_raw_and_ratio_kr)

set.seed(12)
slp_test_data_representation_wet_s_ws_raw_and_log_ratio_kr <- 
  kruskal_test(slope~approach,
               data = slp_test_data_representation_wet_s_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_s_ws_raw_and_log_ratio_kr_pval <- 
  xtract_p(slp_test_data_representation_wet_s_ws_raw_and_log_ratio_kr)

set.seed(12)
slp_test_data_representation_wet_s_ws_ratio_and_log_ratio_kr <- 
  kruskal_test(slope~approach,
               data = slp_test_data_representation_wet_s_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_s_ws_ratio_and_log_ratio_kr_pval <- 
  xtract_p(slp_test_data_representation_wet_s_ws_ratio_and_log_ratio_kr)

slp_test_data_representation_wet_l_ws_prep <- mod_boxplots %>% 
  filter(configuration == "Wet, WS 10 x 15 mm")

set.seed(12)
slp_test_data_representation_wet_l_ws_fk <- fligner_test(slope~approach,
                                                         data = slp_test_data_representation_wet_l_ws_prep,
                                                         distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_l_ws_fk_pval <- 
  xtract_p(slp_test_data_representation_wet_l_ws_fk)

slp_test_data_representation_wet_l_ws_raw_and_ratio <- slp_test_data_representation_wet_l_ws_prep %>% 
  filter(!approach == "Elemental log ratios")

set.seed(12)
slp_test_data_representation_wet_l_ws_raw_and_ratio_fk <- fligner_test(slope~approach,
                                                                       data = slp_test_data_representation_wet_l_ws_raw_and_ratio,
                                                                       distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_l_ws_raw_and_ratio_fk_pval <- 
  xtract_p(slp_test_data_representation_wet_l_ws_raw_and_ratio_fk)

slp_test_data_representation_wet_l_ws_raw_and_log_ratio <- slp_test_data_representation_wet_l_ws_prep %>% 
  filter(!approach == "Elemental ratios")

set.seed(12)
slp_test_data_representation_wet_l_ws_raw_and_log_ratio_fk <- 
  fligner_test(slope~approach,
               data = slp_test_data_representation_wet_l_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_l_ws_raw_and_log_ratio_fk_pval <- 
  xtract_p(slp_test_data_representation_wet_l_ws_raw_and_log_ratio_fk)

slp_test_data_representation_wet_l_ws_ratio_and_log_ratio <- slp_test_data_representation_wet_l_ws_prep %>% 
  filter(!approach == "Elemental concentrations")

set.seed(12)
slp_test_data_representation_wet_l_ws_ratio_and_log_ratio_fk <- 
  fligner_test(slope~approach,
               data = slp_test_data_representation_wet_l_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_l_ws_ratio_and_log_ratio_fk_pval <- 
  xtract_p(slp_test_data_representation_wet_l_ws_ratio_and_log_ratio_fk)

set.seed(12)
slp_test_data_representation_wet_l_ws <- kruskal_test(slope~approach,
                                                      data = slp_test_data_representation_wet_l_ws_prep,
                                                      distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_l_ws_pval <- 
  xtract_p(slp_test_data_representation_wet_l_ws)

set.seed(12)
slp_test_data_representation_wet_l_ws_raw_and_ratio_kr <- 
  kruskal_test(slope~approach,
               data = slp_test_data_representation_wet_l_ws_raw_and_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_l_ws_raw_and_ratio_kr_pval <- 
  xtract_p(slp_test_data_representation_wet_l_ws_raw_and_ratio_kr)

set.seed(12)
slp_test_data_representation_wet_l_ws_raw_and_log_ratio_kr <- 
  kruskal_test(slope~approach,
               data = slp_test_data_representation_wet_l_ws_raw_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_l_ws_raw_and_log_ratio_kr_pval <- 
  xtract_p(slp_test_data_representation_wet_l_ws_raw_and_log_ratio_kr)

set.seed(12)
slp_test_data_representation_wet_l_ws_ratio_and_log_ratio_kr <- 
  kruskal_test(slope~approach,
               data = slp_test_data_representation_wet_l_ws_ratio_and_log_ratio,
               distribution = approximate(nresample = 9999))

slp_test_data_representation_wet_l_ws_ratio_and_log_ratio_kr_pval <- 
  xtract_p(slp_test_data_representation_wet_l_ws_ratio_and_log_ratio_kr)

slp_rep_val <- c(
  slp_test_data_representation_dry_s_ws_fk_pval,
  slp_test_data_representation_dry_s_ws_pval,
  slp_test_data_representation_dry_l_ws_fk_pval,
  slp_test_data_representation_dry_l_ws_pval,
  slp_test_data_representation_wet_s_ws_fk_pval,
  slp_test_data_representation_wet_s_ws_pval,
  slp_test_data_representation_wet_l_ws_fk_pval,
  slp_test_data_representation_wet_l_ws_pval
)

slp_rep_names <- c(
  "slp_test_data_representation_dry_s_ws_fk_pval",
  "slp_test_data_representation_dry_s_ws_pval",
  "slp_test_data_representation_dry_l_ws_fk_pval",
  "slp_test_data_representation_dry_l_ws_pval",
  "slp_test_data_representation_wet_s_ws_fk_pval",
  "slp_test_data_representation_wet_s_ws_pval",
  "slp_test_data_representation_wet_l_ws_fk_pval",
  "slp_test_data_representation_wet_l_ws_pval"
)

slp_rep <- tibble(id = slp_rep_names, p_value = slp_rep_val)

slp_rep_pairwise_val <- c(
  slp_test_data_representation_dry_s_ws_raw_and_ratio_fk_pval,
  slp_test_data_representation_dry_s_ws_raw_and_log_ratio_fk_pval,
  slp_test_data_representation_dry_s_ws_ratio_and_log_ratio_fk_pval,
  slp_test_data_representation_dry_s_ws_raw_and_ratio_kr_pval,
  slp_test_data_representation_dry_s_ws_raw_and_log_ratio_kr_pval,
  slp_test_data_representation_dry_s_ws_ratio_and_log_ratio_kr_pval,
  slp_test_data_representation_dry_l_ws_raw_and_ratio_fk_pval,
  slp_test_data_representation_dry_l_ws_raw_and_log_ratio_fk_pval,
  slp_test_data_representation_dry_l_ws_ratio_and_log_ratio_fk_pval,
  slp_test_data_representation_dry_l_ws_raw_and_ratio_kr_pval,
  slp_test_data_representation_dry_l_ws_raw_and_log_ratio_kr_pval,
  slp_test_data_representation_dry_l_ws_ratio_and_log_ratio_kr_pval,
  slp_test_data_representation_wet_s_ws_raw_and_ratio_fk_pval,
  slp_test_data_representation_wet_s_ws_raw_and_log_ratio_fk_pval,
  slp_test_data_representation_wet_s_ws_ratio_and_log_ratio_fk_pval,
  slp_test_data_representation_wet_s_ws_raw_and_ratio_kr_pval,
  slp_test_data_representation_wet_s_ws_raw_and_log_ratio_kr_pval,
  slp_test_data_representation_wet_s_ws_ratio_and_log_ratio_kr_pval,
  slp_test_data_representation_wet_l_ws_raw_and_ratio_fk_pval,
  slp_test_data_representation_wet_l_ws_raw_and_log_ratio_fk_pval,
  slp_test_data_representation_wet_l_ws_ratio_and_log_ratio_fk_pval,
  slp_test_data_representation_wet_l_ws_raw_and_ratio_kr_pval,
  slp_test_data_representation_wet_l_ws_raw_and_log_ratio_kr_pval,
  slp_test_data_representation_wet_l_ws_ratio_and_log_ratio_kr_pval
)

slp_rep_pairwise_names <- c(
  "slp_test_data_representation_dry_s_ws_raw_and_ratio_fk_pval",
  "slp_test_data_representation_dry_s_ws_raw_and_log_ratio_fk_pval",
  "slp_test_data_representation_dry_s_ws_ratio_and_log_ratio_fk_pval",
  "slp_test_data_representation_dry_s_ws_raw_and_ratio_kr_pval",
  "slp_test_data_representation_dry_s_ws_raw_and_log_ratio_kr_pval",
  "slp_test_data_representation_dry_s_ws_ratio_and_log_ratio_kr_pval",
  "slp_test_data_representation_dry_l_ws_raw_and_ratio_fk_pval",
  "slp_test_data_representation_dry_l_ws_raw_and_log_ratio_fk_pval",
  "slp_test_data_representation_dry_l_ws_ratio_and_log_ratio_fk_pval",
  "slp_test_data_representation_dry_l_ws_raw_and_ratio_kr_pval",
  "slp_test_data_representation_dry_l_ws_raw_and_log_ratio_kr_pval",
  "slp_test_data_representation_dry_l_ws_ratio_and_log_ratio_kr_pval",
  "slp_test_data_representation_wet_s_ws_raw_and_ratio_fk_pval",
  "slp_test_data_representation_wet_s_ws_raw_and_log_ratio_fk_pval",
  "slp_test_data_representation_wet_s_ws_ratio_and_log_ratio_fk_pval",
  "slp_test_data_representation_wet_s_ws_raw_and_ratio_kr_pval",
  "slp_test_data_representation_wet_s_ws_raw_and_log_ratio_kr_pval",
  "slp_test_data_representation_wet_s_ws_ratio_and_log_ratio_kr_pval",
  "slp_test_data_representation_wet_l_ws_raw_and_ratio_fk_pval",
  "slp_test_data_representation_wet_l_ws_raw_and_log_ratio_fk_pval",
  "slp_test_data_representation_wet_l_ws_ratio_and_log_ratio_fk_pval",
  "slp_test_data_representation_wet_l_ws_raw_and_ratio_kr_pval",
  "slp_test_data_representation_wet_l_ws_raw_and_log_ratio_kr_pval",
  "slp_test_data_representation_wet_l_ws_ratio_and_log_ratio_kr_pval"
)

slp_rep_pairwise <- tibble(id = slp_rep_pairwise_names, p_value = slp_rep_pairwise_val)

### differences between wetness ----

mod_boxplots_xconfs <- mod_boxplots %>% 
  separate(configuration, into = c("wetness", "ws"), sep = ",") %>% 
  mutate(ws = gsub(" WS", "WS", ws),
         ws = factor(ws),
         wetness = factor(wetness))

slp_raw_wetness_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental concentrations")

set.seed(12)
slp_raw_wetness_test_fk <- fligner_test(slope~wetness,
                                        data = slp_raw_wetness_test_prep,
                                        distribution = approximate(nresample = 9999))

slp_raw_wetness_test_fk_pval <- 
  xtract_p(slp_raw_wetness_test_fk)

set.seed(12)
slp_raw_wetness_test <- kruskal_test(slope~wetness,
                                     data = slp_raw_wetness_test_prep,
                                     distribution = approximate(nresample = 9999))

slp_raw_wetness_test_pval <- 
  xtract_p(slp_raw_wetness_test)

slp_ratio_wetness_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental ratios")

slp_ratio_wetness_test_fk <- fligner_test(slope~wetness,
                                          data = slp_ratio_wetness_test_prep,
                                          distribution = approximate(nresample = 9999))

slp_ratio_wetness_test_fk_pval <- 
  xtract_p(slp_ratio_wetness_test_fk)

set.seed(12)
slp_ratio_wetness_test <- kruskal_test(slope~wetness,
                                       data = slp_ratio_wetness_test_prep,
                                       distribution = approximate(nresample = 9999))

slp_ratio_wetness_test_pval <- 
  xtract_p(slp_ratio_wetness_test)

slp_log_ratio_wetness_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental log ratios")

set.seed(12)
slp_log_ratio_wetness_test_fk <- fligner_test(slope~wetness,
                                              data = slp_log_ratio_wetness_test_prep,
                                              distribution = approximate(nresample = 9999))

slp_log_ratio_wetness_test_fk_pval <- 
  xtract_p(slp_log_ratio_wetness_test_fk)

set.seed(12)
slp_log_ratio_wetness_test <- kruskal_test(slope~wetness,
                                           data = slp_log_ratio_wetness_test_prep,
                                           distribution = approximate(nresample = 9999))

slp_log_ratio_wetness_test_pval <- 
  xtract_p(slp_log_ratio_wetness_test)

slp_wetness_val <- c(
  slp_raw_wetness_test_fk_pval,
  slp_raw_wetness_test_pval,
  slp_ratio_wetness_test_fk_pval,
  slp_ratio_wetness_test_pval,
  slp_log_ratio_wetness_test_fk_pval,
  slp_log_ratio_wetness_test_pval
)

slp_wetness_names <- c(
  "slp_raw_wetness_test_fk_pval",
  "slp_raw_wetness_test_pval",
  "slp_ratio_wetness_test_fk_pval",
  "slp_ratio_wetness_test_pval",
  "slp_log_ratio_wetness_test_fk_pval",
  "slp_log_ratio_wetness_test_pval"
)

slp_wetness <- tibble(id = slp_wetness_names, p_value = slp_wetness_val)

### differences between ws ----

slp_raw_ws_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental concentrations")

set.seed(12)
slp_raw_ws_test_fk <- fligner_test(slope~ws,
                                   data = slp_raw_ws_test_prep,
                                   distribution = approximate(nresample = 9999))

slp_raw_ws_test_fk_pval <- 
  xtract_p(slp_raw_ws_test_fk)

set.seed(12)
slp_raw_ws_test <- kruskal_test(slope~ws,
                                data = slp_raw_ws_test_prep,
                                distribution = approximate(nresample = 9999))

slp_raw_ws_test_pval <- 
  xtract_p(slp_raw_ws_test)

slp_ratio_ws_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental ratios")

set.seed(12)
slp_ratio_ws_test_fk <- fligner_test(slope~ws,
                                     data = slp_ratio_ws_test_prep,
                                     distribution = approximate(nresample = 9999))

slp_ratio_ws_test_fk_pval <- 
  xtract_p(slp_ratio_ws_test_fk)

set.seed(12)
slp_ratio_ws_test <- kruskal_test(slope~ws,
                                  data = slp_ratio_ws_test_prep,
                                  distribution = approximate(nresample = 9999))

slp_ratio_ws_test_pval <- 
  xtract_p(slp_ratio_ws_test)

slp_log_ratio_ws_test_prep <- mod_boxplots_xconfs %>% 
  filter(approach == "Elemental log ratios")

set.seed(12)
slp_log_ratio_ws_test_fk <- fligner_test(slope~ws,
                                         data = slp_log_ratio_ws_test_prep,
                                         distribution = approximate(nresample = 9999))

slp_log_ratio_ws_test_fk_pval <- 
  xtract_p(slp_log_ratio_ws_test_fk)

set.seed(12)
slp_log_ratio_ws_test <- kruskal_test(slope~ws,
                                      data = slp_log_ratio_ws_test_prep,
                                      distribution = approximate(nresample = 9999))

slp_log_ratio_ws_test_pval <- 
  xtract_p(slp_log_ratio_ws_test)

slp_ws_val <- c(
  slp_raw_ws_test_fk_pval,
  slp_raw_ws_test_pval,
  slp_ratio_ws_test_fk_pval,
  slp_ratio_ws_test_pval,
  slp_log_ratio_ws_test_fk_pval,
  slp_log_ratio_ws_test_pval 
)

slp_ws_names <- c(
  "slp_raw_ws_test_fk_pval",
  "slp_raw_ws_test_pval",
  "slp_ratio_ws_test_fk_pval",
  "slp_ratio_ws_test_pval",
  "slp_log_ratio_ws_test_fk_pval",
  "slp_log_ratio_ws_test_pval"
)

slp_ws <- tibble(id = slp_ws_names, p_value = slp_ws_val)

## Tables with results of statistical tests ----

#Table 1. Differences between data representions across configurations
#1A
table_1a <- tibble(
  "Variable" = c("R<sup>2</sup>", "Intercept", "Slope"),
  "Dry, WS 1 x 15 mm" = c(rsq_test_data_representation_dry_s_ws_pval,
                          intc_test_data_representation_dry_s_ws_pval,
                          slp_test_data_representation_dry_s_ws_pval),
  "Dry, WS 10 x 15 mm" = c(rsq_test_data_representation_dry_l_ws_pval,
                           intc_test_data_representation_dry_l_ws_pval,
                           slp_test_data_representation_dry_l_ws_pval),
  "Wet, WS 1 x 15 mm" = c(rsq_test_data_representation_wet_s_ws_pval,
                          intc_test_data_representation_wet_s_ws_pval,
                          slp_test_data_representation_wet_s_ws_pval),
  "Wet, WS 10 x 15 mm" = c(rsq_test_data_representation_wet_l_ws_pval,
                           intc_test_data_representation_wet_l_ws_pval,
                           slp_test_data_representation_wet_l_ws_pval)
) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
  mutate(across(where(is.numeric), ~ case_when(
    .x > 0.01 & .x <= 0.05 ~ paste0(format(., nsmall = 3), "*"),
    .x > 0.001 & .x <= 0.01 ~ paste0(format(., nsmall = 3), "**"),
    .x <= 0.001 ~ paste0(format(., nsmall = 3), "***"),
    TRUE ~ format(.x, nsmall = 3)
  ))) %>% 
  kbl(booktabs = TRUE,
      #  align = c("l", "r", "r"),
      linesep = "",
      escape = FALSE,
      caption = "Kruskal-Wallis test for examining the difference in medians among the ways data presentations") %>% 
  kable_paper(full_width = TRUE)

table_1a

save_kable(table_1a, file = "tables/table_1a.html")
webshot("tables/table_1a.html", file = "tables/table_1a_raster.jpg", 
        vwidth = 970,
        vheight = 190)


#1B
table_1b <- tibble(
  "Variable" = c("R<sup>2</sup>", "Intercept", "Slope"),
  "Dry, WS 1 x 15 mm" = c(rsq_test_data_representation_dry_s_ws_fk_pval,
                          intc_test_data_representation_dry_s_ws_fk_pval,
                          slp_test_data_representation_dry_s_ws_fk_pval),
  "Dry, WS 10 x 15 mm" = c(rsq_test_data_representation_dry_l_ws_fk_pval,
                           intc_test_data_representation_dry_l_ws_fk_pval,
                           slp_test_data_representation_dry_l_ws_fk_pval),
  "Wet, WS 1 x 15 mm" = c(rsq_test_data_representation_wet_s_ws_fk_pval,
                          intc_test_data_representation_wet_s_ws_fk_pval,
                          slp_test_data_representation_wet_s_ws_fk_pval),
  "Wet, WS 10 x 15 mm" = c(rsq_test_data_representation_wet_l_ws_fk_pval,
                           intc_test_data_representation_wet_l_ws_fk_pval,
                           slp_test_data_representation_wet_l_ws_fk_pval)
) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
  mutate(across(where(is.numeric), ~ case_when(
    .x > 0.01 & .x <= 0.05 ~ paste0(format(., nsmall = 3), "*"),
    .x > 0.001 & .x <= 0.01 ~ paste0(format(., nsmall = 3), "**"),
    .x <= 0.001 ~ paste0(format(., nsmall = 3), "***"),
    TRUE ~ format(.x, nsmall = 3)
  ))) %>% 
  kbl(booktabs = TRUE,
      #  align = c("l", "r", "r"),
      linesep = "",
      escape = FALSE,
      caption = "Fligner-Killeen test for examining difference in variances among the ways of data presentations") %>% 
  kable_paper(full_width = TRUE)

table_1b

save_kable(table_1b, file = "tables/table_1b.html")
webshot("tables/table_1b.html", file = "tables/table_1b_raster.jpg", 
        vwidth = 970,
        vheight = 190)


#Supplementary Table 1 # tam gdzie NA, znaczy, ze global test pokazal nieistotnosc roznicy, nie ma R squared, bo global nie wykazal istotnosci roznicy
#S1A kruskal raw vs ratio
table_s1a <- tibble(
  "Variable" = c("Intercept", "Slope"),
  "Dry, WS 1 x 15 mm" = c(intc_test_data_representation_dry_s_ws_raw_and_ratio_ks_pval,
                          slp_test_data_representation_dry_s_ws_raw_and_ratio_kr_pval),
  "Dry, WS 10 x 15 mm" = c(intc_test_data_representation_dry_l_ws_raw_and_ratio_ks_pval,
                           slp_test_data_representation_dry_l_ws_raw_and_ratio_kr_pval),
  "Wet, WS 1 x 15 mm" = c(intc_test_data_representation_wet_s_ws_raw_and_ratio_kr_pval,
                          slp_test_data_representation_wet_s_ws_raw_and_ratio_kr_pval),
  "Wet, WS 10 x 15 mm" = c(intc_test_data_representation_wet_l_ws_raw_and_ratio_kr_pval,
                           slp_test_data_representation_wet_l_ws_raw_and_ratio_kr_pval)
) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
  mutate(across(where(is.numeric), ~ case_when(
    .x > 0.01 & .x <= 0.05 ~ paste0(format(., nsmall = 3), "*"),
    .x > 0.001 & .x <= 0.01 ~ paste0(format(., nsmall = 3), "**"),
    .x <= 0.001 ~ paste0(format(., nsmall = 3), "***"),
    TRUE ~ format(.x, nsmall = 3)
  ))) %>% 
  kbl(booktabs = TRUE,
      #  align = c("l", "r", "r"),
      linesep = "",
      escape = FALSE,
      caption = "Kruskal-Wallis test for examining the difference in medians between elemental concentrations and elemental ratios") %>% 
  kable_paper(full_width = TRUE)

table_s1a

save_kable(table_s1a, file = "tables/table_s1a.html")
webshot("tables/table_s1a.html", file = "tables/table_s1a_raster.jpg", 
        vwidth = 970,
        vheight = 190)

#S1B kruskal raw vs log ratio
table_s1b <- tibble(
  "Variable" = c("Intercept", "Slope"),
  "Dry, WS 1 x 15 mm" = c(intc_test_data_representation_dry_s_ws_raw_and_log_ratio_ks_pval,
                          slp_test_data_representation_dry_s_ws_raw_and_log_ratio_kr_pval),
  "Dry, WS 10 x 15 mm" = c(intc_test_data_representation_dry_l_ws_raw_and_log_ratio_ks_pval,
                           slp_test_data_representation_dry_l_ws_raw_and_log_ratio_kr_pval),
  "Wet, WS 1 x 15 mm" = c(intc_test_data_representation_wet_s_ws_raw_and_log_ratio_kr_pval,
                          slp_test_data_representation_wet_s_ws_raw_and_log_ratio_kr_pval),
  "Wet, WS 10 x 15 mm" = c(intc_test_data_representation_wet_l_ws_raw_and_log_ratio_kr_pval,
                           slp_test_data_representation_wet_l_ws_raw_and_log_ratio_kr_pval)
) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
  mutate(across(where(is.numeric), ~ case_when(
    .x > 0.01 & .x <= 0.05 ~ paste0(format(., nsmall = 3), "*"),
    .x > 0.001 & .x <= 0.01 ~ paste0(format(., nsmall = 3), "**"),
    .x <= 0.001 ~ paste0(format(., nsmall = 3), "***"),
    TRUE ~ format(.x, nsmall = 3)
  ))) %>% 
  kbl(booktabs = TRUE,
      #  align = c("l", "r", "r"),
      linesep = "",
      escape = FALSE,
      caption = "Kruskal-Wallis test for examining the difference in medians between elemental concentrations and elemental log ratios") %>% 
  kable_paper(full_width = TRUE)

table_s1b

save_kable(table_s1b, file = "tables/table_s1b.html")
webshot("tables/table_s1b.html", file = "tables/table_s1b_raster.jpg", 
        vwidth = 970,
        vheight = 190)

#S1C kruskal ratio vs log ratio
table_s1c <- tibble(
  "Variable" = c("Intercept", "Slope"),
  "Dry, WS 1 x 15 mm" = c(intc_test_data_representation_dry_s_ws_ratio_and_log_ratio_ks_pval,
                          slp_test_data_representation_dry_s_ws_ratio_and_log_ratio_kr_pval),
  "Dry, WS 10 x 15 mm" = c(intc_test_data_representation_dry_l_ws_ratio_and_log_ratio_ks_pval,
                           slp_test_data_representation_dry_l_ws_ratio_and_log_ratio_kr_pval),
  "Wet, WS 1 x 15 mm" = c(intc_test_data_representation_wet_s_ws_ratio_and_log_ratio_kr_pval,
                          slp_test_data_representation_wet_s_ws_ratio_and_log_ratio_kr_pval),
  "Wet, WS 10 x 15 mm" = c(intc_test_data_representation_wet_l_ws_ratio_and_log_ratio_kr_pval,
                           slp_test_data_representation_wet_l_ws_ratio_and_log_ratio_kr_pval)
) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
  mutate(across(where(is.numeric), ~ case_when(
    .x > 0.01 & .x <= 0.05 ~ paste0(format(., nsmall = 3), "*"),
    .x > 0.001 & .x <= 0.01 ~ paste0(format(., nsmall = 3), "**"),
    .x <= 0.001 ~ paste0(format(., nsmall = 3), "***"),
    TRUE ~ format(.x, nsmall = 3)
  ))) %>% 
  kbl(booktabs = TRUE,
      #  align = c("l", "r", "r"),
      linesep = "",
      escape = FALSE,
      caption = "Kruskal-Wallis test for examining the difference in medians between elemental ratios and elemental log ratios") %>% 
  kable_paper(full_width = TRUE)

table_s1c

save_kable(table_s1c, file = "tables/table_s1c.html")
webshot("tables/table_s1c.html", file = "tables/table_s1c_raster.jpg", 
        vwidth = 970,
        vheight = 190)

#S1D fligner raw vs ratio
table_s1d <- tibble(
  "Variable" = c("Intercept", "Slope"),
  "Dry, WS 1 x 15 mm" = c(intc_test_data_representation_dry_s_ws_raw_and_ratio_fk_pval,
                          slp_test_data_representation_dry_s_ws_raw_and_ratio_fk_pval),
  "Dry, WS 10 x 15 mm" = c(intc_test_data_representation_dry_l_ws_raw_and_ratio_fk_pval,
                           slp_test_data_representation_dry_l_ws_raw_and_ratio_fk_pval),
  "Wet, WS 1 x 15 mm" = c(intc_test_data_representation_wet_s_ws_raw_and_ratio_fk_pval,
                          slp_test_data_representation_wet_s_ws_raw_and_ratio_fk_pval),
  "Wet, WS 10 x 15 mm" = c(intc_test_data_representation_wet_l_ws_raw_and_ratio_fk_pval,
                           slp_test_data_representation_wet_l_ws_raw_and_ratio_fk_pval)
) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
  mutate(across(where(is.numeric), ~ case_when(
    .x > 0.01 & .x <= 0.05 ~ paste0(format(., nsmall = 3), "*"),
    .x > 0.001 & .x <= 0.01 ~ paste0(format(., nsmall = 3), "**"),
    .x <= 0.001 ~ paste0(format(., nsmall = 3), "***"),
    TRUE ~ format(.x, nsmall = 3)
  ))) %>% 
  kbl(booktabs = TRUE,
      #  align = c("l", "r", "r"),
      linesep = "",
      escape = FALSE,
      caption = "Fligner-Killeen test for examining the difference in variances between elemental concentrations and elemental ratios") %>% 
  kable_paper(full_width = TRUE)

table_s1d

save_kable(table_s1d, file = "tables/table_s1d.html")
webshot("tables/table_s1d.html", file = "tables/table_s1d_raster.jpg", 
        vwidth = 970,
        vheight = 190)



#S1E fligner raw vs log ratio
table_s1e <- tibble(
  "Variable" = c("Intercept", "Slope"),
  "Dry, WS 1 x 15 mm" = c(intc_test_data_representation_dry_s_ws_raw_and_log_ratio_fk_pval,
                          slp_test_data_representation_dry_s_ws_raw_and_log_ratio_fk_pval),
  "Dry, WS 10 x 15 mm" = c(intc_test_data_representation_dry_l_ws_raw_and_log_ratio_fk_pval,
                           slp_test_data_representation_dry_l_ws_raw_and_log_ratio_fk_pval),
  "Wet, WS 1 x 15 mm" = c(intc_test_data_representation_wet_s_ws_raw_and_log_ratio_fk_pval,
                          slp_test_data_representation_wet_s_ws_raw_and_log_ratio_fk_pval),
  "Wet, WS 10 x 15 mm" = c(intc_test_data_representation_wet_l_ws_raw_and_log_ratio_fk_pval,
                           slp_test_data_representation_wet_l_ws_raw_and_log_ratio_fk_pval)
) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
  mutate(across(where(is.numeric), ~ case_when(
    .x > 0.01 & .x <= 0.05 ~ paste0(format(., nsmall = 3), "*"),
    .x > 0.001 & .x <= 0.01 ~ paste0(format(., nsmall = 3), "**"),
    .x <= 0.001 ~ paste0(format(., nsmall = 3), "***"),
    TRUE ~ format(.x, nsmall = 3)
  ))) %>% 
  kbl(booktabs = TRUE,
      #  align = c("l", "r", "r"),
      linesep = "",
      escape = FALSE,
      caption = "Fligner-Killeen test for examining the difference in variances between elemental concentrations and elemental log ratios") %>% 
  kable_paper(full_width = TRUE)

table_s1e

save_kable(table_s1e, file = "tables/table_s1e.html")
webshot("tables/table_s1e.html", file = "tables/table_s1e_raster.jpg", 
        vwidth = 970,
        vheight = 190)

#S1F fligner ratio vs log ratio
table_s1f <- tibble(
  "Variable" = c("Intercept", "Slope"),
  "Dry, WS 1 x 15 mm" = c(intc_test_data_representation_dry_s_ws_ratio_and_log_ratio_fk_pval,
                          slp_test_data_representation_dry_s_ws_ratio_and_log_ratio_fk_pval),
  "Dry, WS 10 x 15 mm" = c(intc_test_data_representation_dry_l_ws_ratio_and_log_ratio_fk_pval,
                           slp_test_data_representation_dry_l_ws_ratio_and_log_ratio_fk_pval),
  "Wet, WS 1 x 15 mm" = c(intc_test_data_representation_wet_s_ws_ratio_and_log_ratio_fk_pval,
                          slp_test_data_representation_wet_s_ws_ratio_and_log_ratio_fk_pval),
  "Wet, WS 10 x 15 mm" = c(intc_test_data_representation_wet_l_ws_ratio_and_log_ratio_fk_pval,
                           slp_test_data_representation_wet_l_ws_ratio_and_log_ratio_fk_pval)
) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
  mutate(across(where(is.numeric), ~ case_when(
    .x > 0.01 & .x <= 0.05 ~ paste0(format(., nsmall = 3), "*"),
    .x > 0.001 & .x <= 0.01 ~ paste0(format(., nsmall = 3), "**"),
    .x <= 0.001 ~ paste0(format(., nsmall = 3), "***"),
    TRUE ~ format(.x, nsmall = 3)
  ))) %>% 
  kbl(booktabs = TRUE,
      #  align = c("l", "r", "r"),
      linesep = "",
      escape = FALSE,
      caption = "Fligner-Killeen test for examining the difference in variances between elemental ratios and elemental log ratios") %>% 
  kable_paper(full_width = TRUE)

table_s1f

save_kable(table_s1f, file = "tables/table_s1f.html")
webshot("tables/table_s1f.html", file = "tables/table_s1f_raster.jpg", 
        vwidth = 970,
        vheight = 190)

# Table 2. Differences between wetness classes
#2A Kruskal
table_2a <- tibble(
  "Variable" = c("R<sup>2</sup>", "Intercept", "Slope"),
  "Elemental concentrations" = c(rsq_raw_wetness_test_pval,
                           intc_raw_wetness_test_pval,
                           slp_raw_wetness_test_pval),
  "Elemental ratios" = c(rsq_ratio_wetness_test_pval,
                       intc_ratio_wetness_test_pval,
                       slp_ratio_wetness_test_pval),
  "Elemental log ratios" = c(rsq_log_ratio_wetness_test_pval,
                           intc_log_ratio_wetness_test_pval,
                           slp_log_ratio_wetness_test_pval)
) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
  mutate(across(where(is.numeric), ~ case_when(
    .x > 0.01 & .x <= 0.05 ~ paste0(format(., nsmall = 3), "*"),
    .x > 0.001 & .x <= 0.01 ~ paste0(format(., nsmall = 3), "**"),
    .x <= 0.001 ~ paste0(format(., nsmall = 3), "***"),
    TRUE ~ format(.x, nsmall = 3)
  ))) %>% 
  kbl(booktabs = TRUE,
      #  align = c("l", "r", "r"),
      linesep = "",
      escape = FALSE,
      caption = "Kruskal-Wallis test for examining the difference in medians between wet and dry samples") %>% 
  kable_paper(full_width = TRUE)

table_2a

save_kable(table_2a, file = "tables/table_2a.html")
webshot("tables/table_2a.html", file = "tables/table_2a_raster.jpg", 
        vwidth = 970,
        vheight = 190)

#2B Fligner
table_2b <- tibble(
  "Variable" = c("R<sup>2</sup>", "Intercept", "Slope"),
  "Elemental concentrations" = c(rsq_raw_wetness_test_fk_pval,
                           intc_raw_wetness_test_fk_pval,
                           slp_raw_wetness_test_fk_pval),
  "Elemental ratios" = c(rsq_ratio_wetness_test_fk_pval,
                       intc_ratio_wetness_test_fk_pval,
                       slp_ratio_wetness_test_fk_pval),
  "Elemental log ratios" = c(rsq_log_ratio_wetness_test_fk_pval,
                           intc_log_ratio_wetness_test_fk_pval,
                           slp_log_ratio_wetness_test_fk_pval)
) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
  mutate(across(where(is.numeric), ~ case_when(
    .x > 0.01 & .x <= 0.05 ~ paste0(format(., nsmall = 3), "*"),
    .x > 0.001 & .x <= 0.01 ~ paste0(format(., nsmall = 3), "**"),
    .x <= 0.001 ~ paste0(format(., nsmall = 3), "***"),
    TRUE ~ format(.x, nsmall = 3)
  ))) %>% 
  kbl(booktabs = TRUE,
      #  align = c("l", "r", "r"),
      linesep = "",
      escape = FALSE,
      caption = "Fligner-Killeen test for examining the difference in variances between wet and dry samples") %>% 
  kable_paper(full_width = TRUE)

table_2b

save_kable(table_2b, file = "tables/table_2b.html")
webshot("tables/table_2b.html", file = "tables/table_2b_raster.jpg", 
        vwidth = 970,
        vheight = 190)

# Table 3. Differences between WS classes
#3A Kruskal
table_3a <- tibble(
  "Variable" = c("R<sup>2</sup>", "Intercept", "Slope"),
  "Elemental concentrations" = c(rsq_raw_ws_test_pval,
                           intc_raw_ws_test_pval,
                           slp_raw_ws_test_pval),
  "Elemental ratios" = c(rsq_ratio_ws_test_pval,
                       intc_ratio_ws_test_pval,
                       slp_ratio_ws_test_pval),
  "Elemental log ratios" = c(rsq_log_ratio_ws_test_pval,
                           intc_log_ratio_ws_test_pval,
                           slp_log_ratio_ws_test_pval)
) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
  mutate(across(where(is.numeric), ~ case_when(
    .x > 0.01 & .x <= 0.05 ~ paste0(format(., nsmall = 3), "*"),
    .x > 0.001 & .x <= 0.01 ~ paste0(format(., nsmall = 3), "**"),
    .x <= 0.001 ~ paste0(format(., nsmall = 3), "***"),
    TRUE ~ format(.x, nsmall = 3)
  ))) %>% 
  kbl(booktabs = TRUE,
      #  align = c("l", "r", "r"),
      linesep = "",
      escape = FALSE,
      caption = "Kruskal-Wallis test for examining the difference in medians between WS 1 x 15 mm and WS 10 x 15 mm") %>% 
  kable_paper(full_width = TRUE)

table_3a

save_kable(table_3a, file = "tables/table_3a.html")
webshot("tables/table_3a.html", file = "tables/table_3a_raster.jpg", 
        vwidth = 970,
        vheight = 190)

#3B Fligner
table_3b <- tibble(
  "Variable" = c("R<sup>2</sup>", "Intercept", "Slope"),
  "Elemental concentrations" = c(rsq_raw_ws_test_fk_pval,
                           intc_raw_ws_test_fk_pval,
                           slp_raw_ws_test_fk_pval),
  "Elemental ratios" = c(rsq_ratio_ws_test_fk_pval,
                       intc_ratio_ws_test_fk_pval,
                       slp_ratio_ws_test_fk_pval),
  "Elemental log ratios" = c(rsq_log_ratio_ws_test_fk_pval,
                           intc_log_ratio_ws_test_fk_pval,
                           slp_log_ratio_ws_test_fk_pval)
) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
  mutate(across(where(is.numeric), ~ case_when(
    .x > 0.01 & .x <= 0.05 ~ paste0(format(., nsmall = 3), "*"),
    .x > 0.001 & .x <= 0.01 ~ paste0(format(., nsmall = 3), "**"),
    .x <= 0.001 ~ paste0(format(., nsmall = 3), "***"),
    TRUE ~ format(.x, nsmall = 3)
  ))) %>% 
  kbl(booktabs = TRUE,
      #  align = c("l", "r", "r"),
      linesep = "",
      escape = FALSE,
      caption = "Fligner-Killeen test for examining the difference in variances between WS 1 x 15 and WS 10 x 15 mm") %>% 
  kable_paper(full_width = TRUE)

table_3b

save_kable(table_3b, file = "tables/table_3b.html")
webshot("tables/table_3b.html", file = "tables/table_3b_raster.jpg", 
        vwidth = 970,
        vheight = 190)

