library(tidyverse)

standard_ID_2_select <- read_rds("data/standrad_ID.rds")

file_names <- list.files(path = "data", full.names = TRUE)
file_names <- file_names[-which(file_names == "data/standrad_ID.rds")]
file_names <- file_names[-which(file_names == "data/standards_characteristics.xlsx")]

read_raw_xrf <- function(file) {
  read_csv(file, skip = 5) %>% 
    rename(standard_ID = cm...1) %>% 
    dplyr::select(!cm...2:`LE (Value)...8` & !`LE (Value)...53`:`UO2 (Error)`) %>% 
    filter(standard_ID %in% standard_ID_2_select)
}


xrf_list <- lapply(file_names, read_raw_xrf)

remove_from_names <- c("data/", ".csv", "_he")
clean_names <- file_names
for (phrase in remove_from_names) {
  clean_names <- gsub(phrase, "", clean_names)
}

names(xrf_list) <- clean_names

xrf_full_prep1 <- xrf_list %>% 
  imap(~ .x %>% mutate(treatment = .y)) %>% 
  bind_rows() %>% 
  mutate(
    standard_age = ifelse(grepl("Nowe", treatment), "new", "old"),
    window_settings = ifelse(grepl("10x15", treatment), "10 x 15 mm", "1 x 15 mm"),
    replication = ifelse(grepl("_3", treatment), "3", "1"),
    replication = as.factor(ifelse(grepl("_2", treatment), "2", replication)),
    treatment = ifelse(grepl("mokre", treatment), "wet", "dry")
  )

xrf_long_value <- xrf_full_prep1 %>%
  pivot_longer(
    cols = ends_with("(Value)"),
    names_to = "element",
    names_pattern = "(.*) \\(Value\\)",
    values_to = "concentration"
  ) %>% 
  select(!`Mg (Error)`:`U (Error)`) %>% 
  mutate(n = 1:length(element))

xrf_long_error <- xrf_full_prep1 %>%
  pivot_longer(
    cols = ends_with("(Error)"),
    names_to = "element",
    names_pattern = "(.*) \\(Error\\)",
    values_to = "error"
  ) %>% 
  select(!`Mg (Value)`:`U (Value)`) %>% 
  mutate(n = 1:length(element))
  

xrf_long_prep <- full_join(xrf_long_value, xrf_long_error,
                           by = c("standard_ID",
                                  "element",
                                  "treatment",
                                  "standard_age",
                                  "window_settings",
                                  "replication",
                                  "n")) %>% 
  select(!n)

old_standards <- xrf_long_prep %>% 
  distinct(standard_ID, .keep_all = TRUE) %>% 
  filter(standard_age == "old") %>% 
  pull(standard_ID)

standard_characteristics <- read_csv("data/standard_characteristics.csv") %>% 
  mutate(treatment = "standard",
         standard_age = ifelse(standard_ID %in% old_standards, "old", "new")) %>% 
  filter(!is.na(concentration))

xrf_long_red <- xrf_long_prep %>% 
  filter(standard_ID %in% unique(standard_characteristics$standard_ID)) %>% 
  filter(element %in% unique(standard_characteristics$element))

xrf_long <- xrf_long_prep %>%
  bind_rows(standard_characteristics) %>%
  group_by(standard_ID) %>% 
  filter(element %in% element[treatment == "standard"]) %>% 
  ungroup() %>% 
  mutate(across(where(is.character), as.factor))

write_rds(xrf_long, "data/xrf_long.rds")

#w ostatecznym pliku (xrf_long) mam: 10 pomiarow dla kazdego pierwiastka z pojedynczego skanowania razy 3, bo byly 3 skanowania (3 replications) dla dwoch ustawien wielkosci okna (1 x 15 mm i 10 x 15 mm) i dla dwoch treatments (wet i dry) + wartosci dla standardow odczytane z kart charakterystyk. Zachowane sa tylko te pierwiastki przy kazdym ze standardow, dla ktorych byly podane koncentracje na kartach charakterystyk (czyli nawet jesli jakis pierwiastek mial duze koncentracje pomierzone w trakcie skanowania, ale koncentracje dla niego nie byly podane na karcie, to go tu nie uwzglednialem). Wartoci bledow w wiekszosci przypadkow nie byly podane na karcie charakterystyk. 

  
  
