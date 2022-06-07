
# libraries -----------------------------------------------------------------

library(tidyverse)

# 1. names.glue parameter ----------------------------------------------------

(storms_sum <- storms %>% 
  filter(year %in% 1975:1977) %>% 
  group_by(year, status) %>% 
  summarise(mean = mean(pressure, na.rm = TRUE),
                   median = median(pressure, na.rm = TRUE),
                   .groups = "drop")) # add this to your summarise() to drop the groups

storms_sum %>% 
  pivot_wider(names_from = "year", 
              values_from = c("mean", "median"))

wide_storms <- storms_sum %>% 
  pivot_wider(names_from = "year", 
              values_from = c("mean", "median"), 
              names_glue = "{.value}_of_{year}")


# 2. Custom break pivot_longer ---------------------------------------------

wide_storms %>% 
  pivot_longer(-status, 
               names_to = c("stat", "year"),
               names_pattern = "(.*)_of_(.*)",
               names_transform = list(year = as.integer),
               values_to = "value")


# 3. summarise multiple columns with multiple functions ----------------------

mtcars %>% 
  group_by(cyl) %>% 
  summarise(across(starts_with("d"),
                   list(mean = mean, median = median),
                   .names = "{fn}_of_{col}"))
