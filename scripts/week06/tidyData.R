# “Tidy datasets are all alike, 
# but every messy dataset is messy in its own way.” 
# –– Hadley Wickham

# libraries needed --------------------------------------------------------

library(tidyverse)

# Tidy Data ---------------------------------------------------------------

# What is the difference between those tibbles what are the similarities?
# How can you check them?

table1
table2
table3
table4a
table4b


# Tidy Rules -------------------------------------------------------------------

# 1. Each variable must have its own column.
# 2. Each observation must have its own row.
# 3. Each value must have its own cell.


# Practical Rules ---------------------------------------------------------

# 1. Put each dataset in a tibble.
# 2. Put each variable in a column.


# 1. Reasons for Tidy Data ------------------------------------------------

# Compute rate per 10,000
table1 %>% 
  mutate(rate = cases / population * 10000)

# Compute cases per year
table1 %>% 
  count(year, wt = cases)

# Visualise changes over time
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))


# 1.1. Exercise -----------------------------------------------------------

# Compute the rate for table2, and table4a + table4b.
# Discuss how you can achieve this.
# Which table is the easiest to work with?


# Real World Problems -----------------------------------------------------

# 1. One variable might be spread across multiple columns.
# 2. One observation might be scattered across multiple rows.

# Pivoting ----------------------------------------------------------------

# pivot_longer()

table4a

table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")

# full Transformation

tidy4a_t <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), 
               names_to = "year", 
               values_to = "cases", 
               names_transform = list(year = as.integer))
tidy4b_t <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), 
               names_to = "year", 
               values_to = "population", 
               names_transform = list(year = as.integer))
left_join(tidy4a_t, tidy4b_t)

# pivot_wider()

table2

table2 %>%
  pivot_wider(names_from = type, values_from = count)


# 1.2. Exercise -----------------------------------------------------------

# 1. Tidy the simple tibble below. 
# Do you need to make it wider or longer? What are the variables?
  
  preg <- tribble(
    ~pregnant, ~male, ~female,
    "yes",     NA,    10,
    "no",      20,    12
  )

# 2. Tidy this dataset

returns <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
) 





# Example solutions
preg %>% 
  pivot_longer(c(male,female), names_to = "gender") %>%
  mutate(pregnant = ifelse(pregnant == "yes", "pregnant", "not_pregnant")) %>%
  pivot_wider(names_from = pregnant, values_from = value) %>% 
  mutate(total = ifelse(is.na(pregnant), not_pregnant, pregnant + not_pregnant),
         rate = pregnant / total)

returns %>% pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, 
               names_to = "year", 
               values_to = "return", 
               names_transform = list(year = as.integer)) %>%
  arrange(year, half) %>%
  ggplot(aes(str_c(year, half, sep = "."), return, group = 1)) + 
  geom_line() +
  geom_point()


# Divide and conquer ------------------------------------------------------


# Separate ----------------------------------------------------------------

table3

# start
table3 %>% 
  separate(rate, into = c("cases", "population"))

# better
table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)


# Unite -------------------------------------------------------------------

table5

table5 %>% 
  unite(new, century, year)

table5 %>% 
  unite(new, century, year, sep = "") %>%
  mutate(year = as.integer(new)) %>%
  select(-new)


# 1.3 Exercise ------------------------------------------------------------

# What do the extra and fill arguments do in separate()? Experiment with the various options for the following two toy datasets.

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"))
