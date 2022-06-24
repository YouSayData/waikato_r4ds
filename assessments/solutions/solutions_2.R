library(tidyverse)
library(lubridate)
library(ggthemes)

# they changed the link since the assignment


# 1 -----------------------------------------------------------------------


babynames <- read_csv("https://catalogue.data.govt.nz/datastore/dump/0b0b326c-d720-480f-8f86-bf2d221c7d3f?bom=True")
closeAllConnections()


# 2 -----------------------------------------------------------------------


babynames %>% 
  filter(Name == "Thomas") %>%
  ggplot(aes(Year, Count)) +
  geom_col() +
  theme_minimal()


# 3 -----------------------------------------------------------------------


babynames %>%
  mutate(Decade = (Year %/% 10) * 10, 
         Century = (Year %/% 100) * 100) 
  

# 4 -----------------------------------------------------------------------

babynames %>%
  mutate(Decade = (Year %/% 10) * 10) %>%
  select(Decade, Sex, Name) %>%
  distinct %>%
  count(Decade, Sex) %>%
  ggplot(aes(Decade, n, fill = Sex)) +
  geom_col(position = "dodge") +
  theme_minimal()


# 5 -----------------------------------------------------------------------

male_names <- babynames %>% 
  filter(Sex == "M") %>%
  pull(Name) %>%
  unique

female_names <- babynames %>% 
  filter(Sex == "F") %>%
  pull(Name) %>%
  unique

not_gendered_names <- male_names[male_names %in% female_names]

# Truly random
babynames %>% 
  filter(Name %in% sample(not_gendered_names, 5)) %>%
  mutate(Decade = (Year %/% 10) * 10) %>%
  group_by(Decade, Sex, Name) %>%
  summarise(use = sum(Count, na.rm =T)) %>%
  ggplot(aes(Decade, use, fill = Sex)) +
  geom_col(position = "dodge") +
  facet_wrap(~Name, scales = "free_y") +
  theme_minimal() 

# then write down, whatever you observed. The trends differ from name to name.
# Vivian for instance shows a clear change.


# 6 -----------------------------------------------------------------------

babynames %>% 
  mutate(Decade = (Year %/% 10) * 10) %>%
  group_by(Decade, Sex, Name) %>%
  summarise(use = sum(Count, na.rm =T)) %>% 
  filter(use == max(use)) %>% View

