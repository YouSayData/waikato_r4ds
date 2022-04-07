# Libraries needed for this class -----------------------------------------

#install.packages(nycflights13)

library(nycflights13)
library(tidyverse)
library(here)

# alternatively to nycflights13 you can do:
# flights <- read_csv(here("scripts", "week02", "data", "flights.csv"))

# test
flights

# dplyr main functions ---------------------------------------------------------

# - Pick observations by their values (filter()). - done!
# - Reorder the rows (arrange()).
# - Pick variables by their names (select()).
# - Create new variables with functions of existing variables (mutate()).
# - Collapse many values down to a single summary (summarise()).
# - all can be used in conjunction with group_by()

# repeat in main group
# filter(): breakout group exercises --------------------------------------

# 1. Find all flights that
#   a. Had an arrival delay of two or more hours
#   b. Flew to Houston (IAH or HOU)
#   c. Were operated by United, American, or Delta
#   d. Departed in summer (July, August, and September)
#   e. Arrived more than two hours late, but didn’t leave late
(Arr_late <- filter(flights, arr_delay >120 & dep_delay <= 0))
filter(flights, arr_delay >120 & dep_delay <= 0)

#   f. Were delayed by at least an hour, but made up over 30 minutes in flight
(speedy <- filter(flights, dep_delay >= 60, (dep_delay - arr_delay) > 30))
filter(flights, dep_delay >= 60 & arr_delay < (dep_delay -30))

#   g. Departed between midnight and 6am (inclusive)
filter(flights, between(dep_time, 0, 600))
filter(flights, dep_time == 2400)

early_departure <- filter(flights, dep_time == 2400 | dep_time <= 600)
filter(flights, between(dep_time %% 2400, 0, 600))

# 2. Discuss: Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE & NA not missing? Can you figure out the general rule? (NA * 0 is a tricky counterexample!)

# arrange -----------------------------------------------------------------

arrange(flights, year, month, day)
arrange(flights, desc(dep_delay))
arrange(flights, -dep_delay)
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))


# arrange() main group exercises ------------------------------------------

# 1. How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
arrange(df, !is.na(x), x) # <- turns the logical test around
arrange(df, -is.na(x), x) # <- multiplies the logical test with -1
arrange(df, desc(is.na(x)), x) # <- changes the direction of the order

# 2. Sort flights to find the most delayed flights. Find the flights that left earliest.
arrange(flights, -dep_delay)
arrange(flights, -arr_delay)
arrange(flights, dep_time)

arrange(flights, -dep_delay) %>% glimpse
arrange(flights, -dep_delay) |> glimpse()

# 3. Sort flights to find the fastest flights.

flights %>% 
  mutate(fast_flights = (distance / air_time) * 60) %>% 
  arrange(-fast_flights) %>%
  glimpse

arrange(flights, air_time / distance)

# select() ------------------------------------------------------------------

# Select columns by name
select(flights, year, month, day)
# Select all columns between year and day (inclusive)
select(flights, year:day)
select(flights, 2,5,7)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))
select(flights, starts_with("d"))
select(flights, ends_with("r"))
select(flights, contains("ea"))
select(flights, num_range("x", 1:3))

# advanced: regular expression
select(flights, matches("(.)\\1"))

# you can use select to move columns by using the helper function everything()
select(flights, time_hour, air_time, everything())

# select() breakout group exercises ---------------------------------------
# 1. Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.
select(flights, matches("^(dep|arr).*")) %>% glimpse # Choice 1
select(flights, "dep_time", "dep_delay", "arr_time", "arr_delay")
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, c(4, 6, 7, 9))
select(flights, starts_with(c("dep","arr"))) # Choice 5
select(flights, -c(1:3, 5, 8, 10:19))%>% glimpse() # Choice 2
select(flights, ends_with("time") | ends_with("delay")) %>% # Choice 3
  select(!starts_with("sched") & !starts_with("air"))

select(flights, ends_with("_delay") |  # Choice 4
         ends_with("_time") & 
         !(contains("sched") | contains("air")))

# mutate() ----------------------------------------------------------------

# smaller tibble
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)

# create new columns
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)

# use columns you created in the same call
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)

# use transmute to only keep created columns
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

# math operation with fixed numbers use recylcing
# +, -, *, /, ^
# x / sum(x) proportion
# y - mean(y) difference from mean
# integer division %/% and remainder %%

transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

# logs: log(), log2(), log10()
# leading and lagging
(x <- 1:10)
lag(x)
lead(x)

(examplA <- c(22, 26, 30, 34, 38, 42))
examplA - lag(examplA)

# cummulative functions
cumsum(x)
cummean(x)
cumprod(x)
cummin(x)
cummax(x)


# mutate(): breakout group exercises --------------------------------------

# 1. Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they’re not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.

delay_mins <- mutate(flights, dep_time,
                     dep_time = dep_time %% 2400,
                     dhour = dep_time %/% 100,
                     dminute = dep_time %% 100,
                     dep_time_mins = (dhour * 60) + dminute
) %>% glimpse

delays <- mutate(delay_mins, sched_dep_time,
                 sched_dep_time = sched_dep_time %% 2400,
                 shour = sched_dep_time %/% 100,
                 sminute = sched_dep_time %% 100,
                 sched_dep_mins = (shour * 60) + sminute
) %>% glimpse

select(delays, dep_time_mins, sched_dep_mins)

flights %>% mutate(dep_time = dep_time %% 2400,
                   sched_dep_time = sched_dep_time %% 2400,
                   dep_time_mins = (dep_time %/% 100) * 60 + dep_time %% 100,
                   sched_dep_mins = (sched_dep_time %/% 100) * 60 + sched_dep_time %% 100
) %>% glimpse

# summarise() & group_by() ------------------------------------------------

# summarise
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
mean(flights$dep_delay, na.rm=TRUE)

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

summarise(by_day, count = n()) %>%
  summarise(count = sum(count)) %>% 
  summarise(count = sum(count))

flights %>% count


# to pipe or not to pipe --------------------------------------------------

# no pipe
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# pipe

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

ggplot(data = delays, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL") %>%
  ggplot(aes(dist, delay)) + geom_point(aes(size = count), alpha = 1/3) + geom_smooth(se = F)


