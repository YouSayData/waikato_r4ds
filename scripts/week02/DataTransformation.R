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

# - Pick observations by their values (filter()).
# - Reorder the rows (arrange()).
# - Pick variables by their names (select()).
# - Create new variables with functions of existing variables (mutate()).
# - Collapse many values down to a single summary (summarise()).
# - all can be used in conjunction with group_by()

# filter() ----------------------------------------------------------------

filter(flights, month == 1, day == 1)

# if you want it saved
jan1 <- filter(flights, month == 1, day == 1)

# what is happening here?
(dec25 <- filter(flights, month == 12, day == 25))

# why is this wrong?
filter(flights, month = 1)

# ==, !=, <, >, <=, >=
# but be careful with == with doubles
# the computer attempts to be too precise for some math...
sqrt(2) ^ 2 == 2

# use near instead
near(sqrt(2) ^ 2,  2)

# boolean operations
# x, !x, x&y, x|y, xor(x,y)

# this works
filter(flights, month == 11 | month == 12)
# this does not
filter(flights, month == 11 | 12)

# this works again
nov_dec <- filter(flights, month %in% c(11, 12))

# this produces the same results
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

# missing values are contagious
NA > 5
10 == NA
NA + 10
NA / 2
NA == NA

# Let x be Mary's age. We don't know how old she is.
x <- NA
# Let y be John's age. We don't know how old he is.
y <- NA

# Are John and Mary the same age?
x == y
# We don't know!
is.na(x)

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

# filter(): breakout group exercises --------------------------------------

# 1. Find all flights that
#   a. Had an arrival delay of two or more hours
#   b. Flew to Houston (IAH or HOU)
#   c. Were operated by United, American, or Delta
#   d. Departed in summer (July, August, and September)
#   e. Arrived more than two hours late, but didn’t leave late
#   f. Were delayed by at least an hour, but made up over 30 minutes in flight
#   g. Departed between midnight and 6am (inclusive)
# 2. Discuss: Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE & NA not missing? Can you figure out the general rule? (NA * 0 is a tricky counterexample!)

# arrange -----------------------------------------------------------------

arrange(flights, year, month, day)
arrange(flights, desc(dep_delay))
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))


# arrange() main group exercises ------------------------------------------

# 1. How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
# 2. Sort flights to find the most delayed flights. Find the flights that left earliest.
# 3. Sort flights to find the fastest flights.

# select() ------------------------------------------------------------------

# Select columns by name
select(flights, year, month, day)
# Select all columns between year and day (inclusive)
select(flights, year:day)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))
select(flights, starts_with("d"))
select(flights, ends_with("r"))
select(flights, contains("ea"))
select(flights, matches("(.)\\1"))
select(flights, num_range("x", 1:3))

# you can use select to move columns by using the helper function everything()
select(flights, time_hour, air_time, everything())

# select() breakout group exercises ---------------------------------------
# 1. Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.


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


# summarise() & group_by() ------------------------------------------------

# summarise
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
mean(flights$dep_delay, na.rm=TRUE)

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

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
