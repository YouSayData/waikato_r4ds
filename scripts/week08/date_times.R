
# Why is working with dates tricky? ---------------------------------------

# 1. Does every year have 365 days?
# 2. Does every day have 24 hours?
# 3. Does every minute have 60 seconds?
# 4. What is 9am?


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(nycflights13)

# Date data types

# Date <date>
today() %>% typeof
today() %>% class
tibble(date = today())

as.Date("1970-01-01") %>% typeof
as.Date("1970-01-01") %>% class
tibble(date = c(today(),
                as.Date("1970-01-01")))

# Date-Time <dttm> aka POSIXct = seconds post 1 Jan 1970
# there is also POSIXlt which stores the date as a list

# generic functions

now() %>% weekdays
now() %>% months
now() %>% quarters
now() %>% year

class(Sys.time())
class(now())

list_date <- as.POSIXlt(Sys.time())
class(list_date)

names(unclass(list_date))
list_date$sec
list_date$zone
list_date$wday

# strptime
datestring <- c("January 10, 2012 10:40", "December 9, 2011 9:10")
strptime(datestring, "%B %d, %Y %H:%M")
?strptime


# Better lubridate --------------------------------------------------------

c("January 10, 2012", "December 9, 2011") %>%
  mdy

c("January 10, 2012 10:40", "December 9, 2011 9:10") %>%
  mdy_hm(tz = "NZ")

# Turn into timezones -----------------------------------------------------

now() %>% with_tz("CET")
now() %>% force_tz("CET")


# Create time from individual components ------------------------------------

flights %>% 
  select(year, month, day, hour, minute) %>%
  mutate(date = make_date(year, month, day),
            datetime = make_datetime(year, month, day, hour, minute))

flights %>% select(contains("time"))

# custom function

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

# switching between datetime and date -------------------------------------

as_datetime(today())
as_date(now())

# Exercises ---------------------------------------------------------------

# 1. What happens if you parse a string that contains invalid dates? E.g. c("2010-10-10", "bananas")
# 2. Use the appropriate lubridate function to parse those:

d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014


# Individual components from datetime -------------------------------------

now() %>% year
now() %>% month
now() %>% mday
now() %>% yday
now() %>% wday

now() %>% month(label = T, abbr = F)
now() %>% wday(label = T)

flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

flights %>%
  select(1:3, contains("dep_time")) %>%
  filter(is.na(dep_time)) %>%
  mutate(sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
         wday = wday(sched_dep_time, label = TRUE)) %>%
  ggplot(aes(x = wday)) +
  geom_bar()

flights %>%
  select(1:3, contains("dep_time")) %>%
  filter(is.na(dep_time)) %>%
  mutate(sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
         wday = wday(sched_dep_time, label = TRUE),
         date = date(sched_dep_time)) %>% 
  group_by(wday, date) %>%
  count %>%ggplot(aes(wday, n)) +
  geom_boxplot()

# interesting human pattern

# actual departure time ~ mean delay

flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line()


# scheduled departure time ~ mean delay

flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>%
  ggplot(aes(minute, avg_delay)) +
  geom_line()

flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>%
  ggplot(aes(minute, n)) +
  geom_line()


# Exercises ---------------------------------------------------------------


# 1. How does the distribution of flight times within a day change over the course of the year?
# 2. Compare air_time with the duration between the departure and arrival. Explain your findings.
# 3. Confirm the hypothesis that the early departures of flights in minutes 20-30 and 50-60 are caused by scheduled flights that leave early.


# Time spans --------------------------------------------------------------

# How old is Thomas?

t_age <- now() - ymd_hms(19810105190000)
t_age

class(t_age)

# Duration ----------------------------------------------------------------

as.duration(t_age)

dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)

2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)
tomorrow <- today() + ddays(1)
last_year <- now() - dyears(1)


# Periods -----------------------------------------------------------------

# proceed with care when mixing datetimes with duration

one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm 
one_pm + ddays(1)

# use periods

one_pm + days(1)

seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)

10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)

# let's correct an earlier mistake

flights_dt %>% 
  filter(arr_time < dep_time) 

flights_dt <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight),
    sched_arr_time = sched_arr_time + days(overnight)
  )

flights_dt %>% 
  filter(overnight, arr_time < dep_time) 


# Intervals ---------------------------------------------------------------

years(1) / days(1)

# use an interval

next_year <- today() + years(1)
(today() %--% next_year) / days(1)


# When to use what? -------------------------------------------------------

# If you only care about physical time, use a duration; 
# if you need to add human times, use a period; 
# if you need to figure out how long a span is in human units, use an interval.



# Exercises ---------------------------------------------------------------

# 1a. Create a vector of dates giving the first day of every month in 2015. 

first_day <- function(year) {
  first_day_of_year <- ymd(str_c(year,"01-01"))
  tibble(date = months(0:11) + first_day_of_year,
         weekday = weekdays(months(0:11) + first_day_of_year))
}
first_day(2015)

(ymd("2015-01-01") + months(0:11))
(ymd("2015-01-01") + months(0:11)) %>% weekdays
(ymd("2015-01-01") + months(0:11)) %>% wday(label = T)

make_date(2015,1:12,1)

# 1b. Create a vector of dates giving the first day of every month in the current year.
make_date(year(today()), 1:12, 1)
floor_date(today(), "year") + months(0:11)

# 2. Write a function that given a birthday of a person (as a date), returns how old the person is in years.
how_old <- function(birthday) {
  (year(today()) - year(birthday))
}
how_old("1999-11-1")

Howold <- function (birth) {
  print( ((ymd(birth) %--% today())/ years(1)) %>% floor) 
}

Howold("1999-11-1")


