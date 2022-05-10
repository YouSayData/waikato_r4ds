library(tidyverse)
library(ggthemes)
library(scales)
library(future)
library(furrr)
library(tictoc)

# Exercise ----------------------------------------------------------------

# 1. Write a map / loop that simulates 6 out of 49 lotto (without super ball).
# The sequence for the loop is 1:k where k is the number of weeks a player plays
# The function in the body of the loop simulates playing one field of lotto with random numbers.
# It returns a tibble containing a column for the week played and a column showing how many numbers the player had correct.
# Collect all the integers so you can plot them over the weeks played.

# 2. Run it for 100 weeks, bind the results to a tibble and visualise it.


# This doesn't scale well to higher k -------------------------------------


# furrr and future --------------------------------------------------------

benchmark_furrr <- function() {
  tic()
  nothingness <- future_map(rep(1, 7), ~Sys.sleep(.x))
  toc()
}

plan(sequential)
benchmark_furrr()

Num_workers <- availableCores() - 1 
plan(multisession, workers = Num_workers)
benchmark_furrr()

plan(sequential)

# For the sake of unity let's use this function from here -----------------

# Lotto function

lotto <- function(week) tibble(week = week,
                               correct = sample(1:49, 6) %in% sample(1:49, 6) %>% sum) 
# playing Lotto 

k <- 10000
weeks_played <- 1:k
lotto_results <- map(weeks_played, lotto)
lotto_results <- bind_rows(lotto_results)

# helping function to make the axis text nicer
ks <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000,
                                   suffix = "k",
                                   big.mark = ",")(x) }

lotto_results %>% 
  count(correct) %>%
  ggplot(aes(correct, n)) +
  geom_col() +
  scale_y_continuous(labels = ks) +
  labs(title = "Playing Lotto",
       subtitle = str_c("Numbers correct in\n", ks(k), " games")) +
  theme_clean() +
  theme(axis.title = element_blank())

# lotto_results <- map(weeks_played, lotto) is ok for a small amount of drawings, 
# what if we want to run it for 100,000 drawings?

# run a future map!

# Set a "plan" for how the code should run.
Num_workers <- availableCores() - 1 
plan(multisession, workers = Num_workers)
k <- 100000
weeks_played_2 <- 1:k
furrr_lotto_results <- future_map(weeks_played_2, lotto, 
                                  .options = furrr_options(seed = T))
furrr_lotto_results <- bind_rows(furrr_lotto_results) %>%
  arrange(week)
plan(sequential)

furrr_lotto_results %>% 
  count(correct) %>%
  ggplot(aes(correct, n)) +
  geom_col() +
  scale_y_continuous(labels = ks) +
  labs(title = "Playing Lotto",
       subtitle = str_c("Numbers correct in\n", ks(k), " games")) +
  theme_clean() +
  theme(axis.title = element_blank())

furrr_lotto_results %>% filter(correct == 5) %>% count

# all mapping functions can be replaced with a future_ mapping function
# if you run it in plan(sequential) they are basically the same


# future promises ---------------------------------------------------------

# 1
plan(sequential)

tic()
v1 <- {
  Sys.sleep(5)
  10
}
toc()
v1

# 2
Num_workers <- availableCores() - 1 
plan(multisession, workers = Num_workers)

tic()
v2 %<-% {
  Sys.sleep(5)
  20
}
v1
toc()

v2

# 3
tic()
v3 %<-% {
  Sys.sleep(5)
  20
}
v3
toc()

plan(sequential)


# Explicit vs. implicit ---------------------------------------------------

# implicit
v3 %<-% {
  Sys.sleep(5)
  20
}
v3

# explicit
f <- future(
  {
    Sys.sleep(5)
    20
  }
)

v3 <- value(f)