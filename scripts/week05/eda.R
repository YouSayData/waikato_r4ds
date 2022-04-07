
# Introduction to EDA -----------------------------------------------------


# Exploratory Data Analysis (EDA) is an iterative cycle:
# 1. Generate questions about your data.
# 2. Search for answers by visualising, transforming, and modelling your data.
# 3. Use what you learn to refine your questions and/or generate new questions.

# It's not a formal process though. 
# It's a creative process.

# “There are no routine statistical questions, only questionable statistical routines.” — Sir David Cox
# Although two questions are very often useful:
# 1. What type of variation occurs within my variables?
# 2. What type of covariation occurs between my variables?
# Variation is the tendency of the values of a variable to change from measurement to measurement. 
# If variation describes the behavior within a variable, covariation describes the behavior between variables. 
# Covariation is the tendency for the values of two or more variables to vary together in a related way.

# Some defs
# A variable is a quantity, quality, or property that you can measure.
# A value is the state of a variable when you measure it. The value of a variable may change from measurement to measurement.
# An observation is a set of measurements made under similar conditions (you usually make all of the measurements in an observation at the same time and on the same object). 
# Tabular data is a set of values, each associated with a variable and an observation.

# Libraries Used ----------------------------------------------------------

library(tidyverse)

# optional
# install.packages("viridis")
library(viridis) # for accessibility
library(ggthemes)
library(hexbin)

# maybe
library(modelr)

# Variation of 1 variable -------------------------------------------------

# Discrete variables: Distribution ------------------------------------

?diamonds

# Observe via plot
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

# Observe via table
diamonds %>% 
  count(cut) %>%
  rename(count = n) %>%
  mutate(perc = count / sum(count))

# Continuous variables: Distribution --------------------------------------

# via plot
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# via table
diamonds %>% 
  count(cut_width(carat, 0.5))

# explore different bins 
smaller <- diamonds %>% 
  filter(carat < 3)

bigger <- diamonds %>% 
  filter(carat >= 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)


# Optional: ggthemes provides nice themes --------------------------------

ggplot(data = smaller) +
  theme_tufte() +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.1) + 
  theme(axis.title=element_blank()) +
  annotate("text", x = 2.3, y = 7500, adj=1,  family="serif", 
           label = c("Count of diamonds per\ncarat (binwidth = 0.1)."))


# Return to distribution of continuous variables --------------------------

smaller %>% 
  count(cut_width(carat, 0.01))

# by type
ggplot(data = smaller, 
       mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

# Follow up questions,  one should ask ------------------------------------
# Which values are the most common? Why?
# Which values are rare? Why? Does that match your expectations?
# Can you see any unusual patterns? What might explain them?

# Let's have a look

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

# Why are there more diamonds at whole carats and very common fractions of carats?
# Why are there more diamonds slightly to the right of each peak than there are slightly to the left of each peak?
# Why are there no diamonds bigger than 3 carats?

ggplot(data = bigger, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

# Clusters of similar values suggest that subgroups exist in your data.
# How are the observations within each cluster similar to each other?
# How are the observations in separate clusters different from each other?
# How can you explain or describe the clusters?
# Why might the appearance of clusters be misleading?

# Let's have a look at another example
?faithful
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25) +
  xlab("Duration of Eruptions")

# maybe the duration of the eruption is affected by another variable?


# Outliers ----------------------------------------------------------------

# what is suspicious here?
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

# maybe we can change the viewpoint
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual

# Repeat later experiments with and without outliers
# If outliers have minimal effect on the results, and you can’t figure out why they’re there, it’s reasonable to replace them with missing values, and move on. 
# If they have a substantial effect on your results, you shouldn’t drop them without justification. You’ll need to figure out what caused them (e.g. a data entry error) and disclose that you removed them in your write-up.


# Exercises I -------------------------------------------------------------

# 1. Explore the distribution of each of the x, y, and z variables in diamonds. What do you learn? Think about a diamond and how you might decide which dimension is the length, width, and depth.
ggplot(data = diamonds, mapping = aes(x = z)) +
  geom_histogram(binwidth = 0.01) +
  coord_cartesian(xlim = c(3, 9))


diamonds %>% ggplot(aes(x,y)) + geom_point() + coord_fixed()

# 2. Explore the distribution of price. Do you discover anything unusual or surprising? (Hint: Carefully think about the binwidth and make sure you try a wide range of values.)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = price), binwidth = 10) +
  coord_cartesian(xlim = c(500, 2000))

ggplot(data = diamonds, mapping = aes(x = price, fill = color)) + 
  geom_histogram (binwidth = 2500, position = "dodge")

# 3. How many diamonds are 0.99 carat? How many are 1 carat? What do you think is the cause of the difference?
# 4. Compare and contrast coord_cartesian() vs xlim() or ylim() when zooming in on a histogram. What happens if you leave binwidth unset? What happens if you try and zoom so only half a bar shows?



# Dealing with outliers ---------------------------------------------------
# I decided to drop the unusual values, how can I do this?

# 1. Really drop them.
diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

# 2. NA them
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)


# Reason for NAs ----------------------------------------------------------

# often there is a reason for na, which we can use
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_density(mapping = aes(fill = cancelled), alpha = .3)

cancelledFlights <- nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time) * 1,
    not_cancelled = !is.na(dep_time) * 1,
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60,
    quarterlies = cut(sched_dep_time,
                      breaks = seq(from = 0, to = 24, .5),
                      labels = seq(from = .5, to = 24, .5))) %>%
  group_by(quarterlies) %>%
  summarise(cancelled = sum(cancelled),
            not_cancelled = sum(not_cancelled)) %>%
  mutate(count = cancelled + not_cancelled,
         per_cancelled = cancelled / count) %>% 
  filter(per_cancelled != 1)

cancelledFlights %>%
  ggplot(aes(x = quarterlies)) +
  geom_col(aes(y = count)) +
  geom_line(aes(y = per_cancelled * max(count), group = 1)) +
  scale_y_continuous(
    sec.axis = sec_axis(~./max(cancelledFlights$count))
  )



# Exercise II -------------------------------------------------------------

# 1. What happens to missing values in a histogram? What happens to missing values in a bar chart? Why is there a difference?
# 2. What does na.rm = TRUE do in mean() and sum()?


# Covariation -------------------------------------------------------------

# Categorical and continuous variable
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# Too much difference in the counts of the groups
ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

# If there is too much difference in the distribution geom_density might be better
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_density(mapping = aes(colour = cut))

# Alternative boxplot
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

# Alternative violin
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_violin()

# Alternative random-scatter
ggplot(data = diamonds %>% slice(sample(1:nrow(diamonds), 10000)), mapping = aes(x = cut, y = price)) +
  geom_jitter(alpha = .2)

#install.packages("ggbeeswarm")
library(ggbeeswarm)
ggplot(data = diamonds, mapping = aes(x = cut, y = price, col = cut)) +
  geom_quasirandom()

ggplot(data = diamonds %>% filter(cut %in% c("Premium", "Ideal")) %>% slice(sample(1:nrow(diamonds), 250)), mapping = aes(x = cut, y = price)) +
  geom_beeswarm(size = 1)

ggplot(data = diamonds) + 
  theme_tufte() +
  geom_tufteboxplot(mapping = aes(x = cut, y = price)) + 
  theme(axis.title=element_blank()) +
  annotate("text", x = 7, y = 16000, adj=1,  family="serif", 
           label = c("Price per\nquality of cut\nin Diamonds."))

# why are better cut diamonds cheaper?!?
diamonds$cut[1]
# you will have to answer that question, but first let's use order to get the hang of it

# using order

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, median), y = hwy))

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = fct_reorder(class, hwy, median), y = hwy))

ggplot(data = diamonds) + 
  theme_tufte() +
  geom_tufteboxplot(mapping = aes(x = reorder(cut, price, FUN = mean), y = price)) + 
  theme(axis.title=element_blank()) +
  annotate("text", x = 7, y = 16000, adj=1,  family="serif", 
           label = c("Price per\nquality of cut\nin Diamonds."))

ggplot(data = diamonds) + 
  theme_tufte() +
  geom_tufteboxplot(mapping = aes(x = reorder(cut, price, FUN = median), y = price)) + 
  theme(axis.title=element_blank()) +
  annotate("text", x = 7, y = 16000, adj=1,  family="serif", 
           label = c("Price per\nquality of cut\nin Diamonds."))

# long variable names work better flipped
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(y = reorder(class, hwy, FUN = median), x = hwy))


# Exercise III ------------------------------------------------------------
# 1. Use what you’ve learned to improve the visualisation of the departure times of cancelled vs. non-cancelled flights.
# 2. What variable in the diamonds dataset is most important for predicting the price of a diamond? How is that variable correlated with cut? Why does the combination of those two relationships lead to lower quality diamonds being more expensive?

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(aes(sched_dep_time)) + 
  geom_density(aes(fill = cancelled), alpha = 1/5)


# Two categorical values --------------------------------------------------

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>% 
  count(color, cut)

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))


# Exercise IV -------------------------------------------------------------
# 1. How could you rescale the count dataset above to more clearly show the distribution of cut within colour, or colour within cut?
# 2. Use geom_tile() together with dplyr to explore how average flight delays vary by destination and month of year. What makes the plot difficult to read? How could you improve it?
# 3. Why is it slightly better to use aes(x = color, y = cut) rather than aes(x = cut, y = color) in the example above?

# Two continuous variables
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

# changing alpha can help overplotting
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)

# other options

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = smaller) +
  geom_density2d(mapping = aes(x = carat, y = price))

ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_violin(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

# Exercise V --------------------------------------------------------------
# 1. Instead of summarising the conditional distribution with a boxplot, you could use a frequency polygon. What do you need to consider when using cut_width() vs cut_number()? How does that impact a visualisation of the 2d distribution of carat and price?
# 2. Visualise the distribution of carat, partitioned by price.
# 3. How does the price distribution of very large diamonds compare to small diamonds? Is it as you expect, or does it surprise you?
# 4. Combine two of the techniques you’ve learned to visualise the combined distribution of cut, carat, and price.
# 5. Two dimensional plots reveal outliers that are not visible in one dimensional plots. For example, some points in the plot below have an unusual combination of x and y values, which makes the points outliers even though their x and y values appear normal when examined separately. 
# Why is a scatterplot a better display than a binned plot for this case?

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))


# Patterns and Models -----------------------------------------------------

# Patterns in your data provide clues about relationships. If a systematic relationship exists between two variables it will appear as a pattern in the data. If you spot a pattern, ask yourself:
# Could this pattern be due to coincidence (i.e. random chance)?
# How can you describe the relationship implied by the pattern?
# How strong is the relationship implied by the pattern?
# What other variables might affect the relationship?
# Does the relationship change if you look at individual subgroups of the data?

ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

# Models are a tool for extracting patterns out of data

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))

ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))