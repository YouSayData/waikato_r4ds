library(tidyverse)

# Recap Exercise I: Cleaning --------------------------------------------------

# 1. Read the file measurements.csv (https://education.rstudio.com/blog/2020/08/more-example-exams/measurements.csv)
# to create a tibble called measurements.
# (The strings "rad", "sal", and "temp" in the quantity column stand for “radiation”, “salinity”, and “temperature” respectively.)
#

measurements <- read_csv("https://education.rstudio.com/blog/2020/08/more-example-exams/measurements.csv")

measurements_df <- read.csv("https://education.rstudio.com/blog/2020/08/more-example-exams/measurements.csv")

measurements_mem_efficient <-
  read_csv("https://education.rstudio.com/blog/2020/08/more-example-exams/measurements.csv",
    col_types =
      cols(
        visit_id = col_integer()
      )
  )
measurements_mem_efficient %>% glimpse()
measurements %>% glimpse()


# 2. Create a tibble containing only rows where none of the values are NA and save in a tibble called cleaned.
#

measurements %>% map_lgl(.f = function(x) {
  any(is.na(x))
})

cleaned <- measurements %>%
  filter(
    !is.na(reading),
    !is.na(visit_id)
  )

cleaned <- na.omit(measurements)
cleaned <- measurements %>%
  drop_na()


# 3. Count the number of measurements of each type of quantity in cleaned.
# Your result should have one row for each quantity "rad", "sal", and "temp".
cleaned %>%
  count(quantity)

(cleaned1 <- cleaned %>%
  group_by(quantity) %>%
  summarise(Number_of_measurements = n()))

cleaned1

#
# 4. Display the minimum and maximum value of reading separately for each quantity in cleaned. Your result should have one row for each quantity "rad", "sal", and "temp".
#

cleaned %>%
  group_by(quantity) %>%
  summarise(
    maximum = max(reading),
    minimum = min(reading)
  )

(SumCleaned <- cleaned %>%
  group_by(quantity) %>%
  summarise(
    NObs = n(),
    MinValue = min(reading),
    MaxValue = max(reading)
  ))

# 5. Create a tibble in which all salinity ("sal") readings greater than 1 are divided by 100.
# (This is needed because some people wrote percentages as numbers from 0.0 to 1.0, but others wrote them as 0.0 to 100.0.)

(fixed <- cleaned %>%
  mutate(reading = if_else((quantity == "sal") & (reading > 1),
    reading / 100,
    reading
  )))

fixed %>%
  group_by(quantity) %>%
  summarise(min(reading), max(reading))

cleanedsal <- cleaned %>%
  filter(quantity == "sal") %>%
  filter(reading > 1) %>%
  mutate(reading = reading / 100)

remainder <- cleaned %>%
  filter(quantity != "sal" | quantity == "sal" & reading <= 1)

fixed_2 <- bind_rows(cleanedsal, remainder)


cleaned_pivot_b <- pivot_wider(cleaned, names_from = quantity, values_from = reading) %>%
  mutate(sal = ifelse(sal > 1, sal / 100, sal)) %>%
  pivot_longer(rad:temp, names_to = "quantity", values_to = "reading") %>%
  drop_na()

cleaned_pivot_b_2 <- pivot_wider(cleaned, names_from = quantity, values_from = reading) %>%
  mutate(sal = ifelse(sal > 1, sal / 100, sal)) %>%
  pivot_longer(c(rad, sal, temp), names_to = "quantity", values_to = "reading") %>%
  na.omit()

# Recap Exercise II: Functions ---------------------------------------------

# 1. Read the file person.csv (https://education.rstudio.com/blog/2020/08/more-example-exams/person.csv) and store the result in a tibble called person.
#
person <- read_csv("https://education.rstudio.com/blog/2020/08/more-example-exams/person.csv")


# 2. Write a function called summarize_table that takes a title string and a tibble as input and returns a string that says something like,
# “title has # rows and # columns”. For example, summarize_table('our table', person) should return the string "our table has 5 rows and 3 columns".
#

summarise_table <- function(title, data) {
  numbercols <- ncol(data)
  numberrows <- nrow(data) 
  print(paste(title, "has", numberrows, "rows and", numbercols, "columns"))}

summarise_table("person", person)


summarize_table <- function(title, tibble) {
  dimensions <- dim(tibble)
  str_c(title, "has", dimensions[1], "rows and", 
        dimensions[2], "columns", sep = " ")
}

summarize_table("our table", person)

summarize_table <- function(table_name, x){
  row <- nrow(x)
  col <- ncol(x)
  str_c(table_name, "has", row, "rows and", col, "columns", sep = " ")     
}

summarize_table("AWESOMETABLE", person)

# 3. Write another function called show_columns that takes a string and a tibble as input and returns a string that says something like, “table has columns name, name, name".
# For example, show_columns('person', person) should return the string "person has columns person_id, personal_name, family_name".

show_columns <- function(title_str, data) {
  if (!is_tibble(data)) {
    stop("show_columns() expects a tibble as input")
    }
  
  return(str_c(title_str, 
               "has columns", 
               str_c(names(data), collapse  = ", "), 
               sep = " "))
}

show_columns("person", person)

# Test error condition
show_columns("person", c(a = "1", b = "2"))


show_columns <- function(str, tib) {
  colnames <- colnames(tib)
  print(paste(str, "has columns", str_c(names(tib), collapse  = ", ")))
}
show_columns("person", person)

# Recap Exercise III: Tidy Data ----------------------------------------

# You want to tidy "https://education.rstudio.com/blog/2020/02/instructor-certification-exams/infant_hiv.csv")

# a. The first column is ISO3 country codes.
# b. There are three columns for each year from 2009 to 2017. Each set has estimated, low, and high values for the year (in that order).
# c. A dash - indicates that no data is available.
# d. Our analyst tells us that >95% means “the data is unreliable”.

# Your task is to turn this into a tidy data table for further analysis:

# 1. Discuss what columns a tidy layout for this data would have and why.
# 2. Write a function that takes the link to the file containing this table as input and returns a tidy version of the table:
# 2.1 The function should replace all - and >95% values with NA.
# 2.2 The body of the function may contain one or more pipelines and may create temporary or intermediate variables,
# but may not contain any loops.


# Recap Exercise IV: Descriptive Modelling --------------------------------

# The file `toronto-covid-2021-01-04.csv` contains reports of COVID-19 cases in Toronto.

# 1. Load this file to create a tibble called `covid`.
# 2. Construct a model showing how well the number of cases reported in one month predicts the number of cases reported in the next month.
# Hint: (You may find the `lag()` function useful.)
# Hint: If your goal is descriptive modelling, the solution can be a plot!