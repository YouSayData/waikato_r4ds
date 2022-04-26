# Basic Data Analysis Assessment (Assessment II)

# Task

Using the `tidyverse` and optionally any other library, analyse [NZ's baby names](https://catalogue.data.govt.nz/dataset/01ee87cd-ecf8-44a1-ad33-b376a689e597/resource/0b0b326c-d720-480f-8f86-bf2d221c7d3f/download/baby-names-2022-01-07.csv) across time. 

The data set is provided by DIA via data.govt.nz: https://catalogue.data.govt.nz/dataset/baby-name-popularity-over-time/resource/0b0b326c-d720-480f-8f86-bf2d221c7d3f

The data set includes a binary representation of a person's sex at birth.

## Tasks

All task are 16.5% each. 1% is given for the submission itself.

1. Import the data and call it `babynames`. What is the earliest data point, what is the latest?
2. Plot the usage of your own name or a name you like over time.
3. Create two new columns in `babynames`: one for the century and one for the decade. The `Decade` column and the `Century` column should be contain 4-digit integers (e.g. `1990` and `1900`).
4. Find out either graphically or via a table how many different baby names existed for each decade for each sex.
5. Find names that can be used for both sexes in the data set. Of those take 5 random names and plot their use over time for each sex. Briefly describe what you can observe.
6. Find the most common name in each decade for each sex.
