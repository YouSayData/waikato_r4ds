# minimal snowflake example with dplyr using flights and interbull data ####

library(tidyverse)
library(odbc)
library(dbplyr)
library(DBI)

library(tictoc) #speedtest package
library(nycflights13)

## Make connection to snowflake ####-------------------------------------

driver    <- "Trial" #The name in OBDC 64 bit connection, a user DSN that points to SnowflakeDSIIDriver
server    <- "" # host
DBUSERTRIAL  <- "" #part of credentials, best not to be in your script
#DBUSERTRIAL <- ""
database  <- "DEMO_DB"
schema    <- "PUBLIC"
warehouse <- "COMPUTE_TRIAL"

## To save credentials as environment variables, to save re-entry or coding into script
# usethis::edit_r_environ() # to open file (on local computer or on RStudio Workbench)
## then add lines like this (new line after end of file)
# DBUSERTRIAL = 'your_user_name'
# DBPWDTRIAL = 'put_password_here'


# odbc connection
# Need to install odbc driver first
# https://docs.snowflake.com/en/user-guide/odbc-windows.html
odbcCon <- DBI::dbConnect(odbc::odbc(),
                          driver, 
                          uid=DBUSERTRIAL, 
                          #pwd=rstudioapi::askForPassword("Please enter snowflake password"),
                          pwd=Sys.getenv("DBPWDTRIAL"),
                          database=database,
                          schema=schema,
                          warehouse=warehouse)

# Upload data (eg flights) to Snowflake ####-------------------------------------------
# flights <- flights
# dbWriteTable(odbcCon, "FLIGHTS", flights, overwrite = TRUE) #overwrite <-> DANGER! append = TRUE to append


# Get (Query) flights data from snowflake (using cloud compute) ####----------------------

flights_sf <- tbl(odbcCon,
                  "FLIGHTS")

## Speed test - flights ####----

tic("dplyr snowflake")
results_sf <- flights_sf %>% 
  group_by(dest) %>% 
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE)) 

results_sf
toc()

#flights <- flights

tic("dplyr local")
results <- flights %>% 
  group_by(dest) %>% 
  summarise(AVG_DELAY = mean(dep_delay, na.rm = TRUE)) 
results
toc()

# flights <- flights
# write_rds(flights, "flights.rds") #43mb
# write_csv(flights, "flights.csv") #30meg

# Your dplyr code is translated into this: 
results_sf %>% 
  show_query()


# Get (Query) Interbull data from snowflake (using cloud compute) ####---------------

#create snowflake object, which is a table
raw_holconf_long_sf <- tbl(odbcCon,
                  "raw_interbull_Holstein")

results <- raw_holconf_long_sf %>%
  group_by(V1) %>% 
  summarise(count = n()) 
as.data.frame(results)


#load existing data (in this case from RDS)
tic("read dataframe")
raw_holconf_long <- readRDS("raw_holconf_long.RDS")
toc()

dim(raw_holconf_long)

## Speed test (eg Interbull) ####-----------------------------------------
tic("dplyr snowflake")
results_sf <- raw_holconf_long_sf %>% 
  group_by(bull_int_id) %>% 
  summarise(Avg_BV = mean(V12, na.rm = TRUE)) 
results_sf
toc()

tic("dplyr local")
results <- raw_holconf_long %>% 
  group_by(bull_int_id) %>% 
  summarise(Avg_BV = mean(V12, na.rm = TRUE)) 
results
#as.data.frame(results)
toc()



# Load data for upload to snowflake (eg Interbull) ####------------------------------------

# #load existing data (in this case from RDS)
tic("read dataframe")
raw_holconf_long <- readRDS("raw_holconf_long.RDS")
toc()

# Upload data to snowflake  (eg Interbull) ####------------------------------------------------

tic("write table to snowflake")
dbWriteTable(odbcCon, "raw_interbull_Holstein", raw_holconf_long[1:200000,], overwrite = TRUE)
append = TRUE
toc()

n <- 2
tic("write table to snowflake")
dbWriteTable(odbcCon, "raw_interbull_Holstein", raw_holconf_long[(200000*(n-1)+1):(200000*n),], append = TRUE)
toc()

max_records <- dim(raw_holconf_long)[1]
max_records
max_records/200000 #over 22

tic("start loop")
for(n in 3:23){
  tic("write table to snowflake")
  dbWriteTable(odbcCon, "raw_interbull_Holstein",
               raw_holconf_long[(200000*(n-1)+1):min((200000*n), max_records),],
               append = TRUE)
  toc()
}
toc()
