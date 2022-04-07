library(tidyverse) #for csv, tsv etc.
library(haven) # for sav, por, zsav
library(readxl)

# import
?read_csv
?read_csv2
?read_delim
?read_sav

readxl_example()
readxl_example("clippy.xls")
xlsx_example <- readxl_example("datasets.xlsx")
read_excel(xlsx_example)

readr_example("mtcars.csv")
read_csv(readr_example("mtcars.csv"))

read_csv(readr_example("mtcars.csv"), col_types = 
           cols(
             mpg = col_double(),
             cyl = col_integer(),
             disp = col_double(),
             hp = col_integer(),
             drat = col_double(),
             vs = col_integer(),
             wt = col_double(),
             qsec = col_double(),
             am = col_integer(),
             gear = col_integer(),
             carb = col_integer()
           )
)

# export
?write_csv
?write_excel_csv
?write_sav
