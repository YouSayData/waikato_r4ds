library(tidyverse)
library(here)
library(arrow)
library(tictoc)

# check version
packageVersion("arrow")

# Use the penguins data set
data(penguins, package = "palmerpenguins")


# Parquet Files -----------------------------------------------------------

# Create a temporary files for the output
parquet <- tempfile(fileext = ".parquet")
on.exit(unlink(parquet))

csv <- tempfile(fileext = ".csv")
on.exit(unlink(csv))

feather <- tempfile(fileext = ".feather")
on.exit(unlink(feather))

# Your first Arrow function
write_parquet(penguins, sink = parquet)

# If you want to have an actual file
write_parquet(penguins, sink = here("scripts", "week13", "penguins_compressed.parquet"))

larger_tbl <- tibble(x = rep("A", 1e6))

arrow::write_parquet(larger_tbl, sink = parquet, compression = "uncompressed")
readr::write_csv(larger_tbl, file = csv)

# Could also use file.info()
object.size(larger_tbl) %>% print(units = "Mb")
fs::file_info(c(parquet, csv)) %>% select(path, size)


# Even larger file --------------------------------------------------------

even_larger_tbl <- tibble(x = rep("R4DS Waikato", 1e6))

arrow::write_parquet(even_larger_tbl, sink = parquet, compression = "uncompressed")
readr::write_csv(even_larger_tbl, file = csv)

# Could also use file.info()
object.size(even_larger_tbl) %>% print(units = "Mb")
fs::file_info(c(parquet, csv)) %>% select(path, size)

# And the penguins? -------------------------------------------------------

arrow::write_parquet(penguins, sink = parquet, compression = "uncompressed")
readr::write_csv(penguins, file = csv)

# Could also use file.info()
fs::file_info(c(parquet, csv)) %>% select(path, size)

# parquet encoding tricks:
# https://github.com/apache/parquet-format/blob/master/Encodings.md

# Feather files -----------------------------------------------------------

arrow::write_parquet(even_larger_tbl, sink = parquet)
arrow::write_feather(even_larger_tbl, sink = feather)
readr::write_csv(even_larger_tbl, file = csv)

# Could also use file.info()
object.size(even_larger_tbl) %>% print(units = "Mb")
fs::file_info(c(parquet, feather, csv)) %>% select(path, size)


# Using Feather files -----------------------------------------------------

arrow::write_feather(penguins, sink = feather)
(penguins_tbl <- read_feather(feather))
(penguins_feather <- read_feather(feather, as_data_frame = F))

penguins_feather$num_columns
penguins_feather$num_rows
penguins_feather$schema

penguins_feather$columns[[1]]


# Computation -------------------------------------------------------------

call_function(function_name = "mean", 
              penguins_feather$flipper_length_mm)

penguins$flipper_length_mm %>% mean(na.rm = T)
