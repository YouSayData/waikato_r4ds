
# install.packages("sf")
# install.packages("terra")
# install.packages("spData")
# install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")

# sf provides classes for geographic vector data and enables import of geospatial computing libraries
library(sf)
library(terra)
library(spData)
library(spDataLarge)  
library(tidyverse)

# Vector Data Model 
# Points, Lines, Polygons
# Social Sciences etc.

# Raster Data Model
# Environmental Science etc.

# it's possible to use them together and to convert in either direction


# Vector Data -------------------------------------------------------------

# needs CRS (Coordinate Reference System
# SF Libraries

# GDAL
# PROJ
# GEOS
# S2

vignette("sf1") 

# let's try

class(world)
plot(world)
summary(world["lifeExp"])
plot(world["lifeExp"])

world %>% 
  filter(name_long %in% c("New Zealand", "Australia")) %>% 
  select(gdpPercap, lifeExp) %>% 
  plot

world %>% 
  filter(name_long %in% c("New Zealand", "Australia")) %>% 
  select(gdpPercap, lifeExp)


# spData, shape files, plotting ------------------------------------------------

system.file("shapes/world.shp", package = "spData")

world_dfr <- system.file("shapes/world.shp", package = "spData") %>% 
  st_read

world_asia <- world %>%
  filter(continent == "Asia")
asia <- st_union(world_asia)

# layers
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

# further adding
plot(world["continent"], reset = FALSE)
gpd_per_capita_scaled <- world$gdpPercap / 50000
plot(st_geometry(world_cents), add = TRUE, cex = gpd_per_capita_scaled)

# plotting in context
new_zealand <- world %>%
  filter(name_long == "New Zealand")

world_oceania <- world %>%
  filter(continent == "Oceania")

# not completely layered
plot(st_geometry(new_zealand), expandBB = c(2.5, 0.1, 2, 0.1), col = "gray", lwd = 3, reset = T)
plot(st_geometry(world_oceania), add = TRUE)


new_zealand %>% select(geom)

# WKB / WKT

# sf in R supports 7 core simple feature geometry types

# point
# linestring
# polygon
# multipoint
# multilinestring
# multipolygon
# geometry collection

# sf consist of
# sfc
# and data (tibble, df)

oamaru_point <- st_point(c(-45.09, 170.97)) # sfg object
hamilton_point <- st_point(c(-37.78, 175.25)) 

oamaru_geom <- st_sfc(oamaru_point, crs = 4326) # sfc object
hamilton_geom <- st_sfc(hamilton_point, crs = 4326)

# crs is what makes this possible
st_area(new_zealand)
st_area(new_zealand) / 1000^2

oamaru_attr <- tibble(name = "Oamaru",
                      pop_peng = 3000,
                      pop = 14000,
                      pen_per_cap = pop_peng / pop)

hamilton_attr <- tibble(name = "Hamilton",
                      pop_peng = 0,
                      pop = 165000,
                      pen_per_cap = pop_peng / pop)

oamaru_sf = st_sf(oamaru_attr, geometry = oamaru_geom)    # sf object
hamilton_sf = st_sf(hamilton_attr, geometry = hamilton_geom)

both_sf <- bind_rows(oamaru_sf,
                     hamilton_sf)

st_crs(both_sf)



# raster data -------------------------------------------------------------

raster_filepath <- system.file("raster/nz_elev.tif", package = "spDataLarge")

nz_elev <- rast(raster_filepath)
class(nz_elev)

plot(nz_elev)

crs(nz_elev) %>% 
  cat

nz_elev %>%
  as.data.frame(xy = T) %>%
  na.omit %>%
  ggplot(aes(x, y, col = elevation)) +
  geom_point()

nz_elev %>%
  as.data.frame(xy = T) %>%
  na.omit %>%
  ggplot(aes(x, y, fill = elevation)) +
  geom_raster()

# Exercises ---------------------------------------------------------------

# 1. Use summary() on the geometry column of the world data object that is included in the spData package. 
# What does the output tell us about:

# Its geometry type?
#  The number of countries?
#  Its coordinate reference system (CRS)? 

# 2. What does the cex argument do in plot()

# 3. Check the CRS of the raster/nlcd.tif file from the spDataLarge package. 
# What kind of information you can learn from it? 

# 

