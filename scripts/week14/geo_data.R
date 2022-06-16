# Libraries needed --------------------------------------------------------

library(tidyverse)
library(osmdata)
library(sf)
library(ggthemes)
library(showtext)
font_add_google(name = "Montserrat")
showtext_auto()

# Helping Functions -------------------------------------------------------

# functions modified from osmplotr

sf_land <- function(osmd, bbox) {
  ## osmd is a coastline query, and could contain
  ## both lines and polygons.
  ## Lines need to be merged and converted to polygons.
  ## Both need to be clipped against the bounding box
  bbxcoords <- rbind(c(bbox[1, 2], bbox[2, 2]),
                     c(bbox[1, 2], bbox[2, 1]),
                     c(bbox[1, 1], bbox[2, 1]),
                     c(bbox[1, 1], bbox[2, 2])
  )
  bbxcoords <- rbind(bbxcoords, bbxcoords[1,])
  bbx <- st_sfc(st_cast(st_multipoint(bbxcoords), "POLYGON"), crs=st_crs(osmd$osm_lines))
  ## merge lines
  if (!is.null(osmd$osm_lines)) {
    m1 <- osmd$osm_lines
    if (nrow(m1) > 1) {
      if (inherits(m1, "sfc_MULTILINESTRING")) {
        m1 <- st_cast(st_line_merge(st_union(m1)), "LINESTRING")
      } else {
        m1 <- st_cast(st_union(m1), "LINESTRING")
      }
    }
    m2 <- st_polygonize(m1)
    k <- st_dimension(m2)
    ## initialise some empty geometries, like a NULL
    closed <- res <- islands <- st_sfc(st_geometrycollection(), crs=st_crs(osmd$osm_lines))
    if (!(all(st_is_empty(m2)))) {
      ## Cast to polygon - warn=FALSE stops complaints when there aren't any polygons
      islands <- st_cast(m2, warn=FALSE)
    }
    closed <- c(closed, islands, st_geometry(osmd$osm_polygons))
    ## closed is now the combined polygons and islands
    if (!all(st_is_empty(closed))) {
      closed <- st_intersection(closed, bbx)
    }
    ## collect the ones remaining as lines - keep a copy to compare point
    ## order
    m1lines <- m1[is.na(k)]
    ## clips off the outside parts of line
    ## orders of points preserved here (i.e still left handed)
    bbxl <- st_cast(bbx, "LINESTRING")
    if (length(m1lines) > 0) {
      m1clipped <- st_intersection(bbx, m1lines)
      ## union with the lines in the polygon
      m1 <- st_union(m1clipped, bbxl, by_feature=TRUE)
      ## sf polygons at this stage are clockwise - want to detect
      ## if the coastline parts are reversed.
      ## Not sure if there are pathological cases where we can end up
      ## with polygons that have holes. Maybe unlikely given that
      ## this is for coastline.
      polys <- st_cast(st_polygonize(m1))
      ## now to figure out which polygons are sea
      ## Need to sort out whether points are in the
      ## same order as they were when retrieved from osm.
      ##
      ## Original osmdata will have land on the left.
      ## Good approach will be to remove the bounding box from each polygon and
      ## check the overlaps.
      polysM <- st_difference(st_cast(polys, "LINESTRING"), bbxl)
      k <- st_intersects(polysM, m1clipped)
      ## Ones with the same orientation correspond to ocean polygons
      same <- purrr::imap_lgl(k, ~samedirection(polysM[.y], m1clipped[.x]))
      # TK: turned those around, so the resulting polygon is the polygon of the sea
      landpolys <- polys[same]
      seapolys <- polys[!same]
      ## Now need to take difference  between each land polygon and all sea polygons (sequentially)
      ## Then union the land polygons
      
      landpolys.dif <- purrr::map(1:length(landpolys), ~sequential_dif(landpolys[.x], seapolys))
      landpolys <- st_union(do.call(c, landpolys.dif))
      res <- landpolys
    }
    res <- c(res, closed)
    return(res)
  } else {
    ## no lines to process - should test this more carefully
    return(st_intersection(st_geometry(osmd$osm_polygons), bbx))
  }
}

samedirection <- function(poly, lines) {
  p <- st_cast(poly, "POINT")
  l <- st_cast(lines, "POINT")
  idx <- na.omit(match(l, p))
  ## If the idx values are increasing, then order of points is the same, and thus polygon
  ## direction is the same
  return(all(idx == cummax(idx)))
}

sequential_dif <- function(lp, seaset) {
  ## Reduce doesn't work?
  for (i in 1:length(seaset)) {
    lp <- st_difference(lp, seaset[i])
  }
  return(lp)
}


# Show Workings --------------------------------------------------------------------

available_tags("waterway")
available_tags("highway")
available_tags("natural")
available_tags("building")

town <- getbb("Oamaru New Zealand")

main_streets <- opq(town)%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "motorway_link", "motorway_junction",
                            "primary", "primary_link", 
                            "secondary", "secondary_link",
                            "tertiary", "tertiary_link",
                            "road")) %>%
  osmdata_sf()

smaller_streets <- opq(town)%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <-  opq(town)%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

railway <- opq(town)%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

water <- opq(town)%>%
  add_osm_feature(key = "natural", value="water") %>%
  osmdata_sf()

coastline <- opq(town)%>%
  add_osm_feature(key = "natural", value="coastline") %>%
  osmdata_sf()

coast_sf <- sf_land(coastline, town)

ggplot() +
  geom_sf(data = coast_sf,
          fill = "#9bb4d2",
          col = "#9bb4d2",
          size = .2) +
  geom_sf(data = river$osm_lines,
          color = "steelblue",
          size = .2,
          alpha = .5) +
  geom_sf(data = water$osm_polygons,
          fill = "#9bb4d2",
          col = "#9bb4d2",
          size = .1) +
  geom_sf(data = railway$osm_lines,
          color = "black",
          size = .2,
          linetype="21",
          alpha = .2) +
  geom_sf(data = smaller_streets$osm_lines,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = main_streets$osm_lines,
          color = "black",
          size = .3,
          alpha = .3) +
  coord_sf(xlim = town[1,], 
           ylim = town[2,],
           expand = FALSE)  +
  theme_void() +
  theme(plot.background = element_rect(fill = '#fefffe', colour = "#fefffe"),
        text = element_text(family = "Montserrat"), 
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text()) +
  labs(title = "Oamaru, New Zealand",
       caption = "Produced in a You Say Data R Course")

# Convenience Function ----------------------------------------------------

yousaymap <- function(osm_search,
                      coast = F,
                      colourise_ocean = F, # experimental
                      water = T,
                      rail = F,
                      credits = F) {
  town <- getbb(osm_search)
  if (anyNA(town)) {
    stop("place not found through Overpass API.")
  }
  base_map <- ggplot()
  
  if (coast) {
    coastline <- opq(town)%>%
      add_osm_feature(key = "natural", value="coastline") %>%
      osmdata_sf()
    
    base_map <- base_map + 
      geom_sf(data = coastline$osm_lines,
              col = "#666666",
              size = .1)
    
    if (colourise_ocean) {
      coast_sf <- sf_land(coastline, town)
      
      base_map <- base_map +
        geom_sf(data = coast_sf,
                fill = "#9bb4d2",
                col = "#9bb4d2",
                size = .2)
    }
  }
  
  if (water) {
    
    river <-  opq(town)%>%
      add_osm_feature(key = "waterway", value = c("river", "stream")) %>%
      osmdata_sf()
    
    water <- opq(town)%>%
      add_osm_feature(key = "natural", value="water") %>%
      osmdata_sf()
    
    base_map <- base_map +
      geom_sf(data = river$osm_lines,
              color = "steelblue",
              size = .2,
              alpha = .5) +
      geom_sf(data = water$osm_polygons,
              fill = "#9bb4d2",
              col = "#9bb4d2",
              size = .1)
  }
  
  if (rail) {
    railway <- opq(town)%>%
      add_osm_feature(key = "railway", value="rail") %>%
      osmdata_sf()
    
    base_map <- base_map +
      geom_sf(data = railway$osm_lines,
              color = "black",
              size = .2,
              linetype="21",
              alpha = .2)
  }
  
  main_streets <- opq(town)%>%
    add_osm_feature(key = "highway", 
                    value = c("motorway", "primary",  
                              "secondary", "tertiary")) %>%
    osmdata_sf()
  
  smaller_streets <- opq(town)%>%
    add_osm_feature(key = "highway", 
                    value = c("residential", "living_street",
                              "unclassified",
                              "service", "footway")) %>%
    osmdata_sf()
  
  base_map <- base_map +
    geom_sf(data = smaller_streets$osm_lines,
            color = "#666666",
            size = .2,
            alpha = .3) +
    geom_sf(data = main_streets$osm_lines,
            color = "black",
            size = .3,
            alpha = .3) +
    coord_sf(xlim = town[1,], 
             ylim = town[2,],
             expand = FALSE)
  
  if (credits) {
    base_map <- base_map +
      theme_void() +
      theme(plot.background = element_rect(fill = '#fefffe', colour = "#fefffe"),
            text = element_text(family = "Montserrat"), 
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text()) +
      labs(title = osm_search,
           caption = "Produced in a You Say Data R Course")
  } else {
    base_map <- base_map +
      theme_void()
    theme(plot.background = element_rect(fill = '#fefffe', colour = "#fefffe"))
  }
  base_map
}

yousaymap("Oamaru, New Zealand", coast = T, colourise_ocean = T, water = T, rail = T, credits = T)
yousaymap("Hamilton, New Zealand", credits = T)
