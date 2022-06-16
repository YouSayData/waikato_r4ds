library(tidyverse)
library(maps)
library(PBSmapping)
library(mapproj)
# library(gganimate)
library(mapview)

xlim <- c(-12,55)
ylim <- c(20,60)

grep("grey", colours(), value = T)
landborder <- "lightgrey"
land <- "lightgrey"
worldmap <- map_data("world")

# for clipping with PBSmapping
colnames(worldmap) <- c("X","Y","PID","POS","region","subregion")
worldmap <- clipPolys(worldmap, xlim = xlim, ylim = ylim, keepExtra=TRUE)


locsRaw <- read_csv("pleiades-locations.csv")
View(locsRaw)
dataLabel <- "Data: Pleiades Project"

# periods and features ####

periods <- rbind(
  c("archaic","750-550BC"),
  c("classical","550-330BC"),
  c("hellenistic-republican","330-30BC"),
  c("roman","30BC-300CE"),
  c("late-antique","300-640CE")
)

features <- rbind(
  c("","locations"),
  c("settlement","settlements"),
  c("fort","forts"),
  c("temple","temples"),
  c("villa","villas"),
  c("station","stations"),
  c("theatre","theatres"),
  c("amphitheatre","amphitheatres"),
  c("church","churches"),
  c("bridge","bridges"),
  c("bath","baths"),
  c("cemetery","cemeteries"),
  c("plaza","plazas"),
  c("arch","archs")
)

locPleiades <- geom_point(
  data = locsRaw,
  aes(reprLong, reprLat),
  color = "grey70",
  alpha = .75,
  size = 1)

# loops ####

for (i in 1:nrow(features)) {
  locs <- locsRaw[ with(locsRaw, grepl(features[i,1],featureType)),]
  for (ii in 1:nrow(periods)) {
    locPer <- locs[ with(locs,grepl(periods[ii,1],timePeriodsKeys)),]

    fName <- paste0("plots_1/Pleiades_",features[i,2],sprintf("%02d",ii),".png")
    header <- paste0(features[i,2]," in the ",periods[ii,1]," period (",periods[ii,2],")")
    p <- ggplot(locPer) +
      coord_map(xlim = xlim, ylim = ylim) +
      geom_polygon(data = worldmap, 
                   mapping = aes(X,Y,group=PID), 
                   size = 0.1, 
                   colour = landborder,
                   fill = land,
                   alpha = 1) +
      geom_point(
        data = locsRaw,
        aes(reprLong, reprLat),
        color = "grey70",
        alpha = .75,
        size = 1) + 
      geom_point(aes(y=reprLat,x=reprLong),
                 color="salmon",
                 alpha=.75,
                 size=1) + 
      labs(title = header, y="",x="") + 
      annotate("text",
               x=-11,
               y=21,
               hjust=0,
               label=dataLabel,
               size=3,
               color="grey40") +
      theme_minimal(base_family = "serif") +
      theme(panel.background = element_rect(fill = "darkslategrey"))
    
    ggsave(file=fName,plot=p,dpi=600,width=7,height=6)
  }
}

# just one plot ####

locs <- locsRaw[ with(locsRaw, grepl("amphitheatre",featureType)),]
locPer <- locs[ with(locs,grepl("roman",timePeriodsKeys)),]

ggplot(locPer) +
  coord_map(xlim = xlim, ylim = ylim) +
  geom_polygon(data = worldmap, 
               mapping = aes(X,Y,group=PID), 
               size = 0.1, 
               colour = landborder,
               fill = land,
               alpha = 1) +
  geom_point(
    data = locsRaw,
    aes(reprLong, reprLat),
    color = "grey70",
    alpha = .75,
    size = 1) + 
  geom_point(aes(y=reprLat,x=reprLong),
             color="salmon",
             alpha=.75,
             size=1) + 
  labs(title = "Amphitheatres in the Roman period", y="",x="") + 
  annotate("text",
           x=-11,
           y=21,
           hjust=0,
           label=dataLabel,
           size=3,
           color="grey40") +
  theme_minimal(base_family = "serif") +
  theme(panel.background = element_rect(fill = "darkslategrey"))

sf_locPer <- locPer
sp::coordinates(sf_locPer) <- c("reprLong", "reprLat")
sp::proj4string(sf_locPer) <- sp::CRS("+init=epsg:4326")
mapview(sf_locPer, zcol = "featureType", burst = TRUE) 

