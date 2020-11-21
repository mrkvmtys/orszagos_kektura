library(XML)
library(OpenStreetMap)
library(lubridate)
library(ggmap)
library(ggplot2)
library(raster)
library(sp)
library(mapview)


options(digits = 10)

# list all gpx tracks
setwd("C:/Users/mrm/Desktop/szemelyes/OKT/gpx/old")
file_list <- list.files()
act_date <- as.Date(now())

# initiate a blank data frame
df <- data.frame()

# read data

for (i in 1:length(file_list)){

  pfile <- htmlTreeParse(file = file_list[i], error = function(...) {}, useInternalNodes = T)
  
  elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
  times  <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
  coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs) 
  lats <- as.numeric(coords["lat" , ])
  lons <- as.numeric(coords["lon" , ])
  
  temp_data <- data.frame(time = times, lat = lats, lon = lons, ele = elevations)
  temp_data$okt <- sub(".gpx","",file_list[i])
  
  df <- rbind(df, temp_data)
  
  rm(list = c("elevations", "lats", "lons", "pfile", "times", "coords", "temp_data"))
  
  }

# data tranformation

df$okt <- as.factor(df$okt)
df$time <- strptime(df$time, format = "%Y-%m-%dT%H:%M:%OS")

# remove unnecessarily logged parts
df <- df[df$time < "2020-04-14 12:00:00" | df$time > "2020-04-14 22:00:00" , ]

# visualization
# the map size we need
lat  <- c(min(df$lat), max(df$lat))
lon  <- c(min(df$lon), max(df$lon))

# whole hungary
lat2 <- c(46.65,48.53)
lon2 <- c(16.05,22.09)

bbox <- make_bbox(lon, lat)
bbox2 <- make_bbox(lon2, lat2)

bl   <- get_map(bbox, maptype = "terrain", source = "stamen", color = "color")
bl   <- get_stamenmap(bbox, maptype = "terrain")
bl2  <- get_stamenmap(bbox, maptype = "toner")
bl3  <- get_stamenmap(bbox, maptype = "watercolor")

# plots zoomed
# 0
png(file = paste0("C:/Users/mrm/Desktop/szemelyes/OKT/results/okt_", act_date, ".png"), width = 1500, height = 1500, units = "px")

ggmap(bl) + geom_point(data = df,
                       aes(lon, lat, col = okt), size = 2, alpha = 0.7) + labs(x = "Longitude", y = "Latitude", title = "OKT") + scale_color_brewer(palette = "Dark2")

dev.off()

# 1
png(file = paste0("C:/Users/mrm/Desktop/szemelyes/OKT/results/okt1_", act_date, ".png"), width = 1500, height = 1500, units = "px")

ggmap(bl2) + geom_point(data = df,
                       aes(lon, lat, col = okt), size = 2, alpha = 0.7) + labs(x = "Longitude", y = "Latitude", title = "OKT") + scale_color_brewer(palette = "Dark2")

dev.off()

# 2
png(file = paste0("C:/Users/mrm/Desktop/szemelyes/OKT/results/okt2_", act_date, ".png"), width = 1500, height = 1500, units = "px")

ggmap(bl3) + geom_point(data = df,
                        aes(lon, lat, col = okt), size = 2, alpha = 0.7) + labs(x = "Longitude", y = "Latitude", title = "OKT") + scale_color_brewer(palette = "Dark2")

dev.off()

# plots full country

bl_fc   <- get_stamenmap(bbox2, maptype = "terrain")
bl2_fc  <- get_stamenmap(bbox2, maptype = "toner")
bl3_fc  <- get_stamenmap(bbox2, maptype = "watercolor")

# 0
png(file = paste0("C:/Users/mrm/Desktop/szemelyes/OKT/results/okt_fc_", act_date, ".png"), width = 5000, height = 5000, units = "px")

ggmap(bl_fc) + geom_point(data = df,
                       aes(lon, lat, col = okt), size = 2, alpha = 0.7) + labs(x = "Longitude", y = "Latitude", title = "OKT") + scale_color_brewer(palette = "Dark2")

dev.off()

# 1
png(file = paste0("C:/Users/mrm/Desktop/szemelyes/OKT/results/okt1_fc_", act_date, ".png"), width = 5000, height = 5000, units = "px")

ggmap(bl2_fc) + geom_point(data = df,
                        aes(lon, lat, col = okt), size = 2, alpha = 0.7) + labs(x = "Longitude", y = "Latitude", title = "OKT") + scale_color_brewer(palette = "Dark2")

dev.off()

# 2
png(file = paste0("C:/Users/mrm/Desktop/szemelyes/OKT/results/okt2_fc_", act_date, ".png"), width = 5000, height = 5000, units = "px")

ggmap(bl3_fc) + geom_point(data = df,
                        aes(lon, lat, col = okt), size = 2, alpha = 0.7) + labs(x = "Longitude", y = "Latitude", title = "OKT") + scale_color_brewer(palette = "Dark2")

dev.off()

#### end ####
spdf_geo <- df

coordinates(spdf_geo) <- ~lon+lat
proj4string(spdf_geo) <- "+init=epsg:4326"

mapview(spdf_geo)

#rm(list = ls())


library(leaflet)

leaflet() %>% 
  addTiles() %>% 
  addFeatures(spdf_geo, weight = 1, fillColor = "grey", color = "black",
              opacity = 1, fillOpacity = 0.6)
