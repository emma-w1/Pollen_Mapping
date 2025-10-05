install.packages("sf")
install.packages("tmap")
install.packages("tmaptools")
library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(nycgeo)
library(dplyr)


options(scipen = 999)

data <- read.csv("./pollen.csv", header = TRUE)
pollen_data <- data[,c("Borough","Influx_trees","Influx_trees_alrg","Influx_plat","Influx_que","Influx_amb",
                              "Influx_ulm","Influx_pop","Influx_carya","Influx_poa","Influx_acer","Influx_fagus",
                              "Influx_bet","Influx_frax","Latitude","Longitude")]
pollen_data <- st_as_sf(pollen_data,coords = c("Longitude", "Latitude"),  # specify coordinate columns
                       crs = 4326 ) # assign a coordinate reference system (WGS84)

pollen_map = nyc_boundaries(geography = "tract") #NYC MAP

#Manhattan setup
manhattan_data <- pollen_data[pollen_data$Borough == "Manhattan", ]
pollen_map_manhattan = nyc_boundaries(geography = "tract", 
                                      filter_by = "borough",
                                      region = "manhattan")
pollen_map_manhattan <- st_make_valid(pollen_map_manhattan)
pollen_map_manhattan <- st_transform(pollen_map_manhattan, 4326)


# Spatial join: assign each pollen point to a tract
pollen_joined <- st_join(manhattan_data, pollen_map_manhattan, join = st_within)

#Summarized data by census tract
pollen_by_tract <- pollen_joined %>%
  group_by(tract_id = "geoid") %>%
  summarize(mean_influx = mean(Influx_trees, na.rm = TRUE))

#uses tmap to make cloropleth graph
  