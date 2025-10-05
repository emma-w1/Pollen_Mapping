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
pollen_data <- st_as_sf(pollen_data, coords = c("Longitude", "Latitude"),  # specify coordinate columns
                        crs = 4326) # assign a coordinate reference system (WGS84)

pollen_map = nyc_boundaries(geography = "tract") #NYC MAP

#Manhattan setup
manhattan_data <- pollen_data[pollen_data$Borough == "Manhattan", ]
manhattan_data <- st_transform(manhattan_data, 4326)
pollen_map_manhattan = nyc_boundaries(geography = "tract", 
                                      filter_by = "borough",
                                      region = "manhattan")
pollen_map_manhattan <- st_make_valid(pollen_map_manhattan)
pollen_map_manhattan <- st_transform(pollen_map_manhattan, 4326)


#pollen_joined <- st_join(manhattan_data, pollen_map_manhattan, join = st_within)

#assign each pollen point to 0.25km buffer
pollen_buffers <- manhattan_data %>%
  st_transform(32618) %>%  # UTM Zone 18N for NYC
  st_buffer(dist = 250) %>%
  st_transform(4326)  # transform back if needed %>%

pollen_buffers$buffer_id <- 1:nrow(pollen_buffers)

pollen_buffers <- pollen_buffers %>%
  group_by(buffer_id)
  summarize(mean_influx = mean(Influx_trees_alrg, na.rm = TRUE))


#adds pollen_by_tract column to pollen_map_manhattan
#pollen_map_manhattan <- st_join(pollen_map_manhattan, pollen_buffers, join = st_intersects)

# manhattan_coords <- manhattan_data %>%
#   mutate(
#     longitude = st_coordinates(.)[,1],
#     latitude = st_coordinates(.)[,2]
#   )

# pollen_map_manhattan <- st_join(pollen_map_manhattan, 
#                                 manhattan_coords,
#                                 join = st_intersects)

pollen_by_tract <- pollen_map_manhattan %>%
  group_by(buffer_id) %>%
  #mutate(buffer_id = buffer_id)
  #summarize(mean_influx = mean(Influx_trees_alrg.x, na.rm = TRUE))
  st_join(pollen_by_tract, pollen_map_manhattan %>% select(buffer_id), join = st_intersects)

#uses tmap to make chloropleth graph
tmap_mode("view")
tm_shape(pollen_by_tract) + #sets shape to manhattan map
  tm_polygons(fill = "mean_influx", #maps based on pollen influx
              title = "Avg Tree Pollen Influx",
              palette = "YlOrRd", #color palette
              border.alpha = 0.1) + #border thickness
  tm_layout(title = "Manhattan Tree Pollen Intensity") +
  #tm.dots()

tmap_mode("view")
tm_shape(pollen_buffers) +
  tm_polygons(fill = "mean_influx",
              title = "Avg Tree Pollen Influx",
              palette = "YlOrRd",
              border.alpha = 0.5) +
  tm_shape(manhattan_data) +  # Add collection points
  tm_dots(size = 0.05, col = "black", alpha = 0.8) +
  tm_layout(title = "Manhattan Tree Pollen Intensity (250m Buffers)")
  