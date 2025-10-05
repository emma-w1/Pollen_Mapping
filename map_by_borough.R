borough_map <- function(borough) {
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
  
  #borough-specific setup
  borough_data <- pollen_data[pollen_data$Borough == borough, ]
  borough_data <- st_transform(borough_data, 4326)
  pollen_map_borough = nyc_boundaries(geography = "tract", 
                                        filter_by = "borough",
                                        region = tolower(borough))
  pollen_map_borough <- st_make_valid(pollen_map_borough)
  pollen_map_borough <- st_transform(pollen_map_borough, 4326)
  
  
  #assign each pollen point to 0.25km buffer
  pollen_buffers <- borough_data %>%
    st_transform(32618) %>%  # UTM Zone 18N for NYC
    st_buffer(dist = 250) %>%
    st_transform(4326)  # transform back if needed %>%
  
  pollen_buffers$buffer_id <- 1:nrow(pollen_buffers)
  
  pollen_buffers <- pollen_buffers %>%
    group_by(buffer_id) %>%
    summarize(mean_influx = mean(Influx_trees_alrg, na.rm = TRUE))
  
  
  #adds pollen_buffer mean column to pollen_map_borough
  pollen_map_borough <- st_join(pollen_map_borough, pollen_buffers, join = st_intersects)
  
  # pollen_by_tract <- pollen_map_borough %>%
  #   group_by(buffer_id) %>%
  #   
  # st_join(pollen_by_tract, pollen_map_borough %>% select(buffer_id), join = st_intersects)
  
  title = paste(borough,"Tree Pollen Intensity")
  #use tmap to make chloropleth graphs
  tmap_mode("view")
  map1 <- tm_shape(pollen_map_borough) + #sets shape to borough map
    tm_polygons(fill = "mean_influx", #maps based on pollen influx
                title = "Avg Tree Pollen Influx",
                palette = "YlOrRd", #color palette
                border.alpha = 0.1) + #border thickness
    tm_layout(title = title)
  

  map2 <- tm_shape(pollen_buffers) +
    tm_polygons(fill = "mean_influx",
                title = "Avg Tree Pollen Influx",
                palette = "YlOrRd",
                col_alpha = 0.5) +
    tm_shape(borough_data) +  # Add collection points
    tm_dots(size = 0.05, col = "black", alpha = 0.8) +
    tm_title(paste(title,"(250m buffer)")) + 
    tm_legend(item.height = -10, item.width = -10)
  
  print(map1)
  print(map2)
}

borough_map("Manhattan")
