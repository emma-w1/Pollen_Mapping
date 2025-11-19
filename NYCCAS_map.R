library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(nycgeo)
library(dplyr)
library(htmltools)
library(terra)


options(scipen = 999)

map <- st_read("./data/redlining_sfs/manh_holc_sf.json")
col_sites <- read.csv("./data/location_site.csv", header=TRUE)

