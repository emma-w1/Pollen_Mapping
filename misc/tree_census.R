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
library(httr)
library(jsonlite)
library(dotenv)
library(glue)


readRenviron("/Users/wenggeiwong/Pollen_Mapping/.env")

options(scipen = 999)

app_token = Sys.getenv("OPENDATA_APP_TOKEN")
api_key_id = Sys.getenv("OPENDATA_KEY_ID")
api_key = Sys.getenv("OPENDATA_KEY")



api_call <- function(borough){
  if(borough == "Manhattan"){
    link <- "https://data.cityofnewyork.us/api/v3/views/uvpi-gqnh/query.json?query=SELECT%0A%20%20%60tree_id%60%2C%0A%20%20%60block_id%60%2C%0A%20%20%60created_at%60%2C%0A%20%20%60tree_dbh%60%2C%0A%20%20%60stump_diam%60%2C%0A%20%20%60curb_loc%60%2C%0A%20%20%60status%60%2C%0A%20%20%60health%60%2C%0A%20%20%60spc_latin%60%2C%0A%20%20%60spc_common%60%2C%0A%20%20%60steward%60%2C%0A%20%20%60guards%60%2C%0A%20%20%60sidewalk%60%2C%0A%20%20%60user_type%60%2C%0A%20%20%60problems%60%2C%0A%20%20%60root_stone%60%2C%0A%20%20%60root_grate%60%2C%0A%20%20%60root_other%60%2C%0A%20%20%60trunk_wire%60%2C%0A%20%20%60trnk_light%60%2C%0A%20%20%60trnk_other%60%2C%0A%20%20%60brch_light%60%2C%0A%20%20%60brch_shoe%60%2C%0A%20%20%60brch_other%60%2C%0A%20%20%60address%60%2C%0A%20%20%60zipcode%60%2C%0A%20%20%60zip_city%60%2C%0A%20%20%60cb_num%60%2C%0A%20%20%60borocode%60%2C%0A%20%20%60boroname%60%2C%0A%20%20%60cncldist%60%2C%0A%20%20%60st_assem%60%2C%0A%20%20%60st_senate%60%2C%0A%20%20%60nta%60%2C%0A%20%20%60nta_name%60%2C%0A%20%20%60boro_ct%60%2C%0A%20%20%60state%60%2C%0A%20%20%60latitude%60%2C%0A%20%20%60longitude%60%2C%0A%20%20%60x_sp%60%2C%0A%20%20%60y_sp%60%2C%0A%20%20%60council_district%60%2C%0A%20%20%60census_tract%60%2C%0A%20%20%60bin%60%2C%0A%20%20%60bbl%60%0AWHERE%0A%20%20caseless_eq(%60status%60%2C%20%22Alive%22)%0A%20%20AND%20caseless_one_of(%60boroname%60%2C%20%22Bronx%22%2C%20%22Manhattan%22)"
  }else if(borough == "Bronx"){
    link <- "https://data.cityofnewyork.us/api/v3/views/uvpi-gqnh/query.json?query=SELECT%0A%20%20%60tree_id%60%2C%0A%20%20%60block_id%60%2C%0A%20%20%60created_at%60%2C%0A%20%20%60tree_dbh%60%2C%0A%20%20%60stump_diam%60%2C%0A%20%20%60curb_loc%60%2C%0A%20%20%60status%60%2C%0A%20%20%60health%60%2C%0A%20%20%60spc_latin%60%2C%0A%20%20%60spc_common%60%2C%0A%20%20%60steward%60%2C%0A%20%20%60guards%60%2C%0A%20%20%60sidewalk%60%2C%0A%20%20%60user_type%60%2C%0A%20%20%60problems%60%2C%0A%20%20%60root_stone%60%2C%0A%20%20%60root_grate%60%2C%0A%20%20%60root_other%60%2C%0A%20%20%60trunk_wire%60%2C%0A%20%20%60trnk_light%60%2C%0A%20%20%60trnk_other%60%2C%0A%20%20%60brch_light%60%2C%0A%20%20%60brch_shoe%60%2C%0A%20%20%60brch_other%60%2C%0A%20%20%60address%60%2C%0A%20%20%60zipcode%60%2C%0A%20%20%60zip_city%60%2C%0A%20%20%60cb_num%60%2C%0A%20%20%60borocode%60%2C%0A%20%20%60boroname%60%2C%0A%20%20%60cncldist%60%2C%0A%20%20%60st_assem%60%2C%0A%20%20%60st_senate%60%2C%0A%20%20%60nta%60%2C%0A%20%20%60nta_name%60%2C%0A%20%20%60boro_ct%60%2C%0A%20%20%60state%60%2C%0A%20%20%60latitude%60%2C%0A%20%20%60longitude%60%2C%0A%20%20%60x_sp%60%2C%0A%20%20%60y_sp%60%2C%0A%20%20%60council_district%60%2C%0A%20%20%60census_tract%60%2C%0A%20%20%60bin%60%2C%0A%20%20%60bbl%60%0AWHERE%0A%20%20caseless_eq(%60status%60%2C%20%22Alive%22)%0A%20%20AND%20caseless_one_of(%60boroname%60%2C%20%22Bronx%22%2C%20%22Manhattan%22)"
  }else if(borough == "Both"){
    link <- "https://data.cityofnewyork.us/api/v3/views/uvpi-gqnh/query.json?query=SELECT%0A%20%20%60tree_id%60%2C%0A%20%20%60block_id%60%2C%0A%20%20%60created_at%60%2C%0A%20%20%60tree_dbh%60%2C%0A%20%20%60stump_diam%60%2C%0A%20%20%60curb_loc%60%2C%0A%20%20%60status%60%2C%0A%20%20%60health%60%2C%0A%20%20%60spc_latin%60%2C%0A%20%20%60spc_common%60%2C%0A%20%20%60steward%60%2C%0A%20%20%60guards%60%2C%0A%20%20%60sidewalk%60%2C%0A%20%20%60user_type%60%2C%0A%20%20%60problems%60%2C%0A%20%20%60root_stone%60%2C%0A%20%20%60root_grate%60%2C%0A%20%20%60root_other%60%2C%0A%20%20%60trunk_wire%60%2C%0A%20%20%60trnk_light%60%2C%0A%20%20%60trnk_other%60%2C%0A%20%20%60brch_light%60%2C%0A%20%20%60brch_shoe%60%2C%0A%20%20%60brch_other%60%2C%0A%20%20%60address%60%2C%0A%20%20%60zipcode%60%2C%0A%20%20%60zip_city%60%2C%0A%20%20%60cb_num%60%2C%0A%20%20%60borocode%60%2C%0A%20%20%60boroname%60%2C%0A%20%20%60cncldist%60%2C%0A%20%20%60st_assem%60%2C%0A%20%20%60st_senate%60%2C%0A%20%20%60nta%60%2C%0A%20%20%60nta_name%60%2C%0A%20%20%60boro_ct%60%2C%0A%20%20%60state%60%2C%0A%20%20%60latitude%60%2C%0A%20%20%60longitude%60%2C%0A%20%20%60x_sp%60%2C%0A%20%20%60y_sp%60%2C%0A%20%20%60council_district%60%2C%0A%20%20%60census_tract%60%2C%0A%20%20%60bin%60%2C%0A%20%20%60bbl%60%0AWHERE%0A%20%20caseless_eq(%60status%60%2C%20%22Alive%22)%0A%20%20AND%20caseless_one_of(%60boroname%60%2C%20%22Bronx%22%2C%20%22Manhattan%22)"
  }
  r1 <- GET(
    url = link,
    add_headers(
      "X-App-Token" = app_token
    ),
    authenticate(api_key_id,api_key, type = "basic"),
    encode = "json"
  )
  if (status_code(r1) == 200) {
    # JSON response into a dataframe
    print("API call successful")
    df <- fromJSON(content(r1, "text", encoding = "UTF-8"))
    df <- df[,c('status','spc_latin','spc_common','boroname','latitude','longitude')] # filter columns
    return(df)
  }else {
    print(paste("Error:", status_code(r1)))
    print(content(r1, "text"))
  }
}

export_unique_species <- function(df){ # exports unique species names in species.csv
  species_vector <- df[,c('spc_common','spc_latin')]
  unique_species <- unique(species_vector)
  write.csv(unique_species,file="/Users/wenggeiwong/Pollen_Mapping/data/species.csv",row.names=FALSE)
}

export_tree_data <- function(borough){ # exports data from tree census filtered by desired columns and boroughs
  df <- api_call(borough)
  write.csv(df,file=glue("/Users/wenggeiwong/Pollen_Mapping/data/{tolower(borough)}_tree_data.csv"),row.names=FALSE)
}

main <- function(){
  print("Complete!")
}

main()