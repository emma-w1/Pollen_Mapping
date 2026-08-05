
library(pacman)
library(readxl)

p_load(tidyverse, sf, kableExtra, tidycensus, httr, rjson, leaflet, hereR, sp, profvis)

readRenviron("~/.Renviron")

AzureMapsKey <- Sys.getenv("AzureMapsKey")
here_key     <- Sys.getenv("HERE_API_KEY")
set_key(here_key)

data_dir <- "/Users/wenggeiwong/pollen_mapping_data/E2SFCA_model_data"
dir.create(data_dir, showWarnings = FALSE)



# CHANGE FOR MANHATTAN ANALYSIS LATER
district_shp_path <- file.path('/Users/wenggeiwong/pollen_mapping_data/redlining_sfs/brx_holc_sf.json') 
pollen_by_district <- read.csv('/Users/wenggeiwong/pollen_mapping_data/joined_data/bronx_data/bronx_Influx_trees_alrg_bronx.csv')

district_id_col    <- "label"
district_grade_col <- "grade"

districts_raw <- st_read(district_shp_path, quiet = TRUE)


districts <- districts_raw %>%
  rename(district_id = all_of(district_id_col),
         holc_grade  = all_of(district_grade_col)) %>%
  select(district_id, holc_grade, geometry) %>%
  st_make_valid()

# POPULATION PER DISTRICT (areal-weighted interpolation from Census blocks)

blocks <- get_decennial(
  geography = "block",
  variables = "P1_001N",   # total population, 2020 Decennial
  state     = "NY",
  county    = "Bronx",
  year      = 2020,
  geometry  = TRUE
) %>%
  rename(population = value) %>%
  select(population) %>%
  st_transform(st_crs(districts)) %>%
  st_make_valid()


district_pop_geom <- suppressWarnings(
  st_interpolate_aw(blocks["population"], districts, extensive = TRUE)
)


district_pop <- districts %>%
  st_drop_geometry() %>%
  bind_cols(population = district_pop_geom$population) %>%
  mutate(population = replace_na(population, 0)) %>%
  bind_cols(st_geometry(districts)) %>%
  st_as_sf()

# Only keep districts with population > 0 in Bronx, mirroring your
# `filter(brx_pop > 0)` step in the ZIP version.
populations <- district_pop %>%
  filter(population > 0) %>%
  select(district_id, holc_grade, population)

# REPRESENTATIVE POINT PER DISTRICT (for isochrone origin)

districts_pts <- st_point_on_surface(populations)

leaflet(districts_pts) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(radius = 4, fillOpacity = 0.75,
                   popup = ~paste("District", district_id, "-", holc_grade))

# ISOCHRONES PER DISTRICT (w/ HERE API)


isochrone_dir <- file.path(data_dir, "isochrones")
dir.create(isochrone_dir, showWarnings = FALSE)

failed_districts <- character(0)

for (i in seq_len(nrow(districts_pts))) {
  
  layer <- str_replace_all(as.character(districts_pts$district_id[i]), "[^A-Za-z0-9_-]", "_")
  
  # NOTE (bug fix): wrapping each district in tryCatch so one bad geometry
  # doesn't abort the entire loop after (potentially) an hour of HERE API
  # calls. Failed districts are logged to `failed_districts` for you to
  # inspect/re-run individually afterward.
  result <- tryCatch({
    
    isochrone <- isoline(
      poi        = districts_pts[i, ],
      range      = seq(10, 20, 10) * 60,   # 600s, 1200s -> 10 min, 20 min
      range_type = "time",
      datetime   = as.POSIXct(paste0(Sys.Date(), " 10:00"))
    ) %>%
      mutate(name = c("0 to 10 mins", "10 to 20 mins")) %>%
      st_make_valid()
    

    diff_raw <- st_difference(st_geometry(isochrone[2, ]), st_geometry(isochrone[1, ]))
    
    ring_geom <- diff_raw %>%
      st_make_valid() %>%
      st_collection_extract("POLYGON") %>%
      st_combine()
    
    if (length(ring_geom) == 0 || st_is_empty(ring_geom)) {
      ring_geom <- st_geometry(isochrone[2, ])
    }
    
    ring_0_10  <- isochrone[1, ]
    ring_10_20 <- isochrone[2, ]
    st_geometry(ring_10_20) <- ring_geom
    ring_10_20$name  <- "10 to 20 mins"
    ring_10_20$range <- 1200
    
    isochrone_final <- rbind(ring_0_10, ring_10_20) %>%
      select(-any_of(c("departure", "arrival")))
    
    st_write(
      obj         = isochrone_final,
      dsn         = isochrone_dir,
      layer       = layer,
      driver      = "ESRI Shapefile",
      delete_layer = TRUE,
      quiet       = TRUE
    )
    
    TRUE
  }, error = function(e) {
    message("District ", layer, " failed: ", conditionMessage(e))
    FALSE
  })
  
  if (!isTRUE(result)) failed_districts <- c(failed_districts, layer)
  
  Sys.sleep(.1)
}

if (length(failed_districts) > 0) {
  message(length(failed_districts), " district(s) failed isochrone creation: ",
          paste(failed_districts, collapse = ", "))
}

# GEOCODE CLINICS

brx_clinics <- read.csv(file.path(
  data_dir,
  "Uncleaned Clinics for Allergenic Clinic Access (NAICS Codes) - Bronx Addresses.csv"
))

brx_clinics <- brx_clinics %>% 
  mutate(clean_address = Full_Address %>% #
           # Remove suite/apt designators like #4B, Ste 100, Apt 2, Unit C
           gsub("(?i)\\s*(#|ste|suite|apt|unit|fl)\\s*\\w+", "", ., perl = TRUE) %>%
           # Fix missing space in directional street numbers (e.g., W190th -> W 190th)
           gsub("(?i)\\b([NWSE])(\\d)", "\\1 \\2", ., perl = TRUE)
  )


if (!"capacity" %in% names(brx_clinics)) {
  brx_clinics$capacity <- 1
}

brx_clinics$Latitude  <- NA_real_
brx_clinics$Longitude <- NA_real_

for (i in seq_len(nrow(brx_clinics))) {
  
  response <- GET(
    "https://atlas.microsoft.com/search/address/json",
    query = list(
      "api-version"       = "1.0",
      query               = brx_clinics$clean_address[i],
      "subscription-key"  = AzureMapsKey
    )
  )
  

  if (status_code(response) == 200) {
    json <- fromJSON(content(response, "text", encoding = "UTF-8"))
    if (length(json$results) > 0) {
      brx_clinics$Latitude[i]  <- json$results[[1]]$position$lat
      brx_clinics$Longitude[i] <- json$results[[1]]$position$lon
    }
  }
  
  Sys.sleep(0.01)
}

brx_clinics.sf <- brx_clinics %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# CATCHMENT MATRIX: which travel-time band is each clinic in, relative to each district's isochrone

shape.extension <- list.files(path = isochrone_dir, pattern = "\\.shp$", full.names = TRUE)
shape.names <- list.files(path = isochrone_dir, pattern = "\\.shp$", full.names = FALSE) %>%
  str_remove("\\.shp$")

catchment_matrix <- matrix(NA_character_, nrow = nrow(brx_clinics.sf), ncol = length(shape.names))
colnames(catchment_matrix) <- shape.names
rownames(catchment_matrix) <- brx_clinics.sf$Location

for (i in seq_along(shape.names)) {
  
  isochrone <- st_read(dsn = shape.extension[i], quiet = TRUE) %>%
    st_transform(st_crs(brx_clinics.sf))
  
  idx <- st_within(brx_clinics.sf, isochrone)
  
  catchment_matrix[, i] <- vapply(idx, function(x) {
    if (length(x) == 0) NA_character_ else isochrone$name[x[1]]
  }, character(1))
}


# E2SFCA STEP 1 — provider-to-population ratio for each clinic

band_weights <- c("0 to 10 mins" = 1, "10 to 20 mins" = 0.42)

pop_lookup <- populations %>%
  st_drop_geometry() %>%
  select(district_id, population)


pop_lookup$district_id <- str_replace_all(as.character(pop_lookup$district_id), "[^A-Za-z0-9_-]", "_")

demand_by_clinic <- vapply(seq_len(nrow(catchment_matrix)), function(j) {
  bands <- catchment_matrix[j, ]
  in_catchment <- names(bands)[!is.na(bands)]
  if (length(in_catchment) == 0) return(0)
  w <- band_weights[bands[in_catchment]]
  p <- pop_lookup$population[match(in_catchment, pop_lookup$district_id)]
  sum(p * w, na.rm = TRUE)
}, numeric(1))

clinic_ratios <- tibble(
  Location = brx_clinics.sf$Location,
  capacity = brx_clinics.sf$capacity,
  demand   = demand_by_clinic
) %>%
  mutate(R_j = if_else(demand > 0, capacity / demand, NA_real_))

# accessibility index for each district


rj_vec <- clinic_ratios$R_j

accessibility_by_district <- apply(catchment_matrix, 2, function(bands) {
  # Find row indices of clinics that reach this district
  in_catchment <- which(!is.na(bands))
  if (length(in_catchment) == 0) return(0)
  
  weights <- band_weights[bands[in_catchment]]
  ratios  <- rj_vec[in_catchment]
  
  sum(ratios * weights, na.rm = TRUE)
})

district_access <- tibble(
  district_id   = colnames(catchment_matrix),
  accessibility = accessibility_by_district
)

# MAP RESULTS

districts_for_map <- populations %>%
  mutate(district_id_sanitized = str_replace_all(as.character(district_id), "[^A-Za-z0-9_-]", "_")) %>%
  left_join(district_access, by = c("district_id_sanitized" = "district_id")) %>%
  mutate(accessibility = replace_na(accessibility, 0))

colnames(districts_for_map)[colnames(districts_for_map) == "district_id"] <- "zone_id"


# Create color palette based on non-zero accessibility values
pal <- colorNumeric(palette = "YlOrRd", domain = districts_for_map$accessibility)

leaflet(districts_for_map) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor   = ~pal(accessibility),
    fillOpacity = 0.75,
    color       = "#444444",
    weight      = 1,
    popup       = ~paste0(
      "<strong>District:</strong> ", zone_id, " (Grade ", holc_grade, ")<br>",
      "<strong>Population:</strong> ", round(population), "<br>",
      "<strong>Accessibility Index:</strong> ", round(accessibility, 5)
    )
  ) %>%
  addLegend(pal = pal, values = ~accessibility, title = "E2SFCA Index", labFormat = labelFormat(digits=8))



anova_data <- districts_for_map %>%
  st_drop_geometry() %>%
  filter(!is.na(accessibility)) %>%
  filter(holc_grade %in% c("A","B","C","D"))


welchs_anova <- oneway.test(
  accessibility ~ holc_grade,
  data = anova_data,
  var.equal=FALSE
)

welchs_anova_df <- tidy(welchs_anova)

write.csv(welchs_anova_df,file="/Users/wenggeiwong/pollen_mapping_data/access_stats_results/welchs_anova_tests_bronx.csv") # can change to manhattan later

games_howell_result <- rstatix::games_howell_test(anova_data, accessibility ~ holc_grade)
write.csv(games_howell_result,file="/Users/wenggeiwong/pollen_mapping_data/access_stats_results/games_howell_tests_bronx.csv") # can change to manhattan later

districts_for_map <- districts_for_map %>%
  left_join(
    pollen_by_district,
    by = "zone_id"
  )

analysis <- districts_for_map %>%
  st_drop_geometry() %>%
  filter(
    !is.na(accessibility),
    !is.na(mean)
  ) %>% filter(holc_grade %in% c('A','B','C','D'))


analysis$holc_grade <- factor(
  analysis$holc_grade,
  levels = c("A","B","C","D")
)

ggplot(
  districts_for_map |> st_drop_geometry(),
  aes(holc_grade, accessibility)
) +
  geom_boxplot() +
  geom_jitter(width = 0.15)

cor.test(
  analysis$mean,
  analysis$accessibility,
  method = "pearson"
)

model <- lm(accessibility ~ mean, data = analysis)
summary(model)
confint(model)



analysis <- analysis %>%
  mutate(grade_ordinal = factor(holc_grade, levels = c("A","B","C","D"), ordered = TRUE),
         grade_numeric = as.numeric(grade_ordinal))  # A=1, B=2, C=3, D=4

trend_model_access <- lm(accessibility ~ grade_numeric, data = ordered_data)
summary(trend_model_access)

cor.test(analysis$grade_numeric, analysis$accessibility, method = "spearman")
cor.test(analysis$grade_numeric, analysis$mean, method = "spearman")

ggplot(data = analysis, mapping=aes(x=grade_numeric, y=accessibility)) + geom_point() + geom_smooth(method="lm", se=FALSE,color='red')
ggplot(data = analysis, mapping=aes(x=grade_numeric, y=mean)) + geom_point() + geom_smooth(method="lm", se=FALSE, color='blue')
