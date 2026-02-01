library(terra)
library(exactextractr)
library(glue)


# COMPLETED IN QGIS DUE TO LONG RUNTIME!!!
# createTreeRaster <- function(buffer_radius, input_raster='/Users/wenggeiwong/pollen_mapping_data/landcover_2010_nyc_3ft.img'){
#     tree_raster <- rast(input_raster)
#     tree_raster <- tree_raster[[1]]
#     cell_size_feet <- res(tree_raster)[1]
#     buffer_radius_feet <- buffer_radius * 3.28084
#     buffer_cells <- buffer_radius_feet / cell_size_feet

#     print(glue("Cell size: {round(cell_size_feet, 2)} feet\n"))
#     print(glue("Buffer: {buffer_radius}m = {round(buffer_radius_feet, 2)} feet = {round(buffer_cells, 2)} cells\n"))

#     w <- focalMat(tree_raster, d = buffer_cells, type = "circle")
#     print(glue("Circular weight matrix: {nrow(w)}x{ncol(w)}\n"))

#     # output_raster <- focal(tree_raster, w = w, fun = "mean", na.rm = TRUE) # in binary raster, fun=mean calculates proportion of 1s
#     # output_raster <- output_raster * 100 #from proportions to percentages
#     # return(output_raster)
    
#     output_raster <- focal(
#       tree_raster, 
#       w = w, 
#       fun = "mean", 
#       na.rm = TRUE,
#       filename = glue("/Users/wenggeiwong/pollen_mapping_data/tree_canopy_percentage_{buffer_radius}m.tif"),
#       overwrite = TRUE,
#       wopt = list(gdal = c("COMPRESS=LZW", "TILED=YES"))
#     )
# }

