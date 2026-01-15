import rasterio

with rasterio.open("/Users/wenggeiwong/Pollen_Mapping/data/land_use_data/NYC_DEM_1ft_Float_2/DEM_LiDAR_1ft_2010_Improved_NYC.img") as src:
    print(f"CRS: {src.crs}")
    print(f"Units: {src.crs.linear_units}")