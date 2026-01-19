import rasterio
import os
from dotenv import load_dotenv
import pandas as pd
import numpy as np
import geopandas as gpd
from shapely.geometry import Point
from shapely import wkt
from shapely.geometry import box
import fiona
import os

geojson_filepath = "/Users/wenggeiwong/pollen_mapping_data/HYDROGRAPHY_2008_4708806951406562133.geojson"
pollen_filepath="/Users/wenggeiwong/pollen_mapping_data/pollen.csv" 
pollen_data = pd.read_csv(pollen_filepath)
pollen_data = pollen_data[(pollen_data["Borough"]=="Manhattan") | (pollen_data["Borough"]=="Bronx")] # EPSG:4326 (latitude/longitude coordinates)


hydrography_gdf = gpd.read_file(geojson_filepath)
geometry = [Point(xy) for xy in zip(pollen_data["Longitude"], pollen_data["Latitude"])] 
points_gdf = gpd.GeoDataFrame(pollen_data,geometry=geometry,crs="EPSG:4326")
points_gdf = points_gdf.to_crs("EPSG:2263")
hydrography_gdf = hydrography_gdf.to_crs("EPSG:2263")

for i,row in points_gdf.iterrows():
    print(i)
    distances = hydrography_gdf.distance(row['geometry'])
    min_dist = round(distances.min(),4)
    print(min_dist)
    break
    # print(row.geometry)
    # distances = hydrography_gdf.distance(row['geometry'])
    # print(type(distances))
    # print(distances)
    # nearest = distances.min()
    # print(nearest)
