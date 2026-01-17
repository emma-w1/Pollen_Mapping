import rasterio
import os
from dotenv import load_dotenv
import pandas as pd
import numpy as np
import geopandas as gpd
from shapely.geometry import Point

from shapely import wkt
from shapely.geometry import box


import os



df = pd.read_csv("/Users/wenggeiwong/Pollen_Mapping/data/Borough_Boundaries_20260116.csv")
df = df[(df['BoroName']=='Manhattan') | (df['BoroName']=='Bronx')]
df['geometry'] = df['the_geom'].apply(wkt.loads)
gdf = gpd.GeoDataFrame(df, geometry='geometry', crs="EPSG:4326")
gdf = gdf.to_crs(epsg=2263)
# print(gdf.crs.axis_info[0].unit_name)
# print(type(gdf.bounds))

combined_bounds_tuple = gdf.total_bounds
# print(f"Combined bounds (tuple): {combined_bounds_tuple}")
combined_bbox_polygon = box(*combined_bounds_tuple)
# print(f"Combined bounds (Polygon WKT): {combined_bbox_polygon.wkt}")

row_names = list(gdf["BoroName"])
col_names = ["minx","miny","maxx","maxy"]
bounds_df = pd.DataFrame(gdf.bounds.values, index = gdf['BoroName'],columns=col_names)
print(bounds_df)



