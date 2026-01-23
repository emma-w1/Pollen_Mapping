import geopandas as gpd
import pandas as pd
from shapely import wkt

borough_df = pd.read_csv("/Users/wenggeiwong/pollen_mapping_data/Borough_Boundaries_20260116.csv") # EPSG:4362
borough_df = borough_df[(borough_df['BoroName']=='Manhattan') | (borough_df['BoroName']=='Bronx')]
borough_df['geometry'] = borough_df['the_geom'].apply(wkt.loads)
borough_gdf = gpd.GeoDataFrame(borough_df, geometry='geometry', crs="EPSG:4362")

combined_bounds_tuple = borough_gdf.total_bounds
print(combined_bounds_tuple)