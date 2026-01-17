# replicating land use regression from Weinberger et. al 2016

import pandas as pd
import rasterio
from rasterio.mask import mask
from shapely.geometry import Point, mapping
import geopandas as gpd
from pyproj import Transformer
import numpy as np
from sodapy import Socrata
from shapely.geometry import Point
import os
from shapely import wkt
from shapely.geometry import shape
import json
from shapely.geometry import box
import ast




buffer_sizes = [50,100,250,500,1000]
conversion_factor = 3.280839895
buffer_sizes_feet = [buffer*conversion_factor for buffer in buffer_sizes]

output_csv_path="/Users/wenggeiwong/pollen_mapping_data/land_use_data/results.csv"
pollen_filepath="/Users/wenggeiwong/pollen_mapping_data/pollen.csv"
tree_coverage_raster_filepath='/Users/wenggeiwong/landcover_2010_nyc_3ft.img'
pollen_data = pd.read_csv(pollen_filepath)
pollen_data = pollen_data[(pollen_data["Borough"]=="Manhattan") | (pollen_data["Borough"]=="Bronx")]
elevation_filepath = "/Users/wenggeiwong/NYC_DEM_1ft_Float_2/DEM_LiDAR_1ft_2010_Improved_NYC.img"
building_api_link = "https://data.cityofnewyork.us/api/v3/views/5zhs-2jue/query.json?query=SELECT%0A%20%20%60the_geom%60%2C%0A%20%20%60name%60%2C%0A%20%20%60bin%60%2C%0A%20%20%60doitt_id%60%2C%0A%20%20%60shape_area%60%2C%0A%20%20%60base_bbl%60%2C%0A%20%20%60objectid%60%2C%0A%20%20%60construction_year%60%2C%0A%20%20%60feature_code%60%2C%0A%20%20%60geom_source%60%2C%0A%20%20%60ground_elevation%60%2C%0A%20%20%60height_roof%60%2C%0A%20%20%60last_edited_date%60%2C%0A%20%20%60last_status_type%60%2C%0A%20%20%60mappluto_bbl%60%2C%0A%20%20%60shape_length%60%0AWHERE%20%60construction_year%60%20%3C%3D%202013$offset=1000$limit='1055736"
building_df_filepath = "/Users/wenggeiwong/pollen_mapping_data/land_use_data/2013_buildings.csv"
filtered_building_df_filepath = "/Users/wenggeiwong/pollen_mapping_data/land_use_data/2013_buildings_filtered.csv"

def tree_coverage_percentage(raster_filepath):
    with rasterio.open(raster_filepath) as src:
        raster_nodata = src.nodata
        raster_crs = src.crs        
        geometry = [Point(xy) for xy in zip(pollen_data['Longitude'], pollen_data['Latitude'])]
        gdf = gpd.GeoDataFrame(pollen_data, geometry=geometry, crs='EPSG:4326')
        # if coordinate refs are not same, make them
        if gdf.crs != raster_crs:
                    gdf = gdf.to_crs(raster_crs)
        # iterate through each possible buffer size          
        for buffer_size in buffer_sizes_feet:

            # create buffer
            gdf['buffer'] = gdf.geometry.buffer(buffer_size)
            tree_canopy_pcts = []
            # iterate thorugh each point
            for i, row in gdf.iterrows():
                try:
                    # extract raster values within buffer
                    
                    buffer_geom = [mapping(row['buffer'])]
                    original_nodata = src.nodata
                    out_image, out_transform = mask(src, buffer_geom, crop=True, filled=False)
                    
                    # get first band
                    band_data = out_image[0]

                    # remove nodata values based on original nodata value
                    if np.ma.is_masked(band_data):
                        valid_data = band_data.compressed()  # Gets only non-masked values
                    else:
                        # If not masked, filter by nodata value
                        if raster_nodata is not None:
                            valid_data = band_data[band_data != raster_nodata]
                        else:
                            valid_data = band_data.flatten()
                    
                    if len(valid_data) == 0:
                        tree_canopy_pcts.append(0.0)
                        print(f"null data for point {i} at buffer {buffer_size}m")
                        continue
                    
                    # count pixels with value = 1 (tree canopy)
                    tree_pixels = np.sum(valid_data == 1)
                    total_pixels = len(valid_data)
                    
                    # calc percentage
                    pct = (tree_pixels / total_pixels) * 100 if total_pixels > 0 else 0.0
                    tree_canopy_pcts.append(round(pct, 2))
                except Exception as e:
                    print(f"Error processing point {i} at buffer {buffer_size}m: {str(e)}")
                    tree_canopy_pcts.append(np.nan)
            pollen_data[f'tree_canopy_pct_{int(buffer_size/conversion_factor)}m'] = tree_canopy_pcts
        
    pollen_data.to_csv(output_csv_path, index=False)
    print(f"Results saved to {output_csv_path}")
    return pd.read_csv(output_csv_path)

def compareToStudy(variable):
    data = pd.read_csv(output_csv_path)
    # data = tree_coverage_percentage()

    # to compare with paper findings: 
    for buffer in buffer_sizes:
        print(f"{buffer} m: min - {data[f'{variable}_{buffer}m'].min()}, p25 - {np.percentile(data[f'{variable}_{buffer}m'],25)}, p50 - {np.percentile(data[f'{variable}_{buffer}m'],50)}, mean - {data[f'{variable}_{buffer}m'].mean()}, p75 - {np.percentile(data[f'{variable}_{buffer}m'],75)}, max - {data[f'{variable}_{buffer}m'].max()}")
    # print(f"50 m: min - {data['tree_canopy_pct_50m'].min()}, p25 - {np.percentile(data['tree_canopy_pct_50m'],25)}, p50 - {np.percentile(data['tree_canopy_pct_50m'],50)}, mean - {data['tree_canopy_pct_50m'].mean()}, p75 - {np.percentile(data['tree_canopy_pct_50m'],75)}, max - {data['tree_canopy_pct_50m'].max()}")
    # print(f"100 m: min - {data['tree_canopy_pct_100m'].min()}, p25 - {np.percentile(data['tree_canopy_pct_100m'],25)}, p50 - {np.percentile(data['tree_canopy_pct_100m'],50)}, mean - {data['tree_canopy_pct_100m'].mean()}, p75 - {np.percentile(data['tree_canopy_pct_100m'],75)}, max - {data['tree_canopy_pct_100m'].max()}")
    # print(f"250 m: min - {data['tree_canopy_pct_250m'].min()}, p25 - {np.percentile(data['tree_canopy_pct_250m'],25)}, p50 - {np.percentile(data['tree_canopy_pct_250m'],50)}, mean - {data['tree_canopy_pct_250m'].mean()}, p75 - {np.percentile(data['tree_canopy_pct_250m'],75)}, max - {data['tree_canopy_pct_250m'].max()}")
    # print(f"500 m: min - {data['tree_canopy_pct_500m'].min()}, p25 - {np.percentile(data['tree_canopy_pct_500m'],25)}, p50 - {np.percentile(data['tree_canopy_pct_500m'],50)}, mean - {data['tree_canopy_pct_500m'].mean()}, p75 - {np.percentile(data['tree_canopy_pct_500m'],75)}, max - {data['tree_canopy_pct_500m'].max()}")
    # print(f"1000 m: min - {data['tree_canopy_pct_1000m'].min()}, p25 - {np.percentile(data['tree_canopy_pct_1000m'],25)}, p50 - {np.percentile(data['tree_canopy_pct_1000m'],50)}, mean - {data['tree_canopy_pct_1000m'].mean()}, p75 - {np.percentile(data['tree_canopy_pct_1000m'],75)}, max - {data['tree_canopy_pct_1000m'].max()}")

    '''
    ✅
    Variable	Min	p25*	p50*	Mean	p75*	Max
    Tree canopy (%)						
        0.05 km	0.0	5.5	11.4	17.2	28.8	61.3
        0.1 km	0.0	6.2	15.0	18.6	28.1	65.8
        0.25 km	0.0	8.2	16.3	19.2	27.1	59.8
        0.5 km	1.8	9.0	15.2	18.0	23.0	56.4
        1 km	2.8	10.2	14.6	18.2	22.2	54.3
    '''

    '''
    ✅
    Variable	Min	p25*	p50*	Mean	p75*	Max
    Min elevation (feet)						
    0.05 km	-13.5	8.8	27.6	51.6	58.4	271.0
    0.1 km	-62.5	5.9	23.0	45.0	52.1	251.0
    0.25 km	-75.9	-1.9	9.8	31.7	47.0	206.2
    0.5 km	-75.9	-3.3	0.0	13.1	23.4	128.2
    1 km	-75.9	-13.4	-2.6	-2.4	0.2	78.7
    '''

    '''
    ✅
    Variable	Min	p25*	p50*	Mean	p75*	Max
    Mean elevation (feet)						
    0.05 km	3.2	12.5	37.4	60.6	74.1	283.5
    0.1 km	3.3	12.7	36.3	60.3	69.9	283.2
    0.25 km	6.5	13.1	37.5	60.7	71.7	280.3
    0.5 km	8.3	16.3	39.8	60.9	80.6	274.0
    1 km	8.2	17.4	44.1	58.9	84.9	255.6
    '''

    '''
    ✅
    Variable	Min	p25*	p50*	Mean	p75*	Max
    Max elevation (feet)						
    0.05 km	8.4	18.9	44.1	68.4	96.7	301.1
    0.1 km	10.8	25.9	48.4	74.5	106.6	322.7
    0.25 km	17.4	41.9	59.5	87.5	118.0	344.3
    0.5 km	21.1	47.6	80.1	104.3	143.5	373.1
    1 km	24.4	58.0	92.0	120.9	156.4	412.1
    '''

    '''
    Distributed building height (meters)						
    0.05 km	0.0	0.8	2.7	11.3	7.2	139.3
    0.1 km	0.0	1.0	3.2	10.2	7.7	94.0
    0.25 km	0.0	1.4	3.5	8.3	6.8	54.6
    0.5 km	0.1	1.6	3.4	8.3	6.6	41.2
    1 km	0.0	1.4	3.4	6.8	6.2	34.2
    '''

def elevation_statistics(raster_filepath):
    with rasterio.open(raster_filepath) as src:
        raster_crs = src.crs
        raster_nodata = src.nodata # value of null point from metadata

        geometry = [Point(xy) for xy in zip(pollen_data['Longitude'], pollen_data['Latitude'])]
        gdf = gpd.GeoDataFrame(pollen_data, geometry=geometry, crs='EPSG:4326')

        if gdf.crs != raster_crs:
            gdf = gdf.to_crs(raster_crs)

        # find point elevations:
        point_elevations = []
        for i, row in gdf.iterrows():
            try:
                for val in src.sample([(row.geometry.x, row.geometry.y)]): 
                    elevation = val[0]
                    if raster_nodata is not None and elevation == raster_nodata: # check if elevation is null
                        point_elevations.append(np.nan)
                    else:
                        point_elevations.append(float(elevation))
            except Exception as e:
                print(f"Error extracting elevation for point {i}: {str(e)}")
                point_elevations.append(np.nan)

        pollen_data['point_elevation'] = point_elevations


        for buffer_size in buffer_sizes_feet:
            gdf['buffer'] = gdf.geometry.buffer(buffer_size)

            max_elevations = []
            min_elevations = []
            mean_elevations = []

            for i, row in gdf.iterrows():
                try:
                    buffer_geom = [mapping(row['buffer'])]
                    out_image, out_transform = mask(src, buffer_geom, crop=True, filled=False)
                    band_data = out_image[0]
                    
                    if np.ma.is_masked(band_data):
                            valid_data = band_data.compressed()  # Gets only non-masked values
                    else:
                        # If not masked, filter by nodata value
                        if raster_nodata is not None:
                            valid_data = band_data[band_data != raster_nodata]
                        else:
                            valid_data = band_data.flatten()
                    
                    if len(valid_data) == 0:
                            max_elevations.append(np.nan)
                            min_elevations.append(np.nan)
                            mean_elevations.append(np.nan)
                            print(f"Warning: No valid elevation data for point {i} at buffer {buffer_size}m")
                            continue
                            
                    max_elev = float(np.max(valid_data))
                    min_elev = float(np.min(valid_data))
                    mean_elev = float(np.mean(valid_data))
                    max_elevations.append(round(max_elev, 2))
                    min_elevations.append(round(min_elev, 2))
                    mean_elevations.append(round(mean_elev, 2))
                except Exception as e:
                    print(f"Error processing point {i} at buffer {buffer_size}m: {str(e)}")
                    max_elevations.append(np.nan)
                    min_elevations.append(np.nan)
                    mean_elevations.append(np.nan)
            
            pollen_data[f'elevation_max_{int(buffer_size/conversion_factor)}m'] = max_elevations
            pollen_data[f'elevation_min_{int(buffer_size/conversion_factor)}m'] = min_elevations
            pollen_data[f'elevation_mean_{int(buffer_size/conversion_factor)}m'] = mean_elevations

    pollen_data.to_csv(output_csv_path, index=False)
    print(f"Results saved to {output_csv_path}")
    return pd.read_csv(output_csv_path)

def buildings2013():
    
    domain = "data.cityofnewyork.us"
    client = Socrata(domain, os.getenv("OPENDATA_APP_TOKEN"))
    offset = 0
    limit = 50000

    # fetch from regular dataset
    all_current = []

    while True:
        try:
            # done in batches since dataset is too large
            current_batch = client.get(
                "5zhs-2jue", # endpoint
                where=f"construction_year <= '{2013}'",
                limit = limit,
                offset = offset
            )
            if not current_batch:
                break

            all_current.extend(current_batch)
            offset += limit
            print("...")
        except Exception as e:
            print(f"Error fetching current batch at offset {offset}: {e}")
            break

    current_df = pd.DataFrame.from_records(all_current)
    print(f"Total fetched buildings w/ construction date <= 2013: {len(current_df)}")

    # fetch from historic dataset
    all_demolished = []
    offset = 0
    used_fallback = False
    while True:
        try:
            where_clause = (
                        f"construction_year <= '2013' AND "
                        f"(demolition_year > '2013' OR demolition_year IS NULL)"
                    )
            
            demolished_batch = client.get(
                "ipkp-snf6", # endpoint
                where = where_clause,
                limit=limit,
                offset=offset
            )

            if not demolished_batch:
                break

            all_demolished.extend(demolished_batch)
            offset += limit
        
        except Exception as e:
            print(f"Error fetching demolished batch at offset {offset}: {e}")
            # try another way by breaking up where statements
            print(f"Falling back to client-side filtering...")

            demolished_batch = client.get(
                    "ipkp-snf6",
                    where=f"construction_year <= '{2013}'",
                    limit=limit,
                    offset=offset
                )
            
            if not demolished_batch:
                    break
            all_demolished.extend(demolished_batch)
            offset += limit
            used_fallback = True
        
    demolished_df = pd.DataFrame.from_records(all_demolished)

    if used_fallback and "demolition_year" in demolished_df.columns:
        # if count is same after filtering, it didn't work, so filter
        original_count = len(demolished_df)  

        demolished_df['demolition_year'] = pd.to_numeric(demolished_df['demolition_year'], errors='coerce')
        demolished_df = demolished_df[
            (demolished_df["demolition_year"].isna()) | 
            (demolished_df['demolition_year'] > 2013)
        ]
        
        # Compare counts
        if len(demolished_df) < original_count:
            # If count decreased, we did client-side filtering
            print(f"Client-side filtered: {original_count} -> {len(demolished_df)}")
    
    print(f"Demolished buildings that existed in 2013: {len(demolished_df)}")

    all_buildings_2013 = pd.concat([current_df, demolished_df], ignore_index=True)
    print(f"\nTotal buildings in 2013 dataset: {len(all_buildings_2013)}")

    client.close()
    all_buildings_2013.to_csv(building_df_filepath)
    return all_buildings_2013

# only uses buildings from manhattan or bronx
def filter_buildingsdf(buildings_df):
        borough_df = pd.read_csv("/Users/wenggeiwong/pollen_mapping_data/Borough_Boundaries_20260116.csv")
        borough_df = borough_df[(borough_df['BoroName']=='Manhattan') | (borough_df['BoroName']=='Bronx')]
        borough_df['geometry'] = borough_df['the_geom'].apply(wkt.loads)
        borough_gdf = gpd.GeoDataFrame(borough_df, geometry='geometry', crs="EPSG:2263")
        # print(gdf.crs.axis_info[0].unit_name)
        # print(type(gdf.bounds))

        combined_bounds_tuple = borough_gdf.total_bounds
        # print(f"Combined bounds (tuple): {combined_bounds_tuple}")
        combined_bbox_polygon = box(*combined_bounds_tuple)
        print(f"Combined bounds (Polygon WKT): {combined_bbox_polygon.wkt}")

        def load_geometry(geojson_data):
            return shape(ast.literal_eval(geojson_data) )
        
        buildings_df['geometry'] = buildings_df['the_geom'].apply(load_geometry)
        buildings_df = buildings_df.dropna(subset=['geometry'])

        buildings_gdf = gpd.GeoDataFrame(buildings_df,geometry='geometry', crs="EPSG:2263")
        filtered_buildings_gdf = buildings_gdf[buildings_gdf.intersects(combined_bbox_polygon)]
        print("Finished filtering")
        filtered_buildings_df = pd.DataFrame(filtered_buildings_gdf)
        filtered_buildings_df['the_geom'] = filtered_buildings_gdf.geometry.apply(lambda geom: geom.wkt)
        filtered_buildings_df.to_csv(filtered_building_df_filepath, index=False)
        print(f"length of unfiltered: {len(buildings_df)}, length of filtered: {len(filtered_buildings_df)}")
        return filtered_buildings_df

def volume_buildings_gdf():
    def get_geometry(geom_str):
        try:
            return wkt.loads(geom_str)
        except:
            try:
                return shape(json.loads(geom_str))
            except:
                return None
            

    buildings_df = pd.read_csv(filtered_building_df_filepath)
    
    print("Parsing geometries...")
    buildings_df["geometry"] = buildings_df["the_geom"].apply(get_geometry)
    buildings_df = buildings_df[buildings_df["geometry"].notna()]
    print(f"Valid geometries: {len(buildings_df)}",end="/r")
    print("Creating GeoDataFrame...")
    buildings_gdf = gpd.GeoDataFrame(buildings_df, geometry='geometry', crs="EPSG:2263") # in feet
    
    print(f"Loaded {len(buildings_gdf)} buildings")
    print(f"CRS: {buildings_gdf.crs}")
    print(f"Sample geometry: {buildings_gdf.iloc[0].geometry}")
    print(f"Sample bounds: {buildings_gdf.iloc[0].geometry.bounds}")

    # volume calculations
    print("Calculating heights...")
    buildings_gdf["height"] = pd.to_numeric(buildings_gdf["height_roof"], errors='coerce') # height_roof is height from ground
    buildings_gdf = buildings_gdf[buildings_gdf["height"].notna() & (buildings_gdf['height'] > 0)]
    print("Calculating volumes...")
    buildings_gdf["area"] = buildings_gdf.geometry.area
    buildings_gdf["volume"] = buildings_gdf["area"]* buildings_gdf["height"]

    print(f"Buildings with valid geometry and height: {len(buildings_gdf)}")
    print("Finished volume_buildings_gdf()")
    return buildings_gdf

def volume_per_buffer(buildings_gdf):
    print("Starting volume_per_buffer()...")  # ADD
    print(f"Buildings GDF has {len(buildings_gdf)} buildings")  # ADD
    print("Creating points GeoDataFrame...")
    geometry = [Point(xy) for xy in zip(pollen_data["Longitude"],pollen_data["Latitude"])]
    points_gdf = gpd.GeoDataFrame(pollen_data,geometry=geometry,crs="EPSG:4326") # meters
    print(f"Created {len(points_gdf)} points")
    print("Reprojecting points...")  
    if points_gdf.crs != buildings_gdf.crs:
        points_gdf = points_gdf.to_crs(buildings_gdf.crs)
    print("Reprojection complete")  # ADD
    print("Creating spatial index...")  # ADD
    # spacial index allows us to assign buildings to buffer more efficiently
    buildings_sindex = buildings_gdf.sindex
    print("Spatial index created")  # ADD

    # assign for each buffer

    for buffer_size_m in buffer_sizes:
        print(f"\n{'='*50}")  # ADD
        buffer_size_feet = buffer_size_m * conversion_factor # radius in ft, needs to be used for EPSG:2263
        print(f"Creating buffers of {buffer_size_feet:.2f} feet...")  # ADD
        buffer_area_m2 = np.pi * (buffer_size_m**2) # area in meters

        points_gdf["buffer"] = points_gdf.geometry.buffer(buffer_size_feet)

        volume_densities = []

        # for each point in pollen survey
        for i, row in points_gdf.iterrows():
            if i % 5 == 0:  # ADD - print every 5 points
                print(f"  Processing point {i}/{len(points_gdf)}...")
            try:
                buffer_geom = row['buffer']

                # check for possible intersections against all buildings for more efficient runtime
                possible_i = list(buildings_sindex.intersection(buffer_geom.bounds)) # checks if bounding boxes of spatial index overlaps with those of buffer
                possible_matches = buildings_gdf.iloc[possible_i]
                # check for preciuse intersections

                intersections = possible_matches[possible_matches.intersects(buffer_geom)]

                if len(intersections) == 0:
                    print("no intersections (your code is def wrong)")
                    volume_densities.append(0.0)
                    continue

                # take sum of all building volumes in buffer
                total_volume_ft3 = intersections['volume'].sum()

                ft3_to_m3 = 35.31469989 # 35.31469989 ft3 per m3

                total_volume_m3 = total_volume_ft3/ft3_to_m3

                density = total_volume_m3 / buffer_area_m2

                volume_densities.append(round(density,4))

            except Exception as e:
                print(f"Error processing point {i} at buffer {buffer_size_m}m: {str(e)}")
                volume_densities.append(np.nan)
            
        print(f"\n  Completed {buffer_size_m}m buffer")

        pollen_data[f'building_vol_density_{buffer_size_m}m'] = volume_densities
    
    pollen_data.to_csv(output_csv_path, index=False)
    print(f"\nResults saved to {output_csv_path}")
    return pollen_data

        
     

# tree_coverage_percentage(tree_coverage_raster_filepath)
# elevation_statistics(elevation_filepath)
# compareToStudy("tree_canopy_pct")

print("running......")
gdf = volume_buildings_gdf()
print("gdf created")
volume_per_buffer(gdf)
print("done!")

# compareToStudy('building_vol_density')