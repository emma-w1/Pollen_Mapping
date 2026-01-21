# reproducing variables for land use regression model from Weinberger et. al 2016

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

output_csv_path="/Users/wenggeiwong/pollen_mapping_data/land_use_data/results.csv" # EPSG:4326 (latitude/longitude coordinates)
tree_coverage_raster_filepath='/Users/wenggeiwong/pollen_mapping_data/landcover_2010_nyc_3ft.img' # EPSG:2263

pollen_filepath="/Users/wenggeiwong/pollen_mapping_data/pollen.csv" 
pollen_data = pd.read_csv(pollen_filepath) # EPSG:4326 (latitude/longitude coordinates)
pollen_data = pollen_data[(pollen_data["Borough"]=="Manhattan") | (pollen_data["Borough"]=="Bronx")]

elevation_filepath = "/Users/wenggeiwong/NYC_DEM_1ft_Float_2/DEM_LiDAR_1ft_2010_Improved_NYC.img" # EPSG:2263
building_df_filepath = "/Users/wenggeiwong/pollen_mapping_data/land_use_data/2013_buildings.csv" #EPSG:4326
filtered_building_df_filepath = "/Users/wenggeiwong/pollen_mapping_data/land_use_data/2013_buildings_filtered.csv" #EPSG:4326
nyc_planimetrics_filepath = "/Users/wenggeiwong/pollen_mapping_data/HYDROGRAPHY_2008_4708806951406562133.geojson" #ESPG:2263

def tree_coverage_percentage(raster_filepath):
    with rasterio.open(raster_filepath) as src:
        raster_nodata = src.nodata
        raster_crs = src.crs        
        geometry = [Point(xy) for xy in zip(pollen_data['Longitude'], pollen_data['Latitude'])]
        gdf = gpd.GeoDataFrame(pollen_data, geometry=geometry, crs='EPSG:4326')

        if gdf.crs != raster_crs:
                    gdf = gdf.to_crs(raster_crs) # converts to EPSG:2263
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
    print(f"Tree canopy results saved to {output_csv_path}")
    return pd.read_csv(output_csv_path)

def compare_to_study_buffer(variable):
    data = pd.read_csv(output_csv_path)
    # data = tree_coverage_percentage()
    print(variable)
    # to compare with paper findings: 
    for buffer in buffer_sizes:
        print(f"{buffer} m: min - {data[f'{variable}_{buffer}m'].min()}, p25 - {np.percentile(data[f'{variable}_{buffer}m'],25)}, p50 - {np.percentile(data[f'{variable}_{buffer}m'],50)}, mean - {data[f'{variable}_{buffer}m'].mean()}, p75 - {np.percentile(data[f'{variable}_{buffer}m'],75)}, max - {data[f'{variable}_{buffer}m'].max()}")
    print("\n")

def compare_to_study_point(variable):
    data = pd.read_csv(output_csv_path)
    print(variable)
    print(f"min - {data[variable].min()}, p25 - {np.percentile(data[variable],25)}, p50 - {np.percentile(data[variable],50)}, mean - {data[variable].mean()}, p75 - {np.percentile(data[variable],75)}, max - {data[variable].max()}")
    print("\n")

def elevation_statistics(raster_filepath):
    with rasterio.open(raster_filepath) as src:
        raster_crs = src.crs
        raster_nodata = src.nodata # value of null point from metadata

        geometry = [Point(xy) for xy in zip(pollen_data['Longitude'], pollen_data['Latitude'])]
        gdf = gpd.GeoDataFrame(pollen_data, geometry=geometry, crs='EPSG:4326')

        gdf = gdf.to_crs("EPSG:2263")

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
    print(f"Elevation results saved to {output_csv_path}")
    return pd.read_csv(output_csv_path)

#create first buildings dataset using API and assign to buildings_df_pathname
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
                where=f"construction_year < '{2013}'",
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
    print(f"Total fetched buildings w/ construction date < 2013: {len(current_df)}")

    # fetch from historic dataset
    all_demolished = []
    offset = 0
    used_fallback = False
    while True:
        try:
            where_clause = (
                        f"construction_year < '2013' AND "
                        f"(demolition_year >= '2013' OR demolition_year IS NULL)"
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
                    where=f"construction_year < '{2013}'",
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
            (demolished_df['demolition_year'] >= 2013)
        ]

    all_buildings_2013 = pd.concat([current_df, demolished_df], ignore_index=True)
    print(f"\nTotal buildings in 2013 dataset: {len(all_buildings_2013)}")

    client.close()
    all_buildings_2013.to_csv(building_df_filepath)
    print(f'All buildings from 2013 results not saved to {building_df_filepath}; not altered when run')
    return all_buildings_2013

# filter buildings by borough and assing to filtered_buildings_df_pathname
def filter_buildingsdf(buildings_df):
        # filter to manhattan/bronx bounding box
        borough_df = pd.read_csv("/Users/wenggeiwong/pollen_mapping_data/Borough_Boundaries_20260116.csv") # EPSG:4362
        borough_df = borough_df[(borough_df['BoroName']=='Manhattan') | (borough_df['BoroName']=='Bronx')]
        borough_df['geometry'] = borough_df['the_geom'].apply(wkt.loads)
        borough_gdf = gpd.GeoDataFrame(borough_df, geometry='geometry', crs="EPSG:4362")

        combined_bounds_tuple = borough_gdf.total_bounds
        combined_bbox_polygon = box(*combined_bounds_tuple)

        def load_geometry(geojson_data):
            return shape(ast.literal_eval(geojson_data) )
        
        buildings_df['geometry'] = buildings_df['the_geom'].apply(load_geometry)
        buildings_df = buildings_df.dropna(subset=['geometry'])

        buildings_gdf = gpd.GeoDataFrame(buildings_df,geometry='geometry', crs="EPSG:4362")
        filtered_buildings_gdf = buildings_gdf[buildings_gdf.intersects(combined_bbox_polygon)]
        filtered_buildings_df = pd.DataFrame(filtered_buildings_gdf)
        filtered_buildings_df['the_geom'] = filtered_buildings_gdf.geometry.apply(lambda geom: geom.wkt)
        filtered_buildings_df.to_csv(filtered_building_df_filepath, index=False)
        print(f"Finished filtering! length of unfiltered: {len(buildings_df)}, length of filtered: {len(filtered_buildings_df)}")
        print(f"Filtered buildings results not saved to {filtered_building_df_filepath}; method already run")
        return filtered_buildings_df # returns in EPSG:4326

 # create and return gdf with volume, area, height for future analysis 
def volume_buildings_gdf(filtered_buildings_df):

    def get_geometry(geom_str):
        try:
            return wkt.loads(geom_str)
        except:
            pass
        try:
            return shape(json.loads(geom_str))
        except:
            pass
        try:
            return shape(ast.literal_eval(geom_str))
        except:
            return None



    filtered_buildings_df = filtered_buildings_df[filtered_buildings_df["geometry"].notna()]
    filtered_buildings_df["geometry"] = filtered_buildings_df["the_geom"].apply(get_geometry)
    filtered_buildings_gdf = gpd.GeoDataFrame(filtered_buildings_df, geometry='geometry', crs="EPSG:4326") 

    # reprojecting because we need to find area in feet
    filtered_buildings_gdf = filtered_buildings_gdf.to_crs("EPSG:2263")

    # volume calculations
    filtered_buildings_gdf["height"] = pd.to_numeric(filtered_buildings_gdf["height_roof"], errors='coerce') # height_roof is height from ground in feet
    filtered_buildings_gdf = filtered_buildings_gdf[filtered_buildings_gdf["height"].notna() & (filtered_buildings_gdf['height'] > 0)]
    filtered_buildings_gdf["area"] = filtered_buildings_gdf["area"] = filtered_buildings_gdf.geometry.area # in sq feet
    filtered_buildings_gdf["volume"] = filtered_buildings_gdf["area"]* filtered_buildings_gdf["height"] # in cubic feet

    print(f"Buildings with valid geometry and height: {len(filtered_buildings_gdf)}")
    print("Finished creating filtered_buildings_gdf")
    return filtered_buildings_gdf # in EPSG:4326


# find density of volume per buffer and add to final df
def volume_per_buffer(buildings_gdf):
    geometry = [Point(xy) for xy in zip(pollen_data["Longitude"],pollen_data["Latitude"])]
    points_gdf = gpd.GeoDataFrame(pollen_data,geometry=geometry,crs="EPSG:4326") 

    # convert to EPSG:2263 because our buffer size is in feet
    points_gdf = points_gdf.to_crs("EPSG:2263") 
    buildings_gdf = buildings_gdf.to_crs("EPSG:2263")


    # spacial index allows us to assign buildings to buffer more efficiently
    buildings_sindex = buildings_gdf.sindex

    # assign for each buffer

    for buffer_size_m in buffer_sizes:
        print(f"\n{'='*50}")  # ADD
        buffer_size_feet = buffer_size_m * conversion_factor # radius in ft, needs to be used for EPSG:2263
        print(f"Creating buffers of {buffer_size_m} m...")  # ADD
        buffer_area_m2 = np.pi * (buffer_size_m**2) # area in kilometers

        points_gdf["buffer"] = points_gdf.geometry.buffer(buffer_size_feet)

        volume_densities = []

        # for each point in pollen survey
        for i, row in points_gdf.iterrows():
            buffer_geom = row['buffer']

            possible_i = list(buildings_sindex.intersection(buffer_geom.bounds)) # checks if bounding boxes of spatial index overlaps with those of buffer
            possible_matches = buildings_gdf.iloc[possible_i]
            # check for precise intersections (either fractional / centroid method) - fractional method has a lower MAE (see additional_info.txt)
            
            # intersections = possible_matches[possible_matches.centroid.within(buffer_geom)] used for centroid method
            intersections = possible_matches[possible_matches.intersects(buffer_geom)]

            total_volume_ft3 = 0.0
            
            for i, building in intersections.iterrows():
                try:
                    intersection_geom = buffer_geom.intersection(building.geometry)
                    intersection_area_ft2 = intersection_geom.area
                    building_area_ft2 = building['area']

                    if building_area_ft2 > 0:
                        fraction = intersection_area_ft2/building_area_ft2

                        proportional_vol_ft3 = fraction * building['volume']
                        total_volume_ft3 += proportional_vol_ft3
                except Exception as e:
                    continue

            if len(intersections) == 0:
                print("No intersections for this buffer?")
                volume_densities.append(0.0)
                continue

            # take sum of all building volumes in buffer
            # total_volume_ft3 = intersections['volume'].sum() for centroid method
            ft3_to_m3 = 35.31469989 # 35.31469989 ft3 per m3
            total_volume_m3 = total_volume_ft3/ft3_to_m3
            density = total_volume_m3 / buffer_area_m2
            print(f"density: {density}m")
            volume_densities.append(round(density,4))

        print(f"\n  Completed {buffer_size_m}m buffer")

        pollen_data[f'building_vol_density_{buffer_size_m}m'] = volume_densities
    
    pollen_data.to_csv(output_csv_path, index=False)
    print(f"\nBuilding density results saved to {output_csv_path}")
    return pollen_data

# finds the distance to nearest body of water, adds column to output, returns & saved pollen_data to file
def distWater(geojson_filepath): 
    hydrography_gdf = gpd.read_file(geojson_filepath, crs="EPSG:2263")
    geometry = [Point(xy) for xy in zip(pollen_data["Longitude"], pollen_data["Latitude"])] 
    points_gdf = gpd.GeoDataFrame(pollen_data,geometry=geometry,crs="EPSG:4326")
    points_gdf = points_gdf.to_crs("EPSG:2263")
    hydrography_gdf = hydrography_gdf.to_crs("EPSG:2263")
    
    min_distances = []

    for i, row in points_gdf.iterrows():
        current_point = row['geometry']
        current_point_4326 = gpd.GeoSeries(current_point, crs="EPSG:2263").to_crs("EPSG:4326").iloc[0]
        longitude = current_point_4326.x
        latitude = current_point_4326.y

        distances = hydrography_gdf.distance(current_point)
        min_distance = round(min(distances),4)
        # print(f'Distance to nearest body of water at ({longitude}, {latitude}): {min_distance}')
        min_distances.append(min_distance)

    pollen_data['distance_water'] = min_distances
    pollen_data.to_csv(output_csv_path)
    print(f"\nWater body distance results saved to {output_csv_path}")
    return pollen_data



        
     
print("running......")
if input("say enter yes to continue:\n") == "yes":

    # # add elevation / tree cover statistics to final output
    # tree_coverage_percentage(tree_coverage_raster_filepath)
    # elevation_statistics(elevation_filepath)
    # distWater(nyc_planimetrics_filepath)

    # # buildings2013() #create first buildings dataset using API and assign to buildings_df_pathname, not altering file for now
    # buildings_df = pd.read_csv(building_df_filepath)
    # # filter_buildingsdf(buildings_df) # filter buildings by borough and assing to filtered_buildings_df_pathname, not altering file for now
    # filtered_buildings_df = pd.read_csv(filtered_building_df_filepath)
    # filtered_buildings_gdf = volume_buildings_gdf(filtered_buildings_df) # create and return gdf with volume, area, height for future analysis 
    # volume_per_buffer(filtered_buildings_gdf) # find density of volume per buffer and add to final df
    
    # previous lines comment out because results.csv contains all necessary data

    compare_to_study_point("distance_water")
    compare_to_study_buffer("tree_canopy_pct")
    compare_to_study_point("point_elevation")
    compare_to_study_buffer("elevation_max")
    compare_to_study_buffer("elevation_min")
    compare_to_study_buffer("elevation_mean")
    compare_to_study_buffer("building_vol_density")

    print("done!")
    print("after code completes make sure to comment method calls to buildings2013 and filter_buildingsdf to reduce future runtime")





