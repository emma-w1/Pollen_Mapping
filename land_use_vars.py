# replicating land use regression from Weinberger et. al 2016

import pandas as pd
import rasterio
from rasterio.mask import mask
from shapely.geometry import Point, mapping
import geopandas as gpd
from pyproj import Transformer
import numpy as np



buffer_sizes = [50,100,250,500,1000]
conversion_factor = 3.280839895
buffer_sizes_feet = [buffer*conversion_factor for buffer in buffer_sizes]
output_csv_path="/Users/wenggeiwong/Pollen_Mapping/data/land_use_data/test"
pollen_filepath="/Users/wenggeiwong/Pollen_Mapping/data/pollen.csv"
tree_coverage_raster_filepath='/Users/wenggeiwong/landcover_2010_nyc_3ft.img'
pollen_data = pd.read_csv(pollen_filepath)
elevation_filepath = "/Users/wenggeiwong/NYC_DEM_1ft_Float_2/DEM_LiDAR_1ft_2010_Improved_NYC.img"
building_api_link = "https://data.cityofnewyork.us/api/v3/views/5zhs-2jue/query.json?query=SELECT%0A%20%20%60the_geom%60%2C%0A%20%20%60name%60%2C%0A%20%20%60bin%60%2C%0A%20%20%60doitt_id%60%2C%0A%20%20%60shape_area%60%2C%0A%20%20%60base_bbl%60%2C%0A%20%20%60objectid%60%2C%0A%20%20%60construction_year%60%2C%0A%20%20%60feature_code%60%2C%0A%20%20%60geom_source%60%2C%0A%20%20%60ground_elevation%60%2C%0A%20%20%60height_roof%60%2C%0A%20%20%60last_edited_date%60%2C%0A%20%20%60last_status_type%60%2C%0A%20%20%60mappluto_bbl%60%2C%0A%20%20%60shape_length%60%0AWHERE%20%60construction_year%60%20%3C%3D%202013$offset=1000$limit='1055736"


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
            pollen_data[f'tree_canopy_pct_{buffer_size}m'] = tree_canopy_pcts
        
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
    ✅✅
    Variable	Min	p25*	p50*	Mean	p75*	Max
    Tree canopy (%)						
        0.05 km	0.0	5.5	11.4	17.2	28.8	61.3
        0.1 km	0.0	6.2	15.0	18.6	28.1	65.8
        0.25 km	0.0	8.2	16.3	19.2	27.1	59.8
        0.5 km	1.8	9.0	15.2	18.0	23.0	56.4
        1 km	2.8	10.2	14.6	18.2	22.2	54.3
    '''

    '''
    ✅✅
    Variable	Min	p25*	p50*	Mean	p75*	Max
    Min elevation (feet)						
    0.05 km	-13.5	8.8	27.6	51.6	58.4	271.0
    0.1 km	-62.5	5.9	23.0	45.0	52.1	251.0
    0.25 km	-75.9	-1.9	9.8	31.7	47.0	206.2
    0.5 km	-75.9	-3.3	0.0	13.1	23.4	128.2
    1 km	-75.9	-13.4	-2.6	-2.4	0.2	78.7
    '''

    '''
    ✅✅
    Variable	Min	p25*	p50*	Mean	p75*	Max
    Mean elevation (feet)						
    0.05 km	3.2	12.5	37.4	60.6	74.1	283.5
    0.1 km	3.3	12.7	36.3	60.3	69.9	283.2
    0.25 km	6.5	13.1	37.5	60.7	71.7	280.3
    0.5 km	8.3	16.3	39.8	60.9	80.6	274.0
    1 km	8.2	17.4	44.1	58.9	84.9	255.6
    '''

    '''
    ✅✅
    Variable	Min	p25*	p50*	Mean	p75*	Max
    Max elevation (feet)						
    0.05 km	8.4	18.9	44.1	68.4	96.7	301.1
    0.1 km	10.8	25.9	48.4	74.5	106.6	322.7
    0.25 km	17.4	41.9	59.5	87.5	118.0	344.3
    0.5 km	21.1	47.6	80.1	104.3	143.5	373.1
    1 km	24.4	58.0	92.0	120.9	156.4	412.1
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
            
            pollen_data[f'elevation_max_{buffer_size/conversion_factor}m'] = max_elevations
            pollen_data[f'elevation_min_{buffer_size/conversion_factor}m'] = min_elevations
            pollen_data[f'elevation_mean_{buffer_size/conversion_factor}m'] = mean_elevations

    pollen_data.to_csv(output_csv_path, index=False)
    print(f"Results saved to {output_csv_path}")
    return pd.read_csv(output_csv_path)



     

# tree_coverage_percentage(tree_coverage_raster_filepath)
elevation_statistics(elevation_filepath)
# compareToStudy("elevation_mean")