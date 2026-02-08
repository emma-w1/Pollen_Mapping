import rasterio
from rasterio.plot import show
import matplotlib.pyplot as plt
import geopandas as gpd
from rasterio.mask import mask
from rasterstats import zonal_stats
import os
import pandas as pd

manhattan_map = gpd.read_file("/Users/wenggeiwong/pollen_mapping_data/redlining_sfs/manh_holc_sf.json", engine="fiona")
bronx_map = gpd.read_file('/Users/wenggeiwong/pollen_mapping_data/redlining_sfs/brx_holc_sf.json', engine="fiona")


def find_year(raster):
        filename = os.path.basename(raster)
        year = filename.split("_")[0]
        digits_list = [char for char in year if char.isdigit()]
        year = "".join(digits_list)
        year = int(year)
        year = year + 2008
        return year
    
def find_pollutant(raster):
    filename = os.path.basename(raster)
    pollutant = filename.split("_")[-1] 
    pollutant = pollutant.replace("300m","").upper()
    if pollutant == "PM":
        pollutant = "PM2.5"
    return pollutant

def find_pollen(raster):
    filename = os.path.basename(raster)
    pollen_type = filename.split("_")

    if pollen_type[2] == "alrg":
        pollen_type = f"{pollen_type[0]}_{pollen_type[1]}_{pollen_type[2]}"
    else:
        pollen_type = f"{pollen_type[0]}_{pollen_type[1]}"

    return pollen_type

def find_borough(raster):
    filename = os.path.basename(raster)
    filename = filename.split("_")
    if(filename[0]=="bronx"):
        return "bronx"
    elif(filename[0]=="manhattan"):
        return "manhattan"
    else:
        return ""



# Create avg. maps
def joinData(shapefile,raster):
    print("HIIIII")
    # year = find_year(raster)
    pollen_type = find_pollen(raster)    

    with rasterio.open(raster) as src:
        data = src.read(1).astype("float32")  # Convert to float
        profile = src.profile

        gdf = shapefile.to_crs(src.crs)
        shapes = [feature["geometry"] for feature in gdf.__geo_interface__["features"]]
        clipped_data, clipped_transform = mask(
            dataset=src,
            shapes=shapes,
            crop=True,
            filled=True,
            nodata=src.nodata
        )

    clipped_data = clipped_data.astype("float32")

    stats = zonal_stats(
        gdf,
        clipped_data[0],
        affine=clipped_transform,
        stats=["mean", "median", "max", "min", "std","range","sum"]
    )

    zone_id_list = [row.label for _, row in gdf.iterrows()]
    # to add stats later check rasterio documentation (include add_stats argument in zonal_stats method)
    mean_list = [s["mean"] for s in stats]
    median_list = [s["median"] for s in stats]
    max_list = [s["max"] for s in stats]
    min_list = [s["min"] for s in stats]
    std_list = [s["std"] for s in stats]
    range_list = [s["range"] for s in stats]
    sum_list = [s["sum"] for s in stats]

    # Stats df to export as csv later
    dict_stats = {"zone_id": zone_id_list,
                  "mean": mean_list,
                  "median": median_list,
                  "max": max_list,
                  "min": min_list,
                  "std": std_list,
                  "range": range_list,
                  "sum": sum_list}
    
    df_stats = pd.DataFrame(dict_stats)
    print(df_stats)

    gdf["mean"] = mean_list
    gdf["median"] = median_list
    gdf["max"] = max_list
    gdf["min"] = min_list

    for i, stat in enumerate(stats):
        for key, value in stat.items():
            gdf.loc[i, f"pollen_{key}"] = value

    summary = gdf.groupby("grade")["mean"].mean()
    print(summary)




    # Plot
    gdf["label_point"] = gdf.geometry.representative_point()
    fig, ax = plt.subplots(figsize=(10, 10))

    gdf.plot(
        column="mean",
        cmap="OrRd",
        linewidth=0.3,
        edgecolor="black",
        legend=True,
        ax=ax
    )
    for _, row in gdf.iterrows():
        ax.text(
            row.label_point.x,
            row.label_point.y,
            row["label"],   
            ha="center",
            va="center",
            fontsize=5,
            weight="bold"
        )

    ax.set_title(f"Mean {pollen_type} Levels by HOLC District (NYC)", fontsize=16)
    ax.axis("off")

    # Change to include other boroughs later
    if shapefile.equals(manhattan_map):
        map_borough = "manhattan"
    elif shapefile.equals(bronx_map):
        map_borough = "bronx"

    base_folder = "/Users/wenggeiwong/Pollen_Mapping/misc/figures"
    output_folder = os.path.join(base_folder, f"{map_borough}_figures")
    os.makedirs(output_folder, exist_ok=True)
    # Save to folder
    filename = f"mean_{pollen_type}_{map_borough}.png"
    names_in_folder = os.listdir(output_folder)
    if filename not in names_in_folder:
        fig.savefig(
            os.path.join(output_folder, filename),
            dpi=300,
            bbox_inches="tight"
        )

    plt.close(fig)  

    # Export csv files for statistical analysis

    base_folder = "/Users/wenggeiwong/pollen_mapping_data/joined_data"
    output_folder = os.path.join(base_folder, f"{map_borough}_data")
    os.makedirs(output_folder, exist_ok=True)

    filename = f"{pollen_type}_{map_borough}.csv"    
    names_in_folder = os.listdir(output_folder)
    if filename not in names_in_folder:
        df_stats.to_csv(
            os.path.join(output_folder,filename)
        )


def exportData():
    for file_entry in os.scandir('/Users/wenggeiwong/pollen_mapping_data/rasters/pollen_prediction_rasters'):
        filepath = os.path.basename(file_entry)
        print(filepath)
        borough = find_borough(filepath)
        if not filepath.startswith("."):
            if(borough==""):
                joinData(manhattan_map,file_entry.path)
                print("Manhattan export for " + filepath + " complete")
                joinData(bronx_map,file_entry.path)
                print("Bronx export for " + filepath + " complete")
            elif(borough=="bronx"):
                joinData(bronx_map,file_entry.path)
                print("Bronx export for " + filepath + " complete")
            elif(borough=="manhattan"):
                joinData(manhattan_map,file_entry.path)
                print("Manhattan export for " + filepath + " complete")

exportData()
print("DONE RUNNING!!!!")
