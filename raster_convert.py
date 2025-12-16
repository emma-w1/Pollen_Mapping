import rasterio
from rasterio.plot import show
import matplotlib.pyplot as plt
import geopandas as gpd
from rasterio.mask import mask
import numpy as np
from rasterstats import zonal_stats
import os

manhattan_map = gpd.read_file("/Users/wenggeiwong/Pollen_Mapping/data/redlining_sfs/manh_holc_sf.json")
bronx_map = gpd.read_file('/Users/wenggeiwong/Pollen_Mapping/data/redlining_sfs/brx_holc_sf.json')
annual15_pm = "/Users/wenggeiwong/Pollen_Mapping/data/nyccas_data/aa15_pm300m"

# Create avg. maps
def joinData(shapefile,raster):
    filename = os.path.basename(raster)
    year = filename.split("_")[0]
    digits_list = [char for char in year if char.isdigit()]
    year = "".join(digits_list)
    year = int(year)
    year = year + 2008

    pollutant = filename.split("_")[-1]
    pollutant = pollutant.replace("300m","").upper()
    if pollutant == "PM":
        pollutant = "PM2.5"
    
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
        stats=["mean", "median", "max", "min", "std"]
    )

    gdf["pollution_mean"] = [s["mean"] for s in stats]
    gdf["pollution_median"] = [s["median"] for s in stats]
    gdf["pollution_max"] = [s["max"] for s in stats]
    gdf["pollution_min"] = [s["min"] for s in stats]
    gdf["pollution_std"] = [s["std"] for s in stats]

    for i, stat in enumerate(stats):
        for key, value in stat.items():
            gdf.loc[i, f"pollution_{key}"] = value

    summary = gdf.groupby("grade")["pollution_mean"].mean()
    print(summary)




    # Plot
    gdf["label_point"] = gdf.geometry.representative_point()
    fig, ax = plt.subplots(figsize=(10, 10))

    gdf.plot(
        column="pollution_mean",
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
            row["grade"],   # ← THIS comes from properties.grade
            ha="center",
            va="center",
            fontsize=10,
            weight="bold"
        )

    ax.set_title(f"Mean {pollutant} Levels by HOLC District (NYC)", fontsize=16)
    ax.axis("off")

    # Change to include other boroughs later
    if shapefile.equals(manhattan_map):
        map_borough = "manhattan"
    elif shapefile.equals(bronx_map):
        map_borough = "bronx"

    base_folder = "/Users/wenggeiwong/Pollen_Mapping/figures"
    output_folder = os.path.join(base_folder, f"{map_borough}_figures")
    os.makedirs(output_folder, exist_ok=True)
    # Save to folder
    filename = f"mean_{pollutant}_{year}_{map_borough}.png"
    names_in_folder = os.listdir(output_folder)
    if filename not in names_in_folder:
        fig.savefig(
            os.path.join(output_folder, filename),
            dpi=300,
            bbox_inches="tight"
        )

    plt.close(fig)  




# joinData(manhattan_map,annual15_pm)

# annual15_no2 = "/Users/wenggeiwong/Pollen_Mapping/data/nyccas_data/aa15_no2300m"
# joinData(manhattan_map,annual15_no2)

for file_entry in os.scandir('/Users/wenggeiwong/Pollen_Mapping/data/nyccas_data'):
    print(os.path.basename(file_entry))
    if not os.path.basename(file_entry).startswith("."):
        joinData(manhattan_map,file_entry.path)
        print("Manhattan export for " + os.path.basename(file_entry) + " complete")
        joinData(bronx_map,file_entry.path)
        print("Bronx export for " + os.path.basename(file_entry) + " complete")


print("DONE RUNNING!!!!")