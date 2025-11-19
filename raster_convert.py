import rasterio
from rasterio.plot import show
import matplotlib.pyplot as plt
import geopandas as gpd
from rasterio.mask import mask
import numpy as np
from rasterstats import zonal_stats


path = "/Users/wenggeiwong/Pollen_Mapping/data/nyccas_data/aa15_pm300m"
# Vector CRS check
gdf = gpd.read_file("/Users/wenggeiwong/Pollen_Mapping/data/redlining_sfs/manh_holc_sf.json")
print(gdf.crs)

print("\n\n\n")
# Raster CRS check



with rasterio.open(path) as src:
    data = src.read(1).astype("float32")  # Convert to float
    profile = src.profile

    gdf = gdf.to_crs(src.crs)
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

for i, stat in enumerate(stats):
    for key, value in stat.items():
        gdf.loc[i, f"pollution_{key}"] = value

summary = gdf.groupby("grade")["pollution_mean"].mean()
print(summary)




# Plot
fig, ax = plt.subplots(figsize=(10, 10))

gdf.plot(
    column="pollution_mean",
    cmap="OrRd",
    linewidth=0.3,
    edgecolor="black",
    legend=True,
    ax=ax
)

ax.set_title("Mean Pollution Levels by HOLC District (NYC)", fontsize=16)
ax.axis("off")

plt.show()