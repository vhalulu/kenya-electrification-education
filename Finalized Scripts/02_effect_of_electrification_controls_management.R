# ============================================================
# ELEVATION & SLOPE EXTRACTION - CLEAN VERSION
# ============================================================

# Restart R session first to clear conflicts (Ctrl+Shift+F10 in RStudio)

library(tidyverse)
library(here)
library(sf)
library(elevatr)
library(terra)  # Load terra LAST
library(geodata)
library(osmdata)
library(rnaturalearth)
# Load data
person_analysis <- readRDS(here("Data", "Clean", "person_analysis_individual.rds"))

# Get clusters
clusters_sf <- person_analysis %>%
  distinct(cluster_id_unique, lon_wgs84, lat_wgs84) %>%
  filter(!is.na(lon_wgs84), !is.na(lat_wgs84)) %>%
  st_as_sf(coords = c("lon_wgs84", "lat_wgs84"), crs = 4326)

cat("Clusters:", nrow(clusters_sf), "\n")

# Download elevation
cat("Downloading elevation...\n")
elev <- get_elev_raster(clusters_sf, z = 9, src = "aws")
elev_terra <- rast(elev)
writeRaster(elev_terra, here("Data", "Raw", "kenya_elevation.tif"), overwrite = TRUE)

# Calculate slope
cat("Calculating slope...\n")
slope_terra <- terrain(elev_terra, v = "slope", unit = "degrees")
writeRaster(slope_terra, here("Data", "Raw", "kenya_slope.tif"), overwrite = TRUE)

# Extract using terra explicitly
cat("Extracting...\n")
elev_vals <- terra::extract(elev_terra, vect(clusters_sf))
slope_vals <- terra::extract(slope_terra, vect(clusters_sf))

# Combine
terrain_data <- tibble(
  cluster_id_unique = clusters_sf %>% st_drop_geometry() %>% pull(cluster_id_unique),
  elevation_m = elev_vals[, 2],
  slope_degrees = slope_vals[, 2]
)

# Stats
cat("\nElevation: ", round(mean(terrain_data$elevation_m, na.rm=TRUE), 1), "m\n")
cat("Slope: ", round(mean(terrain_data$slope_degrees, na.rm=TRUE), 2), "°\n\n")

# Merge
person_analysis_with_terrain <- person_analysis %>%
  left_join(terrain_data, by = "cluster_id_unique")

# Save
saveRDS(person_analysis_with_terrain, 
        here("Data", "Clean", "person_analysis_with_terrain.rds"))

cat("✅ DONE!\n")


#Add climate variables

# Load your data with elevation & slope
person_analysis_with_terrain <- readRDS(
  here("Data", "Clean", "person_analysis_with_terrain.rds")
)

# Download precipitation data
cat("Downloading precipitation data...\n")
precip <- worldclim_country("Kenya", var = "prec", res = 0.5, 
                            path = here("Data", "Raw"))

# Calculate annual precipitation
annual_precip <- sum(precip)

# Classify into AEZ
aez <- classify(annual_precip,
                matrix(c(0, 400, 1, 400, 800, 2, 800, 1200, 3, 1200, 9999, 4), 
                       ncol = 3, byrow = TRUE)
)

# Save
writeRaster(aez, here("Data", "Raw", "kenya_aez.tif"), overwrite = TRUE)

# Extract for clusters
clusters_sf <- person_analysis_with_terrain %>%
  distinct(cluster_id_unique, lon_wgs84, lat_wgs84) %>%
  filter(!is.na(lon_wgs84), !is.na(lat_wgs84)) %>%
  st_as_sf(coords = c("lon_wgs84", "lat_wgs84"), crs = 4326)

aez_values <- terra::extract(aez, vect(clusters_sf))

aez_data <- tibble(
  cluster_id_unique = clusters_sf %>% st_drop_geometry() %>% pull(cluster_id_unique),
  aez_code = aez_values[, 2],
  aez = case_when(
    aez_code == 1 ~ "Arid",
    aez_code == 2 ~ "Semi-arid",
    aez_code == 3 ~ "Sub-humid",
    aez_code == 4 ~ "Humid"
  )
)

# Distribution
cat("\nAEZ Distribution:\n")
print(table(aez_data$aez))

# Merge
person_analysis_complete <- person_analysis_with_terrain %>%
  left_join(aez_data %>% select(cluster_id_unique, aez), 
            by = "cluster_id_unique")

# Check
cat("\nMissing AEZ:", sum(is.na(person_analysis_complete$aez)), "\n")

# Save
saveRDS(person_analysis_complete,
        here("Data", "Clean", "person_analysis_complete.rds"))


#Add roads data 

# Load your data
person_analysis_complete <- readRDS(
  here("Data", "Clean", "person_analysis_complete.rds")
)

# Download Kenya OSM data from Geofabrik


# Direct download link for Kenya roads

#kenya_roads_url <- "https://download.geofabrik.de/africa/kenya-latest-free.shp.zip"

# Download

#download.file(
 #url = kenya_roads_url,
 #destfile = here("Data", "Raw", "kenya_osm.zip"),
  #mode = "wb"
#)

# Unzip
#unzip(
  #zipfile = here("Data", "Raw", "kenya_osm.zip"),
  #exdir = here("Data", "Raw", "kenya_osm")
#)


# Read roads layer
# Geofabrik provides separate layers for different features
# Look for roads in the "gis_osm_roads_free_1.shp" file

kenya_roads_all <- st_read(
  here("Data", "Raw", "kenya_osm", "gis_osm_roads_free_1.shp")
)

cat("Loaded", nrow(kenya_roads_all), "road segments\n\n")

# Filter to major roads only
# fclass = road classification in Geofabrik data
kenya_roads_major <- kenya_roads_all %>%
  filter(fclass %in% c("motorway", "trunk", "primary", "secondary"))

cat("Major roads:", nrow(kenya_roads_major), "segments\n\n")

# Save
st_write(kenya_roads_major,
         here("Data", "Raw", "kenya_roads_major.shp"),
         append = FALSE)

# Search for the geodatabase
gdb_path <- list.files(
  here("Data"),
  pattern = "HydroRIVERS.*\\.gdb$",
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = TRUE
)

cat("Found", length(gdb_path), "geodatabase(s):\n")
print(gdb_path)

if (length(gdb_path) == 0) {
  cat("\n❌ HydroRIVERS geodatabase not found!\n\n")
  cat("Checking what's in Data/Raw/:\n")
  print(list.files(here("Data", "Raw")))
  
  stop("\nPlease ensure HydroRIVERS_v10_af.gdb is in Data/Raw/")
}

# Use first match
gdb_path <- gdb_path[1]
cat("\n✅ Using geodatabase at:", gdb_path, "\n\n")

# Read it
cat("Reading HydroRIVERS Africa...\n")
africa_rivers <- st_read(gdb_path, quiet = FALSE)

cat("✅ Loaded", nrow(africa_rivers), "river segments\n\n")

# Get Kenya boundary
cat("Getting Kenya boundary...\n")
kenya <- ne_countries(country = "Kenya", returnclass = "sf", scale = "medium")

# Clip rivers to Kenya
cat("Clipping rivers to Kenya (this may take a few minutes)...\n")
kenya_rivers_all <- st_intersection(africa_rivers, kenya)

cat("✅ Kenya rivers:", nrow(kenya_rivers_all), "segments\n\n")

# Filter to major rivers (Strahler >= 4)
cat("Filtering to major rivers (Strahler order >= 4)...\n")
kenya_rivers_major <- kenya_rivers_all %>%
  filter(ORD_STRA >= 4)

cat("✅ Major rivers:", nrow(kenya_rivers_major), "segments\n\n")

# Save
st_write(kenya_rivers_major,
         here("Data", "Raw", "kenya_rivers_major.shp"),
         append = FALSE)

cat("✅ Saved: Data/Raw/kenya_rivers_major.shp\n")

# ============================================================
# CALCULATE DISTANCES AND MERGE FINAL DATASET
# ============================================================

cat("\n=== FINAL MERGE: ADDING ROAD & RIVER DISTANCES ===\n\n")

# Load existing data (already has elevation, slope, AEZ)
person_analysis_complete <- readRDS(
  here("Data", "Clean", "person_analysis_complete.rds")
)

# Get clusters
clusters_sf <- person_analysis_complete %>%
  distinct(cluster_id_unique, lon_wgs84, lat_wgs84) %>%
  filter(!is.na(lon_wgs84), !is.na(lat_wgs84)) %>%
  st_as_sf(coords = c("lon_wgs84", "lat_wgs84"), crs = 4326)

# Load roads and rivers (already saved)
kenya_roads <- st_read(here("Data", "Raw", "kenya_roads_major.shp"), quiet = TRUE)
kenya_rivers <- st_read(here("Data", "Raw", "kenya_rivers_major.shp"), quiet = TRUE)

# Transform to UTM for distance calculation
clusters_utm <- st_transform(clusters_sf, 32737)
roads_utm <- st_transform(kenya_roads, 32737)
rivers_utm <- st_transform(kenya_rivers, 32737)

# Calculate distances
cat("Calculating distances (may take 5-10 min)...\n")
dist_road <- st_distance(clusters_utm, roads_utm)
dist_river <- st_distance(clusters_utm, rivers_utm)

# Create infrastructure data
infra_data <- tibble(
  cluster_id_unique = clusters_sf %>% st_drop_geometry() %>% pull(cluster_id_unique),
  dist_road_km = as.numeric(apply(dist_road, 1, min) / 1000),
  dist_river_km = as.numeric(apply(dist_river, 1, min) / 1000),
  log_dist_road = log(as.numeric(apply(dist_road, 1, min) / 1000) + 1),
  log_dist_river = log(as.numeric(apply(dist_river, 1, min) / 1000) + 1)
)

cat("Road distance: Mean =", round(mean(infra_data$dist_road_km), 2), "km\n")
cat("River distance: Mean =", round(mean(infra_data$dist_river_km), 2), "km\n\n")

# Merge to main dataset
person_analysis_final <- person_analysis_complete %>%
  left_join(infra_data, by = "cluster_id_unique")

# Save final dataset
saveRDS(person_analysis_final,
        here("Data", "Clean", "person_analysis_final.rds"))

cat("✅ DONE! Saved:", nrow(person_analysis_final), "obs with all controls\n")
cat("✅ Variables: elevation, slope, aez, dist_road_km, dist_river_km\n")
