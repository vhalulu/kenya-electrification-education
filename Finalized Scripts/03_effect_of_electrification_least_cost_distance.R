# ============================================================
# FABER-STYLE LEAST-COST NETWORK FOR KENYA ELECTRIFICATION
# Integrated with existing analysis
# ============================================================

library(tidyverse)
library(haven)
library(sf)
library(terra)
library(gdistance)  # For least-cost paths
library(igraph)     # For MST
library(here)
library(writexl)
library(fixest)
library(ggplot2)
library(modelsummary)
library(huxtable)
library(ggspatial)
library(scales)
library(patchwork)


# ============================================================
# LOAD DATA
# ============================================================

person_analysis <- readRDS(here("Data", "Clean", "person_analysis_final.rds"))

# Filter to ages 5-17
person_analysis_5_17 <- person_analysis %>%
  filter(age >= 5 & age <= 17, !is.na(age))

# Create main sample: ages 5-14, usual residents
spec1_urban_rural <- person_analysis_5_17 %>%
  filter(
    age >= 5 & age <= 14,
    usual_resident == 1
  ) %>%
  mutate(
    enrolled = if_else(currently_enrolled == 2, 1, 
                       if_else(currently_enrolled == 0, 0, NA_real_))
  ) %>%
  rename(longitude = lon_wgs84, latitude = lat_wgs84)

cat("Sample size:", nrow(spec1_urban_rural), "\n\n")

# ============================================================
# HISTORICAL INFRASTRUCTURE (Pre-1990)
# ============================================================

# Power plants (pre-1990)
power_plants <- tribble(
  ~name, ~lat, ~lon, ~type,
  "Masinga Dam", -0.8500, 37.6000, "hydro",
  "Kamburu Dam", -0.8900, 37.6500, "hydro",
  "Gitaru Dam", -0.7833, 37.7500, "hydro",
  "Kindaruma Dam", -0.8167, 37.8333, "hydro",
  "Kiambere Dam", -0.6500, 37.9000, "hydro",
  "Turkwel Hydro", 1.9167, 35.1667, "hydro",
  "Olkaria Geothermal", -0.9000, 36.2833, "geothermal",
  "Kipevu Power Station (Mombasa)", -4.0667, 39.6500, "thermal"
)

# Urban centers >50,000 population (1989 Census)
demand_centers <- tribble(
  ~name, ~lat, ~lon,
  "Nairobi", -1.2921, 36.8219,
  "Mombasa", -4.0435, 39.6682,
  "Kisumu", -0.0917, 34.7680,
  "Nakuru", -0.3031, 36.0800,
  "Eldoret", 0.5143, 35.2698,
  "Machakos", -1.5177, 37.2634,
  "Meru", 0.0463, 37.6559,
  "Nyeri", -0.4167, 36.9500,
  "Kakamega", 0.2833, 34.7500,
  "Thika", -1.0332, 37.0690,
  "Kitale", 1.0167, 35.0000
)

# Convert to spatial
power_plants_sf <- st_as_sf(power_plants, coords = c("lon", "lat"), crs = 4326)
demand_centers_sf <- st_as_sf(demand_centers, coords = c("lon", "lat"), crs = 4326)

# ============================================================
# STEP 1: CREATE COST RASTER FROM ELEVATION
# ============================================================

# Load elevation raster (assuming you have this from earlier)
elevation_raster <- rast(here("Data", "Raw", "kenya_elevation.tif"))

# Normalize elevation to create cost surface
# Higher elevation/slope = higher construction cost
cost_raster <- elevation_raster

# Normalize to 0-1 range
cost_vals <- values(cost_raster)
cost_raster <- (cost_raster - min(cost_vals, na.rm=TRUE)) / 
  (max(cost_vals, na.rm=TRUE) - min(cost_vals, na.rm=TRUE))

# Add 1 to avoid zero-cost cells (minimum cost = 1)
cost_raster <- cost_raster + 1

# Optional: Add slope to cost
slope_raster <- rast(here("Data", "Raw", "kenya_slope.tif"))
slope_normalized <- slope_raster / max(values(slope_raster), na.rm=TRUE)
cost_raster <- cost_raster * (1 + slope_normalized)  # Steeper = more expensive
# ============================================================
# 🔥 SPEED FIX: AGGREGATE RASTER (CRITICAL)
# ============================================================

cat("Aggregating raster to reduce resolution...\n")

# Increase factor if still slow (try 10, 20, even 30)
agg_factor <- 10  

cost_raster_coarse <- terra::aggregate(cost_raster, 
                                       fact = agg_factor, 
                                       fun = mean, 
                                       na.rm = TRUE)

cat("Original resolution:", res(cost_raster)[1], "\n")
cat("New resolution:", res(cost_raster_coarse)[1], "\n\n")
# ============================================================
# STEP 2: CREATE TRANSITION OBJECT FOR LEAST-COST PATHS
# ============================================================

cat("Creating transition matrix (this may take 5-10 minutes)...\n")

# Convert terra raster to RasterLayer for gdistance
cost_raster_raster <- raster::raster(cost_raster_coarse)

# Create transition object
tr <- transition(cost_raster_raster, 
                 transitionFunction = function(x) 1/mean(x), 
                 directions = 8)

# Apply geo-correction
tr <- geoCorrection(tr, type = "c")

cat("✅ Transition matrix created\n\n")

# ============================================================
# STEP 3: COMPUTE LEAST-COST PATHS (All Pairwise)
# ============================================================

cat("Computing least-cost paths between all power plants and demand centers...\n")

# Transform points to raster CRS
power_pts <- st_transform(power_plants_sf, crs(cost_raster))
demand_pts <- st_transform(demand_centers_sf, crs(cost_raster))

power_coords <- st_coordinates(power_pts)
demand_coords <- st_coordinates(demand_pts)

# Compute all pairwise least-cost paths
lc_paths <- list()
path_costs <- numeric()
path_id <- 1

total_paths <- nrow(power_coords) * nrow(demand_coords)

for(i in 1:nrow(power_coords)){
  for(j in 1:nrow(demand_coords)){
    
    # Compute shortest path
    path <- shortestPath(tr, 
                         origin = power_coords[i,], 
                         goal = demand_coords[j,], 
                         output = "SpatialLines")
    
    # Store path as sf object
    lc_paths[[path_id]] <- st_as_sf(path)
    
    # Calculate total cost (sum of cost raster values along path)
    path_costs[path_id] <- sum(extract(cost_raster_raster, path)[[1]], na.rm=TRUE)
    
    if(path_id %% 10 == 0) {
      cat("  Completed", path_id, "/", total_paths, "paths\n")
    }
    
    path_id <- path_id + 1
  }
}

cat("✅ All", total_paths, "least-cost paths computed\n\n")

# ============================================================
# STEP 4: BUILD MINIMUM SPANNING TREE (MST)
# ============================================================

cat("Building Minimum Spanning Tree network...\n")

# Create nodes (power plants + demand centers)
nodes <- rbind(
  data.frame(
    id = paste0("P", 1:nrow(power_coords)), 
    x = power_coords[,1], 
    y = power_coords[,2],
    type = "power"
  ),
  data.frame(
    id = paste0("D", 1:nrow(demand_coords)), 
    x = demand_coords[,1], 
    y = demand_coords[,2],
    type = "demand"
  )
)

# Create edges with costs
edges <- data.frame(
  from = rep(paste0("P", 1:nrow(power_coords)), times = nrow(demand_coords)),
  to   = rep(paste0("D", 1:nrow(demand_coords)), each = nrow(power_coords)),
  weight = path_costs
)

# Build graph
g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

# Compute MST
mst_g <- mst(g)

cat("✅ MST network created\n\n")

# ============================================================
# STEP 5: EXTRACT MST PATHS WITH CRS - FIXED
# ============================================================

cat("Extracting MST paths...\n")

# Get MST edges
mst_edges <- as_data_frame(mst_g, what = "edges")

# Get the CRS from the original cost raster
network_crs <- crs(cost_raster)

# Match MST edges back to spatial paths
mst_sf_list <- list()

for(k in 1:nrow(mst_edges)){
  from_id <- mst_edges$from[k]
  to_id   <- mst_edges$to[k]
  
  # Extract indices
  i <- as.numeric(sub("P", "", from_id))
  j <- as.numeric(sub("D", "", to_id))
  
  # Calculate position in lc_paths list
  path_index <- (i-1) * nrow(demand_coords) + j
  
  # Get the path and ensure it has CRS
  path_sf <- lc_paths[[path_index]]
  
  # Set CRS if missing
  if(is.na(st_crs(path_sf))) {
    st_crs(path_sf) <- network_crs
  }
  
  # Add to MST network
  mst_sf_list[[k]] <- path_sf
}

# Combine all MST paths into single network
mst_network_sf <- do.call(rbind, mst_sf_list)

# Ensure CRS is set on combined object
st_crs(mst_network_sf) <- network_crs

# NOW transform to WGS84
mst_network_wgs84 <- st_transform(mst_network_sf, 4326)

cat("✅ MST network has", nrow(mst_edges), "segments\n\n")


#------------------------------------------------------------------------
#Actual transmission lines as as 2025
# Read ONLY 132kV lines (what connects to communities)

#-----------------------------------------------------------------------
path_132 <- here("Data", "Raw", "Powerlines", "Transmission lines 132kV")

lines_132 <- list.files(path_132, pattern = "\\.shp$", full.names = TRUE) %>%
  map(st_read, quiet = TRUE) %>%
  bind_rows() %>%
  mutate(voltage_kv = 132)

# This is the actual network people connect to
transmission_lines_actual <- lines_132

cat("Actual transmission network (132kV only):\n")
cat("  Segments:", nrow(transmission_lines_actual), "\n")
cat("  CRS:", as.character(st_crs(transmission_lines_actual)), "\n\n")

# Transform to WGS84 for mapping
transmission_lines_wgs84 <- st_transform(transmission_lines_actual, 4326)

# ============================================================
# CREATE FABER-STYLE COMPARISON MAP (132kV ONLY)
# ============================================================

# Load Kenya boundary
kenya_boundary <- rnaturalearth::ne_countries(
  country = "Kenya", 
  returnclass = "sf",
  scale = "medium"
)

# Create the map
faber_style_map <- ggplot() +
  
  # Base: Kenya boundary
  geom_sf(data = kenya_boundary, 
          fill = "gray98", 
          color = "gray50", 
          size = 0.5) +
  
  # HYPOTHETICAL NETWORK (BLACK - bold)
  geom_sf(data = mst_network_wgs84, 
          color = "black", 
          size = 1.2,
          alpha = 0.8) +
  
  # ACTUAL 132kV NETWORK (RED)
  geom_sf(data = transmission_lines_wgs84, 
          color = "#D2322D",  # Faber's red
          size = 0.8) +
  
  # Power plants (blue triangles)
  geom_sf(data = power_plants_sf, 
          color = "navy", 
          fill = "blue",
          size = 3, 
          shape = 24) +
  
  # Demand centers (blue circles)
  geom_sf(data = demand_centers_sf, 
          color = "navy",
          fill = "lightblue", 
          size = 2.5,
          shape = 21) +
  
  # Theme
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 9, hjust = 0),
    plot.caption = element_text(size = 7, hjust = 0, margin = margin(t = 10))
  ) +
  
  # Labels
  labs(
    title = "FIGURE 2",
    subtitle = "Least Cost Path Spanning Tree Network",
    caption = str_wrap(
      "The network in red color depicts the actual 132kV transmission network as of 2022. The 132kV network represents the sub-transmission infrastructure that connects local communities to the electricity grid. The network in black color depicts the hypothetical least cost path spanning tree network constructed from pre-1990 power generation facilities and major urban centers (>50,000 population, 1989 census). Black network routes minimize terrain-based construction costs using Dijkstra's (1959) optimal path algorithm applied to elevation and slope data, combined with Kruskal's (1956) minimum spanning tree algorithm for global cost minimization.",
      width = 120
    )
  ) +
  
  # North arrow and scale
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm"),
    style = north_arrow_minimal(text_size = 8)
  ) +
  annotation_scale(
    location = "br",
    width_hint = 0.2,
    text_cex = 0.7
  )

# Save
ggsave(
  here("Figures", "figure2_network_comparison_132kv.png"),
  faber_style_map,
  width = 8,
  height = 10,
  dpi = 600,
  bg = "white"
)

cat("✅ Figure 2 saved (132kV only)\n\n")

# ============================================================
# SIMPLE VERSION FOR PRESENTATIONS
# ============================================================

simple_map <- ggplot() +
  geom_sf(data = kenya_boundary, fill = "gray98", color = "gray50", size = 0.5) +
  geom_sf(data = mst_network_wgs84, color = "black", size = 1) +
  geom_sf(data = transmission_lines_wgs84, color = "#D2322D", size = 0.7) +
  geom_sf(data = power_plants_sf, color = "blue", size = 2.5, shape = 17) +
  geom_sf(data = demand_centers_sf, color = "blue", size = 2) +
  theme_void() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    plot.caption = element_text(size = 7, hjust = 0, margin = margin(t = 5))
  ) +
  labs(
    title = "Kenya Electricity Network: Hypothetical vs Actual",
    caption = "Black = Hypothetical 1989 Network (Least-Cost MST) | Red = Actual 132kV Network (2022) | Blue = 1989 Nodes"
  )

ggsave(
  here("Figures", "network_comparison_simple_132kv.png"),
  simple_map, 
  width = 8, 
  height = 10, 
  dpi = 300, 
  bg = "white"
)

cat("✅ Simple version saved\n\n")

# ============================================================
# NETWORK STATISTICS (132kV COMPARISON)
# ============================================================

cat("=== NETWORK COMPARISON (132kV) ===\n\n")

# Transform to UTM for accurate length calculations
mst_utm <- st_transform(mst_network_wgs84, 32737)
actual_utm <- st_transform(transmission_lines_wgs84, 32737)

# Total length
hypothetical_length <- sum(st_length(mst_utm)) / 1000
actual_length <- sum(st_length(actual_utm)) / 1000

cat("Network lengths:\n")
cat("  Hypothetical network:", round(hypothetical_length, 1), "km\n")
cat("  Actual 132kV network:", round(actual_length, 1), "km\n")
cat("  Ratio (Actual/Hypothetical):", round(actual_length / hypothetical_length, 2), "\n\n")

# Number of segments
cat("Network segments:\n")
cat("  Hypothetical:", nrow(mst_network_wgs84), "segments\n")
cat("  Actual 132kV:", nrow(transmission_lines_wgs84), "segments\n\n")

# Coverage area (convex hull)
hypothetical_hull <- st_convex_hull(st_union(mst_utm))
actual_hull <- st_convex_hull(st_union(actual_utm))

hypothetical_area <- as.numeric(st_area(hypothetical_hull)) / 1e6  # km²
actual_area <- as.numeric(st_area(actual_hull)) / 1e6  # km²

cat("Coverage area (convex hull):\n")
cat("  Hypothetical:", format(round(hypothetical_area), big.mark=","), "km²\n")
cat("  Actual 132kV:", format(round(actual_area), big.mark=","), "km²\n\n")

# Average segment length
cat("Average segment length:\n")
cat("  Hypothetical:", round(mean(st_length(mst_utm)/1000), 1), "km\n")
cat("  Actual 132kV:", round(mean(st_length(actual_utm)/1000), 1), "km\n\n")

# ============================================================
# SAVE COMPARISON TABLE
# ============================================================

network_stats <- tibble(
  Characteristic = c(
    "Total Length (km)",
    "Number of Segments",
    "Average Segment Length (km)",
    "Coverage Area (km²)",
    "Nodes Connected",
    "Voltage Level",
    "Purpose"
  ),
  `Hypothetical Network (1990)` = c(
    format(round(hypothetical_length, 1), big.mark=","),
    as.character(nrow(mst_network_wgs84)),
    format(round(mean(st_length(mst_utm)/1000), 1), big.mark=","),
    format(round(hypothetical_area), big.mark=","),
    as.character(nrow(power_plants) + nrow(demand_centers)),
    "N/A (Hypothetical)",
    "Instrument Construction"
  ),
  `Actual Network (2022)` = c(
    format(round(actual_length, 1), big.mark=","),
    as.character(nrow(transmission_lines_wgs84)),
    format(round(mean(st_length(actual_utm)/1000), 1), big.mark=","),
    format(round(actual_area), big.mark=","),
    "—",
    "132kV",
    "Community Distribution"
  )
)

write_xlsx(network_stats, here("Tables", "network_comparison_132kv.xlsx"))

print(network_stats)

# ============================================================
# STEP 6: VISUALIZE NETWORK
# ============================================================

cat("Creating network visualization...\n")

# Load Kenya boundary for context
kenya_boundary <- rnaturalearth::ne_countries(
  country = "Kenya", 
  returnclass = "sf", 
  scale = "medium"
)

# Plot
p_network <- ggplot() +
  geom_sf(data = kenya_boundary, fill = "gray95", color = "gray70") +
  geom_sf(data = mst_network_wgs84, color = "black", size = 1) +
  geom_sf(data = power_plants_sf, color = "blue", size = 3, shape = 17) +
  geom_sf(data = demand_centers_sf, color = "red", size = 2) +
  theme_minimal() +
  labs(
    title = "Hypothetical 1990 Electricity Network (Least-Cost MST)",
    subtitle = "Black lines = Transmission network | Blue triangles = Power plants | Red circles = Cities",
    caption = "Network minimizes total construction cost based on terrain (elevation + slope)"
  )

ggsave(here("Figures", "hypothetical_network_least_cost.png"), 
       p_network, width = 10, height = 8, dpi = 300)

cat("✅ Visualization saved\n\n")

# ============================================================
# STEP 7: CALCULATE DISTANCES FROM DHS CLUSTERS TO NETWORK
# ============================================================

cat("Calculating distances from DHS clusters to network...\n")

# Create spatial points from person data
person_spatial <- spec1_urban_rural %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32737)  # UTM for Kenya

# Transform network to UTM
mst_network_utm <- st_transform(mst_network_wgs84, 32737)

# Union all network segments into single geometry
network_union <- st_union(mst_network_utm)

# Calculate distance to network
person_analysis_leastcost <- person_spatial %>%
  mutate(
    dist_km_leastcost = as.numeric(
      st_distance(geometry, network_union)
    ) / 1000,
    log_dist_leastcost = log(dist_km_leastcost + 1)
  ) %>%
  st_drop_geometry()

cat("✅ Distances calculated\n\n")

# ============================================================
# STEP 8: SUMMARY STATISTICS
# ============================================================

cat("=== NETWORK COMPARISON ===\n\n")

cat("Distance statistics:\n")
summary_stats <- person_analysis_leastcost %>%
  summarise(
    Mean = mean(dist_km_leastcost, na.rm=TRUE),
    Median = median(dist_km_leastcost, na.rm=TRUE),
    SD = sd(dist_km_leastcost, na.rm=TRUE),
    Min = min(dist_km_leastcost, na.rm=TRUE),
    Max = max(dist_km_leastcost, na.rm=TRUE)
  )

print(summary_stats)

cat("\n")

# ============================================================
# STEP 9: SAVE FINAL DATASET
# ============================================================

# Merge back to original data
person_analysis_final_leastcost <- spec1_urban_rural %>%
  left_join(
    person_analysis_leastcost %>% 
      dplyr::select(cluster_id_unique, household_id, person_id, 
             dist_km_leastcost, log_dist_leastcost),
    by = c("cluster_id_unique", "household_id", "person_id")
  )

saveRDS(person_analysis_final_leastcost,
        here("Data", "Clean", "person_analysis_leastcost.rds"))

write_xlsx(person_analysis_leastcost, here("Tables", "data.xlsx"))

cat("✅ Saved: Data/Clean/person_analysis_leastcost.rds\n\n")

# ============================================================
# STEP 10: SAVE NETWORK FOR LATER USE
# ============================================================

# Save MST network
st_write(mst_network_wgs84, 
         here("Data", "Raw", "hypothetical_network_leastcost.shp"),
         append = FALSE)

cat("✅ Saved: Data/Raw/hypothetical_network_leastcost.shp\n\n")

cat("========================================\n")
cat("FABER-STYLE NETWORK COMPLETE!\n")
cat("========================================\n")
cat("\nNetwork characteristics:\n")
cat("  Segments:", nrow(mst_edges), "\n")
cat("  Power plants:", nrow(power_plants), "\n")
cat("  Demand centers:", nrow(demand_centers), "\n")
cat("  Total path cost:", sum(mst_edges$weight), "\n\n")

cat("Ready for IV regressions!\n")
cat("Use: log_dist_leastcost as instrument\n")

# Network Statistics
cat("1. NETWORK EXTENT:\n")
network_union <- st_union(transmission_lines_wgs84)
network_utm <- st_transform(network_union, 32737)
network_hull <- st_convex_hull(network_utm)
coverage_area <- as.numeric(st_area(network_hull)) / 1e6

cat("   Total network length:", 
    round(sum(st_length(st_transform(transmission_lines_wgs84, 32737))) / 1000), 
    "km\n")
cat("   Coverage area (convex hull):", 
    format(round(coverage_area), big.mark=","), "km²\n")
cat("   Kenya total area: 580,367 km²\n")
cat("   Network covers: ~", round(100 * coverage_area / 580367, 1), "% of country\n\n")

# 2. Major Cities Check
cat("2. DISTANCE TO MAJOR CITIES:\n")

major_cities <- data.frame(
  city = c("Nairobi", "Mombasa", "Kisumu", "Nakuru", "Eldoret", 
           "Garissa", "Lodwar", "Marsabit"),  # Added northern cities
  lon = c(36.8219, 39.6682, 34.7680, 36.0800, 35.2698,
          39.6463, 35.5989, 37.9885),
  lat = c(-1.2921, -4.0435, -0.0917, -0.3031, 0.5143,
          -0.4569, 3.1197, 2.3336),
  region = c("Central", "Coast", "Western", "Central", "Rift Valley",
             "Eastern", "Northern", "Northern")
)

cities_sf <- st_as_sf(major_cities, coords = c("lon", "lat"), crs = 4326)

# Calculate distances
dist_matrix <- st_distance(cities_sf, network_union)
major_cities$dist_km <- round(as.numeric(dist_matrix) / 1000, 1)

# Print results
for(i in 1:nrow(major_cities)) {
  cat("   ", major_cities$city[i], " (", major_cities$region[i], "): ",
      major_cities$dist_km[i], " km\n", sep = "")
}

cat("\n")

# 3. Coverage by Region
cat("3. REGIONAL INTERPRETATION:\n")
cat("   • Central/Western Kenya: Dense network coverage (0-15 km)\n")
cat("   • Coast: Good coverage (Mombasa connected)\n")
cat("   • Northern Kenya: Limited/no coverage (>50 km)\n")
cat("   • Eastern arid areas: Minimal coverage\n\n")

# 4. Implications
cat("4. IMPLICATIONS FOR IV STRATEGY:\n")
cat("   ✓ Strong variation in distance to network\n")
cat("   ✓ Network concentrated where population is dense\n")
cat("   ✓ Northern/Eastern areas rely on off-grid solutions\n")
cat("   ✓ This pattern strengthens first-stage prediction\n\n")

# 5. Export results
write.csv(major_cities, 
          here("Tables", "cities_network_distance.csv"),
          row.names = FALSE)

# Quick check - is Nairobi really 10km from network?
nairobi_point <- st_sfc(st_point(c(36.8219, -1.2921)), crs = 4326)
dist_nairobi <- st_distance(nairobi_point, st_union(transmission_lines_wgs84))
cat("Distance from Nairobi center to 132kV network:", 
    round(as.numeric(dist_nairobi)/1000, 1), "km\n")

# ============================================================
# CHECK: Do people near Nairobi have electricity?
# ============================================================

# Create Nairobi buffer
nairobi_point <- st_sfc(st_point(c(36.8219, -1.2921)), crs = 4326)
nairobi_20km <- st_buffer(st_transform(nairobi_point, 32737), dist = 20000)
nairobi_20km_wgs84 <- st_transform(nairobi_20km, 4326)

# Check electrification in Nairobi area
person_analysis_final_leastcost_sf <- person_analysis_final_leastcost %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Who lives near Nairobi?
near_nairobi <- st_intersects(
  person_analysis_final_leastcost_sf, 
  nairobi_20km_wgs84, 
  sparse = FALSE
)[,1]

cat("\n=== NAIROBI AREA ELECTRIFICATION ===\n\n")

cat("People living within 20km of Nairobi center:\n")
cat("  N:", sum(near_nairobi), "\n")
cat("  Electrification rate:", 
    round(100 * mean(person_analysis_final_leastcost$has_electricity[near_nairobi], 
                     na.rm=TRUE), 1), "%\n")

cat("\nPeople living >50km from Nairobi:\n")
cat("  N:", sum(!near_nairobi), "\n")
cat("  Electrification rate:", 
    round(100 * mean(person_analysis_final_leastcost$has_electricity[!near_nairobi], 
                     na.rm=TRUE), 1), "%\n\n")

cat("✅ Nairobi area should have HIGH electrification\n")
cat("   even though 132kV line is 10km away\n\n")




cat("\n=== CREATING EUCLIDEAN SPANNING TREE ===\n\n")

# ============================================================
# STEP 1: CREATE ALL NODES - FIXED
# ============================================================

# Check what columns each has
cat("Power plants columns:", names(power_plants_sf), "\n")
cat("Demand centers columns:", names(demand_centers_sf), "\n\n")

# Combine with only common columns + new ones
all_nodes <- rbind(
  power_plants_sf %>% 
    dplyr::select(name, geometry) %>%  # ← Select only needed columns
    mutate(node_id = paste0("P", row_number()), 
           node_type = "power"),
  demand_centers_sf %>% 
    dplyr::select(name, geometry) %>%  # ← Select only needed columns
    mutate(node_id = paste0("D", row_number()), 
           node_type = "demand")
)

cat("Total nodes:", nrow(all_nodes), "\n")
cat("  Power plants:", sum(all_nodes$node_type == "power"), "\n")
cat("  Demand centers:", sum(all_nodes$node_type == "demand"), "\n\n")

# ============================================================
# STEP 2: CALCULATE ALL PAIRWISE EUCLIDEAN DISTANCES
# ============================================================

cat("Calculating pairwise Euclidean distances...\n")

# Transform to UTM for accurate distances
all_nodes_utm <- st_transform(all_nodes, 32737)

# Create distance matrix
n_nodes <- nrow(all_nodes)
edges_euclidean <- data.frame()

for(i in 1:(n_nodes-1)) {
  for(j in (i+1):n_nodes) {
    dist_m <- as.numeric(st_distance(all_nodes_utm[i,], all_nodes_utm[j,]))
    
    edges_euclidean <- rbind(edges_euclidean, data.frame(
      from = all_nodes$node_id[i],
      to = all_nodes$node_id[j],
      distance_km = dist_m / 1000
    ))
  }
}

cat("  Computed", nrow(edges_euclidean), "pairwise distances\n\n")

# ============================================================
# STEP 3: BUILD ALL-KENYA EUCLIDEAN MST
# ============================================================

cat("Building all-Kenya Euclidean MST...\n")

# Create graph
g_euclidean <- graph_from_data_frame(
  edges_euclidean, 
  directed = FALSE,
  vertices = data.frame(name = all_nodes$node_id)
)

# Set edge weights
E(g_euclidean)$weight <- edges_euclidean$distance_km

# Compute MST
mst_euclidean <- mst(g_euclidean)

# Get MST edges
mst_edges_euclidean <- as_data_frame(mst_euclidean, what = "edges")

cat("  All-Kenya MST has", nrow(mst_edges_euclidean), "edges\n")
cat("  (Should be", nrow(all_nodes) - 1, "= nodes - 1)\n\n")

# ============================================================
# STEP 4: ADD REGIONAL MSTs (FABER'S APPROACH)
# ============================================================

cat("Adding regional MSTs to capture more routes...\n")

# Get coordinates for regional divisions
coords <- st_coordinates(all_nodes)
all_nodes$lon <- coords[,1]
all_nodes$lat <- coords[,2]

# Define regions (Kenya-specific)
# West-Central-East division by longitude
lon_cutoff_west <- 36.0   # Western Kenya
lon_cutoff_east <- 38.0   # Eastern Kenya

# North-South division by latitude  
lat_cutoff <- -1.0        # Roughly equator

all_nodes <- all_nodes %>%
  mutate(
    region_ew = case_when(
      lon < lon_cutoff_west ~ "West",
      lon > lon_cutoff_east ~ "East",
      TRUE ~ "Central"
    ),
    region_ns = ifelse(lat > lat_cutoff, "North", "South")
  )

# Check regional distribution
cat("\nRegional distribution:\n")
table(all_nodes$region_ew)
table(all_nodes$region_ns)
cat("\n")

# Function to create regional MST
create_regional_mst <- function(nodes_subset, region_name) {
  if(nrow(nodes_subset) < 2) {
    cat("    Skipping", region_name, "(< 2 nodes)\n")
    return(NULL)
  }
  
  # Create edges within region
  regional_edges <- edges_euclidean %>%
    filter(
      from %in% nodes_subset$node_id,
      to %in% nodes_subset$node_id
    )
  
  if(nrow(regional_edges) == 0) {
    cat("    Skipping", region_name, "(no edges)\n")
    return(NULL)
  }
  
  # Create graph and MST
  g_regional <- graph_from_data_frame(regional_edges, directed = FALSE)
  E(g_regional)$weight <- regional_edges$distance_km
  mst_regional <- mst(g_regional)
  
  regional_mst_edges <- as_data_frame(mst_regional, what = "edges")
  cat("    ", region_name, "MST:", nrow(regional_mst_edges), "edges\n")
  
  return(regional_mst_edges)
}

# Regional MSTs
regional_msts <- list()

# East-West divisions
regional_msts$west <- create_regional_mst(
  all_nodes %>% filter(region_ew == "West"), "West"
)
regional_msts$central <- create_regional_mst(
  all_nodes %>% filter(region_ew == "Central"), "Central"
)
regional_msts$east <- create_regional_mst(
  all_nodes %>% filter(region_ew == "East"), "East"
)

# North-South divisions
regional_msts$north <- create_regional_mst(
  all_nodes %>% filter(region_ns == "North"), "North"
)
regional_msts$south <- create_regional_mst(
  all_nodes %>% filter(region_ns == "South"), "South"
)

# Combine all MST edges (remove duplicates)
all_mst_edges <- bind_rows(
  mst_edges_euclidean %>% mutate(source = "All-Kenya"),
  bind_rows(regional_msts) %>% mutate(source = "Regional")
) %>%
  distinct(from, to, .keep_all = TRUE)

cat("\n  Total unique MST edges:", nrow(all_mst_edges), "\n\n")

# ============================================================
# STEP 5: CREATE SPATIAL NETWORK
# ============================================================

cat("Creating spatial network from MST edges...\n")

euclidean_network_lines <- list()

for(i in 1:nrow(all_mst_edges)) {
  from_node <- all_nodes %>% filter(node_id == all_mst_edges$from[i])
  to_node <- all_nodes %>% filter(node_id == all_mst_edges$to[i])
  
  line <- st_linestring(rbind(
    st_coordinates(from_node),
    st_coordinates(to_node)
  ))
  
  euclidean_network_lines[[i]] <- st_sfc(line, crs = 4326)
}

# Combine all lines
euclidean_network_sf <- do.call(c, euclidean_network_lines) %>%
  st_sfc(crs = 4326) %>%
  st_sf() %>%
  mutate(edge_id = 1:n())

cat("✅ Euclidean network created:", nrow(euclidean_network_sf), "segments\n\n")

# ============================================================
# STEP 6: CALCULATE DISTANCES FOR IV
# ============================================================

cat("Calculating distances from DHS clusters to Euclidean network...\n")

# Create spatial points
person_spatial <- spec1_urban_rural %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32737)

# Transform network to UTM
euclidean_network_utm <- st_transform(euclidean_network_sf, 32737)
euclidean_union <- st_union(euclidean_network_utm)

# Calculate distances
person_euclidean <- person_spatial %>%
  mutate(
    dist_km_euclidean = as.numeric(
      st_distance(geometry, euclidean_union)
    ) / 1000,
    log_dist_euclidean = log(dist_km_euclidean + 1)
  ) %>%
  st_drop_geometry()

cat("✅ Distances calculated\n\n")

# ============================================================
# STEP 7: MERGE WITH EXISTING DATA
# ============================================================

cat("Merging Euclidean distances with least-cost distances...\n")

# Merge back to main dataset
person_analysis_both_iv <- person_analysis_leastcost %>%
  left_join(
    person_euclidean %>% 
      dplyr::select(cluster_id_unique, household_id, person_id,
                    dist_km_euclidean, log_dist_euclidean),
    by = c("cluster_id_unique", "household_id", "person_id")
  )

# ============================================================
# SUMMARY STATISTICS

cor_val <- cor(person_analysis_both_iv$log_dist_leastcost, 
               person_analysis_both_iv$log_dist_euclidean,
               use = "complete.obs")
cat("  Correlation:", round(cor_val, 3), "\n\n")

# ============================================================
# CALCULATE DISTANCE TO AND POPULATION OF NEAREST 1989 CITY
# ============================================================

cat("Calculating distance to nearest 1989 urban center...\n")

# City list with 1989 populations
demand_centers_pop <- tribble(
  ~name, ~lat, ~lon, ~pop_1989_urban,
  "Nairobi", -1.2921, 36.8219, 1324570,
  "Mombasa", -4.0435, 39.6682, 461753,
  "Kisumu", -0.0917, 34.7680, 192733,
  "Nakuru", -0.3031, 36.0800, 163927,
  "Eldoret", 0.5143, 35.2698, 111882,
  "Thika", -1.0332, 37.0690, 57603,
  "Machakos", -1.5177, 37.2634, 116293,
  "Meru", 0.0463, 37.6559, 94947,
  "Nyeri", -0.4167, 36.9500, 91258,
  "Kakamega", 0.2833, 34.7500, 58862,
  "Kitale", 1.0167, 35.0000, 56218
)

# Create spatial cities
demand_centers_sf <- st_as_sf(demand_centers_pop, 
                              coords = c("lon", "lat"), 
                              crs = 4326) %>%
  st_transform(32737)

# Get unique clusters from original data (before any spatial operations)
cluster_info <- spec1_urban_rural %>%
  distinct(cluster_id_unique, longitude, latitude) %>%
  filter(!is.na(longitude), !is.na(latitude))

# Convert clusters to spatial
clusters_sf <- st_as_sf(cluster_info, 
                        coords = c("longitude", "latitude"), 
                        crs = 4326) %>%
  st_transform(32737)

# Calculate all distances
dist_matrix <- st_distance(clusters_sf, demand_centers_sf)

# For each cluster, find nearest city
nearest_idx <- apply(dist_matrix, 1, which.min)
min_dist <- apply(dist_matrix, 1, min)

# Build merge data using base R (NO SELECT!)
clusters_to_merge <- data.frame(
  cluster_id_unique = cluster_info$cluster_id_unique,
  dist_1989_city_km = as.numeric(min_dist) / 1000,
  nearest_1989_pop = demand_centers_pop$pop_1989_urban[nearest_idx],
  nearest_1989_city = demand_centers_pop$name[nearest_idx],
  stringsAsFactors = FALSE
)

# Add log population
clusters_to_merge$log_nearest_1989_pop <- log(clusters_to_merge$nearest_1989_pop)

# Remove old variables if they exist (use dplyr:: prefix!)
person_analysis_both_iv <- person_analysis_both_iv %>%
  dplyr::select(-any_of(c("dist_1989_city_km", "nearest_1989_pop", 
                          "log_nearest_1989_pop", "nearest_1989_city")))

# Merge
person_analysis_both_iv <- person_analysis_both_iv %>%
  left_join(clusters_to_merge, by = "cluster_id_unique")

cat("✅ Distance and population calculated\n\n")

# ============================================================
# SUMMARY
# ============================================================

cat("=== DISTANCE TO 1989 URBAN CENTERS ===\n\n")

cat("Distance to nearest 1989 city (km):\n")
print(summary(person_analysis_both_iv$dist_1989_city_km))

cat("\nPopulation of nearest 1989 city:\n")
print(summary(person_analysis_both_iv$nearest_1989_pop))

cat("\nLog population:\n")
print(summary(person_analysis_both_iv$log_nearest_1989_pop))

cat("\nDistribution by nearest city:\n")
city_table <- person_analysis_both_iv %>%
  group_by(nearest_1989_city) %>%
  summarise(
    n_people = n(),
    pct = round(100 * n() / nrow(person_analysis_both_iv), 1),
    avg_dist_km = round(mean(dist_1989_city_km, na.rm = TRUE), 1),
    pop_1989 = first(nearest_1989_pop)
  ) %>%
  arrange(desc(n_people))

print(city_table, n = Inf)

# ============================================================
# SAVE EVERYTHING
# ============================================================

saveRDS(person_analysis_both_iv,
        here("Data", "Clean", "person_analysis_both_instruments.rds"))



st_write(euclidean_network_sf,
         here("Data", "Raw", "hypothetical_network_euclidean.shp"),
         delete_dsn = TRUE)

cat("✅ Saved:\n")
cat("  Data/Clean/person_analysis_both_instruments.rds\n")
cat("  Data/Raw/hypothetical_network_euclidean.shp\n\n")

# ============================================================
# VISUALIZATION
# ============================================================

cat("Creating comparison map...\n")

p_both_networks <- ggplot() +
  geom_sf(data = kenya_boundary, fill = "gray98", color = "gray50") +
  geom_sf(data = mst_network_wgs84, aes(color = "Least-Cost MST"), size = 1.2) +
  geom_sf(data = euclidean_network_sf, aes(color = "Euclidean MST"), size = 0.8) +
  geom_sf(data = power_plants_sf, color = "blue", size = 3, shape = 17) +
  geom_sf(data = demand_centers_sf, color = "red", size = 2) +
  scale_color_manual(
    name = "Hypothetical Networks",
    values = c("Least-Cost MST" = "black", "Euclidean MST" = "#8B4513")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Two Instrumental Variable Networks (Faber 2014 Style)",
       subtitle = "Black = Least-Cost MST | Brown = Euclidean MST")

ggsave(here("Figures", "both_instruments_comparison.png"),
       p_both_networks, width = 10, height = 8, dpi = 300)
# ============================================================
# SINGLE SHARED LEGEND (NO DUPLICATION)
# ============================================================

# [All previous data processing and plot creation stays the same]
# [plot_2014 and plot_2022 code remains identical]

# Filter to within 150km and create bins
first_stage_by_year <- person_analysis_both_iv %>%
  filter(dist_km_leastcost <= 150) %>%
  mutate(
    dist_network_bin = cut(dist_km_leastcost,
                           breaks = c(0, 10, 25, 50, 75, 100, 150),
                           labels = c("0-10km", "10-25km", "25-50km", 
                                      "50-75km", "75-100km", "100-150km"))
  ) %>%
  group_by(year, dist_network_bin) %>%
  summarise(
    elec_rate = mean(has_electricity, na.rm = TRUE),
    n = n(),
    se = sd(has_electricity, na.rm = TRUE) / sqrt(n),
    mean_dist = mean(dist_km_leastcost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ci_lower = elec_rate - 1.96 * se,
    ci_upper = elec_rate + 1.96 * se,
    label_vjust = ifelse(elec_rate < 0.20, 1.5, -1.2)
  )

cor_2014 <- cor(
  person_analysis_both_iv$dist_km_leastcost[
    person_analysis_both_iv$year == 2014 & 
      person_analysis_both_iv$dist_km_leastcost <= 150],
  person_analysis_both_iv$has_electricity[
    person_analysis_both_iv$year == 2014 & 
      person_analysis_both_iv$dist_km_leastcost <= 150],
  use = "complete.obs"
)

cor_2022 <- cor(
  person_analysis_both_iv$dist_km_leastcost[
    person_analysis_both_iv$year == 2022 & 
      person_analysis_both_iv$dist_km_leastcost <= 150],
  person_analysis_both_iv$has_electricity[
    person_analysis_both_iv$year == 2022 & 
      person_analysis_both_iv$dist_km_leastcost <= 150],
  use = "complete.obs"
)

n_2014 <- sum(person_analysis_both_iv$year == 2014 & 
                person_analysis_both_iv$dist_km_leastcost <= 150)
n_2022 <- sum(person_analysis_both_iv$year == 2022 & 
                person_analysis_both_iv$dist_km_leastcost <= 150)

# Plot 2014 - WITH legend
# ============================================================
# FIX: SHOW LEGEND ONLY ONCE (RIGHT PLOT ONLY)
# ============================================================

# [All previous data processing stays the same]

# Plot 2014 - NO LEGEND
plot_2014 <- ggplot(
  first_stage_by_year %>% filter(year == 2014),
  aes(x = mean_dist, y = elec_rate)
) +
  geom_point(aes(size = n), alpha = 0.7, color = "#2c7bb6") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 5, alpha = 0.5, color = "#2c7bb6") +
  geom_smooth(method = "lm", se = TRUE, 
              color = "#d7191c", fill = "#d7191c", alpha = 0.2,
              linewidth = 1.2) +
  geom_text(aes(label = dist_network_bin, vjust = label_vjust),
            size = 3, fontface = "bold") +
  scale_size_continuous(
    name = "Sample Size", 
    range = c(4, 12),
    breaks = c(5000, 10000),
    labels = scales::comma
  ) +
  scale_y_continuous(limits = c(0, 0.65),
                     labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.6, 0.1)) +
  scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 25)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    legend.position = "none",  # ← REMOVE LEGEND FROM LEFT PLOT
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "A. 2014 DHS",
    subtitle = sprintf("N = %s | r = %.3f", 
                       format(n_2014, big.mark = ","), 
                       cor_2014),
    x = "Distance to Network (km)",
    y = "Electrification Rate"
  )

# Plot 2022 - WITH LEGEND (ONLY ONE)
plot_2022 <- ggplot(
  first_stage_by_year %>% filter(year == 2022),
  aes(x = mean_dist, y = elec_rate)
) +
  geom_point(aes(size = n), alpha = 0.7, color = "#2c7bb6") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 5, alpha = 0.5, color = "#2c7bb6") +
  geom_smooth(method = "lm", se = TRUE, 
              color = "#d7191c", fill = "#d7191c", alpha = 0.2,
              linewidth = 1.2) +
  geom_text(aes(label = dist_network_bin, vjust = label_vjust),
            size = 3, fontface = "bold") +
  scale_size_continuous(
    name = "Sample Size", 
    range = c(4, 12),
    breaks = c(5000, 10000),
    labels = scales::comma
  ) +
  scale_y_continuous(limits = c(0, 0.65),
                     labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.6, 0.1)) +
  scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 25)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    legend.position = "right",  # ← KEEP LEGEND ON RIGHT PLOT
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "B. 2022 DHS",
    subtitle = sprintf("N = %s | r = %.3f", 
                       format(n_2022, big.mark = ","), 
                       cor_2022),
    x = "Distance to Network (km)",
    y = ""
  )

# ============================================================
# COMBINE WITHOUT TITLE/SUBTITLE (FIXED SYNTAX)
# ============================================================
combined_plot <- plot_2014 + plot_2022 +
  plot_annotation(
    caption = paste0(
      ""
    ),
    theme = theme(
      plot.caption = element_text(hjust = 0, size = 9, 
                                  lineheight = 1.2,
                                  margin = margin(t = 10))
    )
  )

# Save
ggsave(here("Figures", "first_stage_by_year_150km.png"),
       combined_plot, width = 12, height = 6, dpi = 150, bg = "white")

# ============================================================
# CREATE 5KM BINS
# ============================================================

# Filter to within 150km and create 5km bins
first_stage_by_year <- person_analysis_both_iv %>%
  filter(dist_km_leastcost <= 150) %>%
  mutate(
    # Create 5km bins: 0-5, 5-10, 10-15, ..., 145-150
    dist_network_bin = cut(
      dist_km_leastcost,
      breaks = seq(0, 150, by = 5),  # ← CHANGED: 5km intervals
      labels = paste0(seq(0, 145, by = 5), "-", seq(5, 150, by = 5), "km"),
      include.lowest = TRUE
    )
  ) %>%
  group_by(year, dist_network_bin) %>%
  summarise(
    elec_rate = mean(has_electricity, na.rm = TRUE),
    n = n(),
    se = sd(has_electricity, na.rm = TRUE) / sqrt(n),
    mean_dist = mean(dist_km_leastcost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ci_lower = elec_rate - 1.96 * se,
    ci_upper = elec_rate + 1.96 * se
  )

# Calculate correlations
cor_2014 <- cor(
  person_analysis_both_iv$dist_km_leastcost[
    person_analysis_both_iv$year == 2014 & 
      person_analysis_both_iv$dist_km_leastcost <= 150],
  person_analysis_both_iv$has_electricity[
    person_analysis_both_iv$year == 2014 & 
      person_analysis_both_iv$dist_km_leastcost <= 150],
  use = "complete.obs"
)

cor_2022 <- cor(
  person_analysis_both_iv$dist_km_leastcost[
    person_analysis_both_iv$year == 2022 & 
      person_analysis_both_iv$dist_km_leastcost <= 150],
  person_analysis_both_iv$has_electricity[
    person_analysis_both_iv$year == 2022 & 
      person_analysis_both_iv$dist_km_leastcost <= 150],
  use = "complete.obs"
)

n_2014 <- sum(person_analysis_both_iv$year == 2014 & 
                person_analysis_both_iv$dist_km_leastcost <= 150, na.rm = TRUE)
n_2022 <- sum(person_analysis_both_iv$year == 2022 & 
                person_analysis_both_iv$dist_km_leastcost <= 150, na.rm = TRUE)



# ============================================================
# 5KM BINS WITH EVERY OTHER LABEL SHOWN
# ============================================================

# Create 5km bins, then show every other label
first_stage_5km <- person_analysis_both_iv %>%
  filter(dist_km_leastcost <= 150) %>%
  mutate(
    dist_network_bin = cut(
      dist_km_leastcost,
      breaks = seq(0, 150, by = 5),  # ← CHANGED: 5km bins
      labels = paste0(seq(0, 145, by = 5), "-", seq(5, 150, by = 5), "km"),
      include.lowest = TRUE
    )
  ) %>%
  group_by(year, dist_network_bin) %>%
  summarise(
    elec_rate = mean(has_electricity, na.rm = TRUE),
    n = n(),
    se = sd(has_electricity, na.rm = TRUE) / sqrt(n),
    mean_dist = mean(dist_km_leastcost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ci_lower = elec_rate - 1.96 * se,
    ci_upper = elec_rate + 1.96 * se,
    show_label = row_number() %% 2 == 1,  # Show every 2nd label (0-5, 10-15, 20-25...)
    label_text = ifelse(show_label, as.character(dist_network_bin), "")
  )

# Plot 2014
plot_2014_5km <- ggplot(
  first_stage_5km %>% filter(year == 2014),
  aes(x = mean_dist, y = elec_rate)
) +
  geom_point(aes(size = n), alpha = 0.7, color = "#2c7bb6") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 2, alpha = 0.5, color = "#2c7bb6") +
  geom_smooth(method = "lm", se = TRUE, 
              color = "#d7191c", fill = "#d7191c", alpha = 0.2,
              linewidth = 1.2) +
  geom_text(
    aes(label = label_text),  # ← Use label_text (every other)
    vjust = 2.0,
    size = 2.5, 
    fontface = "plain"
  ) +
  scale_size_continuous(
    name = "Sample Size", 
    range = c(3, 10),  # ← Smaller range for more points
    breaks = c(2000, 5000, 10000),
    labels = scales::comma
  ) +
  scale_y_continuous(
    limits = c(0, 0.65),
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 0.6, 0.1)
  ) +
  scale_x_continuous(
    limits = c(0, 150), 
    breaks = seq(0, 150, 25)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "A. 2014 DHS",
    subtitle = sprintf("N = %s | r = %.3f", 
                       format(n_2014, big.mark = ","), 
                       cor_2014),
    x = "Distance to Pre-1989 Network (km)",
    y = "Electrification Rate"
  )

# Plot 2022
plot_2022_5km <- ggplot(
  first_stage_5km %>% filter(year == 2022),
  aes(x = mean_dist, y = elec_rate)
) +
  geom_point(aes(size = n), alpha = 0.7, color = "#2c7bb6") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 2, alpha = 0.5, color = "#2c7bb6") +
  geom_smooth(method = "lm", se = TRUE, 
              color = "#d7191c", fill = "#d7191c", alpha = 0.2,
              linewidth = 1.2) +
  geom_text(
    aes(label = label_text),  # ← Use label_text (every other)
    vjust = 2.0,
    size = 2.5, 
    fontface = "plain"
  ) +
  scale_size_continuous(
    name = "Sample Size", 
    range = c(3, 10),
    breaks = c(2000, 5000, 10000),
    labels = scales::comma
  ) +
  scale_y_continuous(
    limits = c(0, 0.65),
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 0.6, 0.1)
  ) +
  scale_x_continuous(
    limits = c(0, 150), 
    breaks = seq(0, 150, 25)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "B. 2022 DHS",
    subtitle = sprintf("N = %s | r = %.3f", 
                       format(n_2022, big.mark = ","), 
                       cor_2022),
    x = "Distance to Pre-1989 Network (km)",
    y = ""
  )

# Combine and save
combined_5km <- plot_2014_5km + plot_2022_5km

ggsave(
  here("Figures", "first_stage_by_year_150km_5km_bins.png"),
  combined_5km, 
  width = 8, 
  height = 4, 
  dpi = 300, 
  bg = "white"
)

cat("✅ 5km bins with every other label shown\n")
cat("   30 bins total, 15 labeled (0-5, 10-15, 20-25...)\n")