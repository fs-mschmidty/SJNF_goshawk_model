library(reticulate)
library(rgee)
library(sf)
library(targets)
library(tidyverse)
library(sjnftools)
library(terra)
library(googledrive)

ee_Authenticate()

ee_Initialize(
  auth_mode = "notebook",
  drive = T
)

sak_file <- "C:\\Users\\MichaelSchmidt2\\Downloads\\learn-py-gee-mschmidty-fc594af8674e.json"

ee_utils_sak_copy(
  sakfile = sak_file
)

points <- tar_read(psuedoabs)

tc <- ee$ImageCollection("OREGONSTATE/PRISM/Norm91m")

tmax_monthly <- tc$select("tmax")

annual_tmax <- tmax_monthly$max()

pts_ee <- sf_as_ee(tar_read(psuedoabs) |> st_transform(4326))

extracted_ee <- annual_tmax$reduceRegions(
  collection = pts_ee,
  reducer = ee$Reducer$max(),
  scale = 800
)

sf_pts <- ee_as_sf(extracted_ee) |>
  as_tibble() |>
  select(id, annual_max_temp = max)


annual_temp_img <- function(year) {
  imgs <- tc$filter(ee$Filter$calendarRange(year, year, "year"))$select(c(
    "tmmx",
    "tmmn"
  ))$map(function(img) {
    tmean <- img$select("tmmx")$add(img$select("tmmn"))$divide(2)
  })

  imgs$reduce(ee$Reducer$mean())$rename(paste0("tmean_", year))
}

# Store annual values
annual_vals <- list()

years <- 2010:2020

for (yr in years) {
  img <- annual_temp_img(yr)

  df <- rgee::ee_extract(
    x = img,
    y = pts_ee,
    fun = rgee::ee$Reducer$first(),
    scale = scale,
    sf = FALSE
  )

  annual_vals[[as.character(yr)]] <- df[[1]] # the single column of temps
}

# Long-term average per point
avg_temp <- rowMeans(as.data.frame(annual_vals), na.rm = TRUE)

# Return sf with new variable
cbind(points, avg_annual_temp = avg_temp)

lon <- -108.5
lat <- 37.5

# 1. Build the sf object correctly
# bd <- st_as_sf(
#   data.frame(lon = lon, lat = lat),
#   coords = c("lon", "lat"),
#   crs = 4326 # Assign WGS84 explicitly at creation
# ) |>
#   st_buffer(dist = 50000)
poly_bbox <- crd_bd |>
  st_transform(4326) |>
  st_bbox()

roi_ee <- ee$geometry$bbox(
  west = poly_bbox[["xmin"]],
  south = poly_bbox[["ymin"]],
  east = poly_bbox[["xmax"]],
  north = poly_bbox[["ymax"]]
)

roi_ee <- sf_as_ee(poly_bbox)$geometry()

monthly_indices <- as.list(sprintf("%02d", 1:12))

prism_months <- ee$ImageCollection("OREGONSTATE/PRISM/Norm91m") %>%
  ee$ImageCollection$select("tmax") %>%
  ee$ImageCollection$filter(ee$Filter$inList("system:index", monthly_indices))


annual_tmax <- prism_months$max()
tmax_pixel_max <- annual_tmax$clip(roi_ee)

annual_tmax_rast <- ee_as_rast(
  image = tmax_pixel_max,
  region = roi_ee,
  scale = 800,
  via = "getDownloadURL"
)
# Reproject to a localized CRS (e.g., NAD83 / UTM Zone 13N)
tmax_projected <- project(annual_tmax_rast, "EPSG:26913")

# Plot the corrected raster
plot(tmax_projected, main = "Peak Annual Tmax Normal (Celsius)")
print(minmax(annual_tmax_rast))
plot(annual_tmax_rast)

annual_tmax_rast |>
  project(mask_rast) |>
  crop(mask_rast, mask = TRUE)

library(rgee)
library(sf)
library(terra)
library(googledrive)
ee_Initialize()
# 1. Initialize EE with Drive credentials
ee_Initialize(drive = TRUE)

# 2. Define Area
# poly_bbox <- st_as_sf(
#   data.frame(lon = -108.5, lat = 37.5),
#   coords = c("lon", "lat"),
#   crs = 4326
# ) |>
#   st_buffer(dist = 50000)
#
# roi_ee <- sf_as_ee(poly_bbox)$geometry()

poly_bbox <- crd_bd |>
  st_transform(4326) |>
  st_bbox()

roi_ee <- ee$Geometry$BBox(
  west = poly_bbox[["xmin"]],
  south = poly_bbox[["ymin"]],
  east = poly_bbox[["xmax"]],
  north = poly_bbox[["ymax"]]
)
# 3. Filter exactly the 12 months
months_list <- ee$List(list(
  "01",
  "02",
  "03",
  "04",
  "05",
  "06",
  "07",
  "08",
  "09",
  "10",
  "11",
  "12"
))

prism_months <- ee$ImageCollection("OREGONSTATE/PRISM/Norm91m") %>%
  ee$ImageCollection$select("tmax") %>%
  ee$ImageCollection$filter(ee$Filter$inList("system:index", months_list))

# 4. Calculate the Annual Mean on the fly
# Taking the mean() of the 12 monthly normals gives you the annual normal
annual_mean_tmax <- prism_months$mean()$clip(roi_ee)$toFloat()

# 5. Export to Google Drive
print("Submitting task to Google Drive...")
task <- ee_image_to_drive(
  image = annual_mean_tmax,
  description = "PRISM_Annual_Mean_Tmax_crd",
  folder = "rgee_exports", # Will create this folder in your Drive
  region = roi_ee,
  scale = 800,
  crs = "EPSG:4269", # Keep native projection
  maxPixels = 1e9 # Prevent memory caps
)

task$start()

# 6. Monitor the Task
# R will pause here and show you the status. It usually takes 1-2 minutes.
ee_monitoring(task)

# 7. Download from Drive to your local machine
# 7. Download safely from the specific folder
print("Task complete. Locating file in the rgee_exports folder...")

# Step A: Point R exactly to the folder Earth Engine created
export_folder <- drive_get("rgee_exports")

# Step B: Search ONLY inside that folder for your raster
drive_file <- drive_ls(
  path = export_folder,
  pattern = "PRISM_Annual_Mean_Tmax_crd"
)

# Download the most recent match found inside that folder
drive_download(
  file = drive_file[1, ],
  path = "PRISM_Annual_Mean_Tmax_crd.tif",
  overwrite = TRUE
)
# 8. Load and Plot
annual_mean_rast <- rast("PRISM_Annual_Mean_Tmax_crd.tif")

# Clean any background masking artifacts just in case
annual_mean_rast[is.infinite(annual_mean_rast) | is.nan(annual_mean_rast)] <- NA

plot(annual_mean_rast, main = "30-Year Annual Mean Tmax (Celsius)")

reproject_amr <- project(annual_mean_rast, tar_read(treemap_covs)) |>
  crop(tar_read(treemap_covs)$ForTypName, mask = T)

plot(reproject_amr)
c(tar_read(treemap_covs)$ForTypName, reproject_amr)

tar_read(treemap_covs)$ForTypName

ph_image <- ee$Image("OpenLandMap/SOL/SOL_PH-H2O_USDA-4C1A2A_M/v02")

# 2. Convert points to EE
pts_ee <- sf_as_ee(tar_read(psuedoabs) |> st_transform(4326))

# 3. Extract values (stays server-side)
extracted_ee <- ph_image$reduceRegions(
  collection = pts_ee,
  reducer = ee$Reducer$first(),
  scale = 250
)

# 4. Export to Drive
task <- ee$batch$Export$table$toDrive(
  collection = extracted_ee,
  folder = "rgee_exports", # Will create this folder in your Drive
  description = "soil_ph_export",
  fileFormat = "GeoJSON"
)
task$start()

# 5. Monitor
ee_monitoring(task, quiet = FALSE)

# 6. Download from Drive to temp file
drive_file <- drive_ls(
  path = "rgee_exports",
  pattern = "soil_ph_export"
)

# Download the most recent match found inside that folder
drive_download(
  file = drive_file[1, ],
  path = "soil_ph_test.json",
  overwrite = TRUE
)
temp_dir <- tempdir()
ee_drive_to_local(task = task, dsn = temp_dir)

# 7. Read the file (Drive writes .geojson)
file_path <- list.files(temp_dir, pattern = "geojson$", full.names = TRUE)[1]
soil_sf <- sf::st_read("soil_ph_test.json", quiet = TRUE)

# 8. Clean + return same format you had
soil_sf |>
  dplyr::as_tibble() |>
  dplyr::select(-geometry) |>
  dplyr::rename_with(~ paste0("ph_", .x), c(b0, b10, b30, b60, b100, b200)) |>
  dplyr::select(id, ph_b60) |>
  dplyr::mutate(ph_b60 = as.numeric(ph_b60))


drive_ee_download <- function(ee_call) {
  # 4. Export to Drive
  task_desk <- paste0(
    "ee_download_",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )

  task <- ee$batch$Export$table$toDrive(
    collection = ee_call,
    folder = "rgee_exports",
    description = task_desk,
    fileFormat = "GeoJSON"
  )
  task$start()

  # 5. Monitor
  ee_monitoring(task, quiet = FALSE)

  # 6. Download from Drive to temp file
  export_folder <- googledrive::drive_get("rgee_exports")
  drive_file <- googledrive::drive_ls(
    path = export_folder,
    pattern = task_desk
  )

  temp_file <- tempfile(fileext = ".json")

  googledrive::drive_download(
    file = drive_file[1, ],
    path = temp_file,
    overwrite = TRUE
  )

  # 7. Read the file (Drive writes .geojson)
  sf_obj <- sf::st_read(temp_file, quiet = TRUE)
  googledrive::drive_rm(drive_file[1, ])
  return(sf_obj)
}
tc <- ee$ImageCollection("OREGONSTATE/PRISM/Norm91m")

## Annual Max Temp
tmax_monthly <- tc$select("tmax")
tmin_monthly <- tc$select("tmin")

annual_tmax <- tmax_monthly$max()

pts_ee <- sf_as_ee(tar_read(psuedoabs) |> st_transform(4326))

extracted_ee <- annual_tmax$reduceRegions(
  collection = pts_ee,
  reducer = ee$Reducer$max(),
  scale = 800
)

annual_max_temp <- drive_ee_download(extracted_ee) |>
  as_tibble() |>
  select(id, annual_max_temp = max)

# --- 1. Compute Earth Engine Terrain Stack ---
dem <- ee$ImageCollection('COPERNICUS/DEM/GLO30')$select('DEM')$mosaic()
terrain_products <- ee$Terrain$products(dem)

slope <- terrain_products$select('slope')
aspect <- terrain_products$select('aspect')

tpi_kernel <- ee$Kernel$circle(radius = 3, units = 'pixels')
mean_dem <- dem$reduceNeighborhood(
  reducer = ee$Reducer$mean(),
  kernel = tpi_kernel
)
# mean_dem <- dem$focal_mean(kernel = tpi_kernel)
tpi <- dem$subtract(mean_dem)$rename('TPI')

tri_kernel <- ee$Kernel$square(radius = 1, units = 'pixels')
# 2. Compute focal sum of the DEM (s)
dem_sum <- dem$reduceNeighborhood(
  reducer = ee$Reducer$sum(),
  kernel = tri_kernel
)

# 3. Compute the square of the DEM (E^2)
dem_sq <- dem$multiply(dem)

# 4. Compute focal sum of the squared DEM (t)
dem_sq_sum <- dem_sq$reduceNeighborhood(
  reducer = ee$Reducer$sum(),
  kernel = tri_kernel
)

# 5. Use an expression to compute Riley's formula
# Formula expanded: sqrt(dem_sq_sum + (9 * dem_sq) - (2 * dem * dem_sum))
tri <- dem_sq_sum$add(dem_sq$multiply(9))$subtract(
  dem$multiply(2)$multiply(dem_sum)
)$sqrt()$rename('TRI')

terrain_stack <- dem$addBands(slope)$addBands(aspect)$addBands(tpi)$addBands(
  tri
)$rename(c('elevation', 'slope', 'aspect', 'TPI', 'TRI'))
geom_input <- tar_read(psuedoabs)
# --- 2. Polymorphic Input Normalization & Routing ---
is_ee_raster <- inherits(geom_input, "ee.image.Image") ||
  inherits(geom_input, "ee.imagecollection.ImageCollection")

if (is_ee_raster) {
  geom_type <- "Raster"
  ee_raster_geometry <- geom_input$geometry()
  processed_ee <- terrain_stack$clip(ee_raster_geometry)
} else {
  if (inherits(geom_input, "sf") || inherits(geom_input, "sfc")) {
    message("Local R 'sf' layer detected. Converting to GEE environment...")
    geom_string <- as.character(sf::st_geometry_type(
      geom_input,
      by_geometry = FALSE
    ))
    ee_geom <- sf_as_ee(geom_input)
  } else {
    if (inherits(geom_input, "ee.geometry.Geometry")) {
      ee_type_obj <- geom_input$type()
      geom_string <- ee_type_obj$getInfo()
    } else {
      ee_geom_obj <- geom_input$geometry()
      ee_type_obj <- ee_geom_obj$type()
      geom_string <- ee_type_obj$getInfo()
    }
    ee_geom <- geom_input
  }

  if (grepl("Point", geom_string, ignore.case = TRUE)) {
    geom_type <- "Points"
    processed_ee <- terrain_stack$sampleRegions(
      collection = ee_geom,
      scale = 30,
      geometries = TRUE
    )
  } else {
    geom_type <- "Polygon"
    processed_ee <- terrain_stack$clip(ee_geom)
  }
}

task_desc <- paste0(file_prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))

if (geom_type == "Points") {
  message("Launching GEE point extraction table export...")
  task <- ee_table_to_drive(
    collection = processed_ee,
    folder = g_folder,
    description = task_desc,
    fileFormat = "GeoJSON"
  )
  ext <- ".geojson"
} else {
  message("Launching GEE regional raster mosaic export...")
  export_region <- if (is_ee_raster) geom_input$geometry() else ee_geom
  task <- ee$batch$Export$image$toDrive(
    image = processed_ee,
    description = task_desc,
    scale = scale,
    region = export_region,
    fileFormat = "GeoTIFF"
  )
  ext <- ".tif"
}


# --- 3. FIX: Custom Safe Monitoring Loop (Bypasses ee_monitoring Overflow) ---
task$start()
message(
  "Task dispatched safely to Earth Engine Cloud. Monitoring execution state..."
)

ee_monitoring(task, quiet = FALSE)

# --- 4. Locate and Download the File via 'googledrive' ---
message("Locating generated asset inside Google Drive filesystem...")
target_filename <- paste0(task_desc, ext)

Sys.sleep(5)


folder <- googledrive::drive_get("rgee_exports")
file <- googledrive::drive_ls(folder, pattern = task_desc)

# Download to temporary file
local_path <- tempfile(fileext = ".json")
googledrive::drive_download(file[1, ], path = local_path, overwrite = TRUE)

googledrive::drive_rm(file[1, ]) # clean up Drive
t <- sf::st_read(local_path, quiet = TRUE)
# --- 5. Ingest Data Back into R Memory Context ---
if (geom_type == "Points") {
  point_df <- read.csv(tmp_path)
  result <- st_read(point_df, geometry_column = ".geo", quiet = TRUE)
} else {
  result <- terra::rast(tmp_path)
}

return(result)

## Raster return for RGEE
poly_bbox <- crd_bd |>
  st_transform(4326) |>
  st_bbox()

roi_ee <- ee$Geometry$BBox(
  west = poly_bbox[["xmin"]],
  south = poly_bbox[["ymin"]],
  east = poly_bbox[["xmax"]],
  north = poly_bbox[["ymax"]]
)
treemap_img <- ee$ImageCollection('USFS/GTAC/TreeMap/v2022')$first()

# 1. Compute neighborhood mean directly from the main image
mean_canopy_200m <- treemap_img$select('CANOPYPCT')$reduceNeighborhood(
  reducer = ee$Reducer$mean(),
  kernel = ee$Kernel$circle(200, "meters")
)$rename("CANOPYPCT_200M")

# 2. Append the new band to the original image, then slice out your specific 8 bands
target_bands <- c(
  'BALIVE',
  'QMD',
  'TPA_LIVE',
  'TPA_DEAD',
  'CANOPYPCT',
  'STANDHT',
  'SDIsum',
  'CANOPYPCT_200M'
)
raster_stack <- treemap_img$addBands(mean_canopy_200m)$select(
  target_bands
)$clip(roi_ee)$toFloat()

task_desc <- paste0(
  "treemap_rast_",
  format(Sys.time(), "%Y%m%d_%H%M%S")
)

export_folder <- "rgee_exports"

task <- rgee::ee_image_to_drive(
  image = raster_stack,
  description = task_desc,
  folder = export_folder,
  region = roi_ee,
  scale = 30,
  crs = "EPSG:4269",
  maxPixels = 1e9
)

task$start()
rgee::ee_monitoring(task, quiet = FALSE)


# 6. Download safely to a temporary file
export_folder <- googledrive::drive_get("rgee_exports")

repeat {
  drive_file <- googledrive::drive_ls(path = export_folder, pattern = task_desc)

  if (nrow(drive_file) > 0) {
    break
  }

  Sys.sleep(5)
}


temp_tif <- tempfile(fileext = ".tif")

googledrive::drive_download(
  file = drive_file[1, ],
  path = temp_tif,
  overwrite = TRUE
)

geom_input_cl <- crd_bd |>
  st_transform(26913)

# 7. Load into a SpatRaster and name the layer cleanly
final_rast <- terra::rast(temp_tif) |>
  terra::project("epsg:26913") |>
  terra::crop(geom_input_cl, mask = T)

# Clean up Google Drive
googledrive::drive_rm(drive_file[1, ])

return(final_rast)

## Terrain Combo

glo_30_collection <- ee$ImageCollection('COPERNICUS/DEM/GLO30')
glo_30_proj <- glo_30_collection$first()$projection()
dem <- glo_30_collection$select('DEM')$mosaic()$setDefaultProjection(
  glo_30_proj
)

terrain_products <- ee$Terrain$products(dem)

slope <- terrain_products$select('slope')
aspect <- terrain_products$select('aspect')

tpi_kernel <- ee$Kernel$circle(radius = 3, units = 'pixels')
mean_dem <- dem$reduceNeighborhood(
  reducer = ee$Reducer$mean(),
  kernel = tpi_kernel
)
# mean_dem <- dem$focal_mean(kernel = tpi_kernel)
tpi <- dem$subtract(mean_dem)$rename('TPI')

tri_kernel <- ee$Kernel$square(radius = 1, units = 'pixels')
# 2. Compute focal sum of the DEM (s)
dem_sum <- dem$reduceNeighborhood(
  reducer = ee$Reducer$sum(),
  kernel = tri_kernel
)

# 3. Compute the square of the DEM (E^2)
dem_sq <- dem$multiply(dem)

# 4. Compute focal sum of the squared DEM (t)
dem_sq_sum <- dem_sq$reduceNeighborhood(
  reducer = ee$Reducer$sum(),
  kernel = tri_kernel
)

# 5. Use an expression to compute Riley's formula
# Formula expanded: sqrt(dem_sq_sum + (9 * dem_sq) - (2 * dem * dem_sum))
tri <- dem_sq_sum$add(dem_sq$multiply(9))$subtract(
  dem$multiply(2)$multiply(dem_sum)
)$sqrt()$rename('TRI')

raster_stack <- dem$addBands(slope)$addBands(aspect)$addBands(tpi)$addBands(
  tri
)$rename(c('elevation', 'slope', 'aspect', 'TPI', 'TRI'))

geom_input <- tar_read(tree_map_for_type_crd)
e <- geom_input |>
  terra::project("EPSG:4326") |>
  terra::ext()

# convert to Earth Engine BBox
roi_ee <- ee$Geometry$BBox(
  west = e$xmin,
  south = e$ymin,
  east = e$xmax,
  north = e$ymax
)

raster_stack <- raster_stack$clip(roi_ee)$toFloat()

result <- get_gee_rast(raster_stack)

result |>
  terra::project(geom_input) |>
  terra::crop(geom_input, mask = TRUE)

raster_stack <- tar_read(climate_ee_spec)
geom_input <- tar_read(tree_map_for_type_crd)
e <- geom_input |>
  terra::project("EPSG:4326") |>
  terra::ext()

# convert to Earth Engine BBox
roi_ee <- ee$Geometry$BBox(
  west = e$xmin,
  south = e$ymin,
  east = e$xmax,
  north = e$ymax
)

final_raster_stack <- raster_stack$toFloat()

result <- get_gee_rast(
  raster_stack = final_raster_stack,
  roi_ee = roi_ee
) |>
  terra::project(geom_input) |>
  terra::crop(geom_input, mask = TRUE)


months_list <- rgee::ee$List(list(
  "01",
  "02",
  "03",
  "04",
  "05",
  "06",
  "07",
  "08",
  "09",
  "10",
  "11",
  "12"
))

# Helper to build a reducer and return the single-band result

reduce_band <- function(band_name, reducer_fn) {
  rgee::ee$ImageCollection("OREGONSTATE/PRISM/Norm91m")$select(
    band_name
  )$filter(rgee::ee$Filter$inList("system:index", months_list))$reduce(
    reducer_fn
  )
}

# Compute each stat
tmax_max <- reduce_band("tmax", rgee::ee$Reducer$max())$rename("tmax_max")
tmin_min <- reduce_band("tmin", rgee::ee$Reducer$min())$rename("tmin_min")
tmean_mean <- reduce_band("tmean", rgee::ee$Reducer$mean())$rename(
  "tmean_mean"
)
ppt_sum <- reduce_band("ppt", rgee::ee$Reducer$sum())$rename("ppt_sum")

# Combine them into one stacked image
annual_img <- tmax_max$addBands(tmin_min)$addBands(tmean_mean)$addBands(
  ppt_sum
)
result <- download_gee(geom_input = geom_input, raster_stack = annual_img)
geom_input <- tar_read(tree_map_for_type_crd)
e <- geom_input |>
  terra::project("EPSG:4326") |>
  terra::ext()

# convert to Earth Engine BBox
roi_ee <- ee$Geometry$BBox(
  west = e$xmin,
  south = e$ymin,
  east = e$xmax,
  north = e$ymax
)

raster_stack <- annual_img$clip(roi_ee)$toFloat()

result <- get_gee_rast(raster_stack = raster_stack, roi_ee = roi_ee) |>
  terra::project(geom_input) |>
  terra::crop(geom_input, mask = TRUE)


task_desc <- paste0(
  "gee_rast_",
  format(Sys.time(), "%Y%m%d_%H%M%S")
)

export_folder <- "rgee_exports"

task <- rgee::ee_image_to_drive(
  image = raster_stack,
  description = task_desc,
  folder = export_folder,
  region = roi_ee,
  scale = 30,
  crs = "EPSG:4269",
  maxPixels = 1e9
)

task$start()
rgee::ee_monitoring(task, quiet = FALSE)

# 6. Download safely to a temporary file
export_folder <- googledrive::drive_get(export_folder)

repeat {
  drive_file <- googledrive::drive_ls(
    path = export_folder,
    pattern = task_desc
  )

  if (nrow(drive_file) > 0) {
    break
  }

  Sys.sleep(5)
}

temp_tif <- tempfile(fileext = ".tif")

googledrive::drive_download(
  file = drive_file[1, ],
  path = temp_tif,
  overwrite = TRUE
)

# 7. Load into a SpatRaster and name the layer cleanly
final_rast <- terra::rast(temp_tif)
# Clean up Google Drive
googledrive::drive_rm(drive_file[1, ])

return(final_rast)
