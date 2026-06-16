library(terra)
library(sf)
library(tidyverse)

tm <- rast(
  "T:\\FS\\NFS\\SanJuan\\Program\\2600WildlifeMgmt\\GIS\\Lynx\\mapping_guidance_layers\\RDS-2025-0032\\Data\\TreeMap2022_CONUS.tif"
)

ps_a <- tar_read(psuedoabs) |>
  st_transform(crs(tm))

ForTypName <- terra::extract(tm, ps_a)

ForTypName |>
  as_tibble() |>
  count(ForTypName) |>
  View()

get_tree_map_attribute <- function(x, attribute, pts) {
  tm <- rast(x)

  activeCat(tm) <- attribute

  pts_cl <- pts |>
    st_transoform(crs(tm))

  terra::extract(tm, pts_cl) |>
    as_tibble()
}
