# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(sjnftools)
library(geotargets)
# library(tarchetypes) # Load other packages as needed.

set.seed(12345)

options(
  max.print = 100,
  vsc.use_httpgd = TRUE,
  device = "windows"
)
# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "sf",
    "terra",
    "sjnftools",
    "elevatr",
    "janitor",
    "geotargets",
    "tidysdm"
  ), # packages that your targets need to run
  format = "qs", # Optionally set the default storage format. qs is fast.
)
options(clustermq.scheduler = "multiprocess")

tar_source()

list(
  tar_target(proj, "+proj=utm +zone=13"),
  tar_target(epsg, "epsg:26913"),
  ## Need to centroid the nests they are currently polygons.
  tar_target(
    nogo_nest_sites,
    load_and_clean_nogo_nests("data/NorthernGoshawk_R2_NRM_20230731.shp", epsg)
  ),
  tar_target(model_area, build_model_area(r2_bd, nogo_nest_sites, epsg)),
  tar_terra_rast(
    terrain,
    get_elevation_data(model_area)
  ),
  tar_target(
    tree_canopy_cover_file,
    "D:\\GIS_Data\\NLCD\\nlcd_tcc_CONUS_2021_v2021-4\\nlcd_tcc_conus_2021_v2021-4.tif"
  ),
  tar_target(
    landfire_evt_file,
    "D:\\GIS_Data\\Landfire\\LF2023\\LF2023_EVT_240_CONUS\\LF2023_EVT_240_CONUS\\Tif\\LC23_EVT_240.tif"
  ),
  tar_terra_rast(
    tar_load_clip_tree_canopy_cover,
    load_clip_tree_canopy_cover(
      tree_canopy_cover_file,
      model_area,
      projection = proj
    )
  ),
  tar_target(
    thinned_nest_sites,
    tidysdm::thin_by_cell(
      sf::st_transform(nogo_nest_sites, crs(terrain)),
      terrain
    )
  ),
  tar_target(
    psuedoabs,
    sample_pseudoabs(
      thinned_nest_sites,
      tar_load_clip_tree_canopy_cover,
      1000,
      method = c("dist_disc", 2500, 10000)
    ) |>
      mutate(id = row_number())
  ),
  tar_target(
    terrain_cov,
    get_covariates(terrain, psuedoabs, cat = FALSE)
  ),
  tar_target(
    canopy_cover,
    get_covariates(tree_canopy_cover_file, psuedoabs, cat = FALSE) |>
      rename(canopy_cover = Layer_1)
  ),
  tar_target(
    surrounding_canopy_cover,
    build_surrounding_canopy_cover(tree_canopy_cover_file, psuedoabs)
  ),
  tar_target(
    evt_lf,
    get_covariates(landfire_evt_file, psuedoabs, cat = "EVT_LF")
  ),
  tar_target(
    evt_gp_n,
    get_covariates(landfire_evt_file, psuedoabs, cat = "EVT_GP_N")
  ),
  tar_target(
    evt_gp_n_grouped_level,
    build_evt_gp_n_grouped_level(landfire_evt_file, psuedoabs)
  ),
  tar_target(
    evt_gp_n_grouped,
    get_covariates(
      landfire_evt_file,
      psuedoabs,
      new_level = evt_gp_n_grouped_level
    )
  ),
  tar_target(
    evt_height,
    get_evt_height_covariat(
      "D:\\GIS_Data\\Landfire\\LF2023_EVH_240_CONUS\\LF2023_EVH_240_CONUS\\Tif\\LC23_EVH_240.tif",
      psuedoabs
    )
  ),
  tar_target(
    sclass,
    get_covariates(
      "D:\\GIS_Data\\Landfire\\LF2023_SClass_240_CONUS\\LF2023_SClass_240_CONUS\\Tif\\LC23_SCla_240.tif",
      psuedoabs,
      cat = "DESCRIPTIO"
    )
  ),
  # tar_terra_rast(
  #   landfire_evt_gp_n,
  #   load_landfire_evt(
  #     "D:\\GIS_Data\\Landfire\\LF2023\\LF2023_EVT_240_CONUS\\LF2023_EVT_240_CONUS\\Tif\\LC23_EVT_240.tif",
  #     model_area,
  #     "EVT_GP_N"
  #   )
  # ),
  # tar_terra_rast(
  #   landfire_evt_lf,
  #   load_landfire_evt(
  #     "D:\\GIS_Data\\Landfire\\LF2023\\LF2023_EVT_240_CONUS\\LF2023_EVT_240_CONUS\\Tif\\LC23_EVT_240.tif",
  #     model_area,
  #     "EVT_LF"
  #   )
  # ),
  tar_target(
    all_covs,
    psuedoabs |>
      left_join(evt_height, by = "id") |>
      left_join(terrain_cov, by = "id") |>
      left_join(canopy_cover, by = "id") |>
      left_join(surrounding_canopy_cover, by = "id") |>
      left_join(evt_gp_n_grouped, by = "id") |>
      left_join(sclass, by = "id")
  )
  # tar_terra_rast(
  #   cov_raster,
  #   build_cov_raster(terrain, tar_load_clip_tree_canopy_cover)
  # )
)
