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
    "tidysdm",
    "arcgislayers",
    "tidymodels"
  ), # packages that your targets need to run
  format = "qs", # Optionally set the default storage format. qs is fast.
)
options(clustermq.scheduler = "multiprocess")

tar_source()

list(
  tar_target(proj, "+proj=utm +zone=13"),
  tar_target(epsg, "epsg:26913"),
  tar_target(
    fs_regions,
    arc_open(
      "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_RegionBoundaries_01/MapServer/1"
    ) |>
      arc_select() |>
      filter(!REGION %in% c("10", "03", "08", "09")) |>
      st_make_valid()
  ),
  tar_target(
    fs_districts,
    arc_open(
      "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_RangerDistricts_03/MapServer/1"
    ) |>
      arc_select() |>
      st_make_valid() |>
      filter(!REGION %in% c("10", "03", "08", "09")) |>
      st_transform(st_crs(fs_regions))
  ),
  tar_target(
    nogo_nest_sites,
    load_and_clean_nogo_nests(
      "data/All_region_amgo.shp",
      epsg,
      year_cutoff = 2018
    ) |>
      st_transform(st_crs(fs_districts)) |>
      st_intersection(fs_districts)
  ),
  tar_terra_rast(
    elevation_all,
    get_elev_raster(
      st_buffer(nogo_nest_sites, 2000),
      z = 10,
      override_size_check = T,
      clip = "bbox"
    ) |>
      rast()
  ),
  tar_target(
    thinned_nest_sites,
    tidysdm::thin_by_cell(
      sf::st_transform(nogo_nest_sites, crs(elevation_all)),
      elevation_all
    )
  ),
  tar_target(
    psuedoabs,
    sample_pseudoabs(
      thinned_nest_sites,
      elevation_all,
      1200,
      method = c("dist_disc", 2500, 10000)
    ) |>
      mutate(id = row_number())
  ),
  # tar_target(model_area, build_model_area(r2_bd, nogo_nest_sites, epsg)),
  # tar_terra_rast(
  #   terrain,
  #   get_elevation_data(model_area)
  # ),
  tar_target(
    terrain,
    get_terrain_by_region(points = psuedoabs, regions = fs_regions)
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
  # tar_target(
  #   terrain_cov,
  #   get_covariates(terrain, psuedoabs, cat = FALSE)
  # ),
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
    get_covariates_sclass(
      "D:\\GIS_Data\\Landfire\\LF2023_SClass_240_CONUS\\LF2023_SClass_240_CONUS\\Tif\\LC23_SCla_240.tif",
      psuedoabs
    )
  ),
  tar_target(
    all_covs,
    psuedoabs |>
      left_join(evt_height, by = "id") |>
      left_join(terrain, by = "id") |>
      left_join(canopy_cover, by = "id") |>
      left_join(surrounding_canopy_cover, by = "id") |>
      left_join(evt_gp_n_grouped, by = "id") |>
      left_join(sclass, by = "id")
  ),
  tar_target(
    nogo_nest_model,
    build_nogo_nest_model(all_covs)
  ),
  tar_target(
    nogo_nest_model_output,
    save_nogo_nest_model(nogo_nest_model)
  ),

  ##COHA Nest Model
  tar_target(
    coha_nest_sites,
    load_and_clean_nogo_nests(
      "data/COHA_SSHA_SITES_ALL_REGIONS.shp",
      epsg,
      year_cutoff = 2018
    ) |>
      st_transform(st_crs(fs_districts)) |>
      st_intersection(fs_districts) |>
      filter(str_detect(sci_name, "cooperii"))
  ),
  tar_target(
    coha_thinned_nest_sites,
    tidysdm::thin_by_cell(
      sf::st_transform(coha_nest_sites, crs(elevation_all)),
      elevation_all
    )
  ),
  tar_target(
    coha_psuedoabs,
    sample_pseudoabs(
      coha_thinned_nest_sites,
      elevation_all,
      250,
      method = c("dist_disc", 2500, 10000)
    ) |>
      mutate(id = row_number())
  ),
  tar_target(
    coha_terrain,
    get_terrain_by_region(points = coha_psuedoabs, regions = fs_regions)
  ),
  tar_target(
    coha_canopy_cover,
    get_covariates(tree_canopy_cover_file, coha_psuedoabs, cat = FALSE) |>
      rename(canopy_cover = Layer_1)
  ),
  tar_target(
    coha_surrounding_canopy_cover,
    build_surrounding_canopy_cover(tree_canopy_cover_file, coha_psuedoabs)
  ),
  tar_target(
    coha_evt_lf,
    get_covariates(landfire_evt_file, coha_psuedoabs, cat = "EVT_LF")
  ),
  tar_target(
    coha_evt_gp_n,
    get_covariates(landfire_evt_file, coha_psuedoabs, cat = "EVT_GP_N")
  ),
  tar_target(
    coha_evt_gp_n_grouped,
    get_covariates(
      landfire_evt_file,
      coha_psuedoabs,
      new_level = evt_gp_n_grouped_level
    )
  ),
  tar_target(
    coha_evt_height,
    get_evt_height_covariat(
      "D:\\GIS_Data\\Landfire\\LF2023_EVH_240_CONUS\\LF2023_EVH_240_CONUS\\Tif\\LC23_EVH_240.tif",
      coha_psuedoabs
    )
  ),
  tar_target(
    coha_sclass,
    get_covariates_sclass(
      "D:\\GIS_Data\\Landfire\\LF2023_SClass_240_CONUS\\LF2023_SClass_240_CONUS\\Tif\\LC23_SCla_240.tif",
      coha_psuedoabs
    )
  ),
  tar_target(
    coha_all_covs,
    coha_psuedoabs |>
      left_join(coha_evt_height, by = "id") |>
      left_join(coha_terrain, by = "id") |>
      left_join(coha_canopy_cover, by = "id") |>
      left_join(coha_surrounding_canopy_cover, by = "id") |>
      left_join(coha_evt_gp_n_grouped, by = "id") |>
      left_join(coha_sclass, by = "id")
  ),
  tar_target(
    coha_nest_model,
    build_nogo_nest_model(coha_all_covs)
  ),
  tar_target(
    coha_nest_model_output,
    save_nogo_nest_model(coha_nest_model, "coha_nest_model")
  ),

  ##SSH Nest Model
  tar_target(
    ssh_nest_sites,
    load_and_clean_nogo_nests(
      "data/COHA_SSHA_SITES_ALL_REGIONS.shp",
      epsg,
      year_cutoff = 2018
    ) |>
      st_transform(st_crs(fs_districts)) |>
      st_intersection(fs_districts) |>
      filter(str_detect(sci_name, "striatus"))
  ),
  tar_target(
    ssh_thinned_nest_sites,
    tidysdm::thin_by_cell(
      sf::st_transform(ssh_nest_sites, crs(elevation_all)),
      elevation_all
    )
  ),
  tar_target(
    ssh_psuedoabs,
    sample_pseudoabs(
      ssh_thinned_nest_sites,
      elevation_all,
      250,
      method = c("dist_disc", 2500, 10000)
    ) |>
      mutate(id = row_number())
  ),
  tar_target(
    ssh_terrain,
    get_terrain_by_region(points = ssh_psuedoabs, regions = fs_regions)
  ),
  tar_target(
    ssh_canopy_cover,
    get_covariates(tree_canopy_cover_file, ssh_psuedoabs, cat = FALSE) |>
      rename(canopy_cover = Layer_1)
  ),
  tar_target(
    ssh_surrounding_canopy_cover,
    build_surrounding_canopy_cover(tree_canopy_cover_file, ssh_psuedoabs)
  ),
  tar_target(
    ssh_evt_lf,
    get_covariates(landfire_evt_file, ssh_psuedoabs, cat = "EVT_LF")
  ),
  tar_target(
    ssh_evt_gp_n,
    get_covariates(landfire_evt_file, ssh_psuedoabs, cat = "EVT_GP_N")
  ),
  tar_target(
    ssh_evt_gp_n_grouped,
    get_covariates(
      landfire_evt_file,
      ssh_psuedoabs,
      new_level = evt_gp_n_grouped_level
    )
  ),
  tar_target(
    ssh_evt_height,
    get_evt_height_covariat(
      "D:\\GIS_Data\\Landfire\\LF2023_EVH_240_CONUS\\LF2023_EVH_240_CONUS\\Tif\\LC23_EVH_240.tif",
      ssh_psuedoabs
    )
  ),
  tar_target(
    ssh_sclass,
    get_covariates_sclass(
      "D:\\GIS_Data\\Landfire\\LF2023_SClass_240_CONUS\\LF2023_SClass_240_CONUS\\Tif\\LC23_SCla_240.tif",
      coha_psuedoabs
    )
  ),
  tar_target(
    ssh_all_covs,
    ssh_psuedoabs |>
      left_join(ssh_evt_height, by = "id") |>
      left_join(ssh_terrain, by = "id") |>
      left_join(ssh_canopy_cover, by = "id") |>
      left_join(ssh_surrounding_canopy_cover, by = "id") |>
      left_join(ssh_evt_gp_n_grouped, by = "id") |>
      left_join(ssh_sclass, by = "id")
  ),
  tar_target(
    ssh_nest_model,
    build_nogo_nest_model(ssh_all_covs)
  ),
  tar_target(
    ssh_nest_model_output,
    save_nogo_nest_model(ssh_nest_model, "coha_nest_model")
  ),

  ## I probably need to delete everything after this.

  tar_terra_rast(
    crd_covs,
    get_covs_for_predict(
      area = crd_bd,
      evt_gps = evt_gp_n_grouped_level,
      proj = epsg
    ),
    datatype = "INT4S"
  ),
  # tar_terra_rast(
  #   crd_prediction,
  #   predict_raster(nogo_nest_model, crd_covs)
  # ),

  tar_terra_rast(
    test_covs,
    get_covs_for_predict(
      area = test_boundary,
      evt_gp_n_grouped_level,
      proj = epsg
    )
  ),
  # tar_terra_rast(
  tar_target(
    carson_model,
    get_rd_prediction(
      model_path = "output/model_V5.rds",
      rds = r3_boundaries,
      rd_id = r3_testing_rd_id[[1]],
      terrain_model = terrain,
      canopy_cover = tar_load_clip_tree_canopy_cover,
      evt_gp_n_new_level = evt_gp_n_grouped_level
    )
  )
)
