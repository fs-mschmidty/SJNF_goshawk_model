# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(sjnftools)
library(geotargets)

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
    "tidymodels",
    "rgee",
    "googledrive"
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
      filter(!region %in% c("10", "03", "08", "09")) |>
      st_make_valid()
  ),
  tar_target(
    fs_districts,
    arc_open(
      "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_RangerDistricts_03/MapServer/1"
    ) |>
      arc_select() |>
      st_make_valid() |>
      filter(!region %in% c("10", "03", "08", "09")) |>
      st_transform(st_crs(fs_regions))
  ),
  ## Load Tree Map
  tar_target(
    tree_map_path,
    "D:\\GIS_Data\\TreeMap\\RDS-2025-0032\\Data\\TreeMap2022_CONUS.tif"
  ),
  tar_target(
    nogo_nest_sites,
    load_and_clean_nogo_nests(
      "data/All_region_amgo.shp",
      epsg,
      year_cutoff = 2018
    ) |>
      st_transform(st_crs(fs_districts)) |>
      st_intersection(fs_districts) |>
      st_intersection(filter(fs_regions, region == "02"))
  ),
  tar_terra_rast(
    aggregate_treemap,
    build_ag_treemap(tree_map_path, nogo_nest_sites, fact = 4)
  ),
  tar_target(
    thinned_nest_sites,
    build_thinned_nest_sites(
      nogo_nest_sites = nogo_nest_sites,
      r_path = aggregate_treemap
    )
  ),
  tar_target(
    psuedoabs,
    sample_pseudoabs(
      thinned_nest_sites,
      aggregate_treemap,
      nrow(thinned_nest_sites) * 3,
      method = c("dist_disc", 300, 10000)
    ) |>
      mutate(id = row_number())
  ),
  tar_target(
    terrain,
    get_gee_terrain(
      geom_input = psuedoabs
    )
  ),
  # tar_target(
  #   treemap,
  #   get_gee_treemap(
  #     geom_input = psuedoabs
  #   )
  # ),
  tar_target(
    canopy_height,
    get_gee_canopy_height(psuedoabs)
  ),
  tar_target(
    canopy_cover,
    get_gee_canopy_cover(
      geom_input = psuedoabs
    )
  ),
  tar_target(
    forest_type_raw,
    get_tree_map_attribute(tree_map_path, "ForTypName", psuedoabs)
  ),
  tar_target(
    climate,
    get_gee_climate(psuedoabs)
  ),
  tar_target(
    soil,
    get_gee_ph(psuedoabs)
  ),
  tar_target(
    distance_to_water,
    get_gee_distance_to_water(geom_input = psuedoabs) |>
      rename(DISTANCE_TO_WATER = first)
  ),
  # All coves
  tar_target(
    all_covs,
    psuedoabs |>
      left_join(terrain, by = "id") |>
      left_join(forest_type_raw, by = "id") |>
      left_join(canopy_height, by = "id") |>
      left_join(canopy_cover, by = "id") |>
      left_join(distance_to_water, by = "id") |>
      # left_join(treemap, by = "id") |>
      left_join(mutate(climate, id = as.numeric(id)), by = "id") |>
      left_join(soil, by = "id") |>
      drop_na() |>
      select(-id)
  ),
  ## Build Model
  tar_target(
    nogo_model_recipe,
    build_nogo_model_recipe(all_covs)
  ),
  tar_target(
    nogo_nest_model,
    build_nogo_nest_model(all_covs, nogo_model_recipe)
  ),
  tar_target(
    nogo_nest_model_output,
    save_nogo_nest_model(nogo_nest_model)
  ),
  tar_terra_rast(
    treemap_for_type_crd,
    rast(tree_map_path) |>
      crop(st_transform(crd_bd, 5070), mask = T),
    preserve_metadata = "zip"
  ),
  tar_terra_rast(
    treemap_crd,
    get_gee_treemap(
      geom_input = treemap_for_type_crd
    ),
    preserve_metadata = "zip"
  ),
  tar_terra_rast(
    canopy_height_crd,
    get_gee_canopy_height(
      geom_input = treemap_for_type_crd
    )
  ),
  tar_terra_rast(
    terrain_crd,
    get_gee_terrain(
      geom_input = treemap_for_type_crd
    ),
    preserve_metadata = "zip"
  ),
  tar_terra_rast(
    climate_crd,
    get_gee_climate(
      geom_input = treemap_for_type_crd
    ),
    preserve_metadata = "zip"
  ),
  ## Soils Data
  tar_terra_rast(
    soil_crd,
    get_gee_ph(
      geom_input = treemap_for_type_crd
    )
  ),
  tar_terra_rast(
    canopy_cover_crd,
    get_gee_canopy_cover(
      geom_input = treemap_for_type_crd
    )
  ),
  tar_terra_rast(
    distance_to_water_crd,
    get_gee_distance_to_water(
      geom_input = treemap_for_type_crd
    )
  ),
  tar_terra_rast(
    crd_covs,
    make_predict_covs(
      c(
        treemap_crd,
        treemap_for_type_crd,
        canopy_height_crd,
        canopy_cover_crd,
        distance_to_water_crd,
        terrain_crd,
        climate_crd,
        soil_crd
      )
    ),
    preserve_metadata = "zip"
  ),
  tar_terra_rast(
    crd_predict,
    predict_raster(
      nogo_nest_model,
      crd_covs,
      typ = "prob"
    )
  ),
  tar_target(
    pagosa_rd,
    fs_districts |>
      filter(districtorgcode == "021306") |>
      st_make_valid() |>
      st_union() |>
      st_as_sf()
  ),
  tar_target(
    dolores_rd,
    fs_districts |>
      filter(districtorgcode == "021305") |>
      st_make_valid() |>
      st_union() |>
      st_as_sf()
  ),
  tar_target(
    columbine_rd,
    fs_districts |>
      filter(districtorgcode == "021308") |>
      st_make_valid() |>
      st_union() |>
      st_as_sf()
  ),
  tar_terra_rast(
    pag_predict,
    predict_rd(
      geom_input = pagosa_rd,
      model = nogo_nest_model,
      tree_map_path = tree_map_path
    )
  ),
  tar_terra_rast(
    dol_predict,
    predict_rd(
      geom_input = dolores_rd,
      model = nogo_nest_model,
      tree_map_path = tree_map_path
    )
  ),
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
    build_thinned_nest_sites(
      nogo_nest_sites = coha_nest_sites,
      r_path = aggregate_treemap
    )
  ),
  tar_target(
    coha_psuedoabs,
    sample_pseudoabs(
      coha_thinned_nest_sites,
      aggregate_treemap,
      nrow(coha_thinned_nest_sites) * 3,
      method = c("dist_disc", 300, 10000)
    ) |>
      mutate(id = row_number())
  ),
  tar_target(
    coha_covs,
    build_covs_all(input_geom = coha_psuedoabs, tree_map_path = tree_map_path)
  ),
  tar_target(
    coha_model_recipe,
    build_nogo_model_recipe(coha_covs)
  ),
  tar_target(
    coha_nest_model,
    build_nogo_nest_model(coha_covs, coha_model_recipe)
  ),
  tar_terra_rast(
    col_coha_predict,
    predict_rd(
      geom_input = columbine_rd,
      model = coha_nest_model,
      tree_map_path = tree_map_path
    )
  )
)
