build_covs_all <- function(
  input_geom,
  tree_map_path
) {
  terrain <- get_gee_terrain(
    geom_input = input_geom
  )
  treemap <- get_gee_treemap(
    geom_input = input_geom
  )

  canopy_height <- get_gee_canopy_height(input_geom)

  canopy_cover <- get_gee_canopy_cover(
    geom_input = input_geom
  )

  forest_type_raw <- get_tree_map_attribute(
    tree_map_path,
    "ForTypName",
    input_geom
  )

  climate <- get_gee_climate(input_geom)

  soil <- get_gee_ph(input_geom)

  distance_to_water <- get_gee_distance_to_water(geom_input = input_geom) |>
    rename(DISTANCE_TO_WATER = first)

  # All coves
  all_covs <- input_geom |>
    left_join(terrain, by = "id") |>
    left_join(forest_type_raw, by = "id") |>
    left_join(canopy_height, by = "id") |>
    left_join(canopy_cover, by = "id") |>
    left_join(distance_to_water, by = "id") |>
    left_join(treemap, by = "id") |>
    left_join(mutate(climate, id = as.numeric(id)), by = "id") |>
    left_join(soil, by = "id") |>
    drop_na() |>
    select(-id)

  return(all_covs)
}

