load_and_clean_nogo_nests <- function(path, epsg) {
  data <- st_read(path) |>
    clean_names() |>
    filter(
      source_geo == "Point",
      site_type == "Nest",
      year(last_vis_1) >= 2018
    ) |>
    st_centroid() |>
    st_transform(epsg) |>
    filter(shape_stat == "CURRENT")

  data
}
