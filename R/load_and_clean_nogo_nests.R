load_and_clean_nogo_nests <- function(path, epsg, year_cutoff) {
  data <- st_read(path) |>
    clean_names() |>
    filter(
      source_geo == "Point",
      site_type == "Nest",
      year(last_vis_1) >= year_cutoff
    ) |>
    st_centroid() |>
    st_transform(epsg) |>
    filter(shape_stat == "CURRENT")

  data
}
