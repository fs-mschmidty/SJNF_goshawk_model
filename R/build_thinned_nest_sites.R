build_thinned_nest_sites <- function(nogo_nest_sites, r_path) {
  if (is.character(r_path)) {
    rast_load <- rast(r_path)
  } else {
    rast_load <- r_path
  }

  nogo_nest_sites_cl <- nogo_nest_sites |>
    st_transform(crs(rast_load))

  rast_load_crop <- rast_load |>
    crop(nogo_nest_sites_cl)

  tidysdm::thin_by_cell(
    nogo_nest_sites_cl,
    rast_load_crop,
    drop_na = TRUE
  )
}
