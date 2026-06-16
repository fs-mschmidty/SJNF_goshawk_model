build_ag_treemap <- function(tree_map_path, nogo_nest_sites, fact) {
  r <- rast(tree_map_path)
  activeCat(r) <- "QMD"
  cl_nogo_nest_sites <- nogo_nest_sites |>
    st_transform(crs(r))

  r |>
    crop(cl_nogo_nest_sites) |>
    aggregate(fact = fact)
}

