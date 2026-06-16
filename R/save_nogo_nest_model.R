save_nogo_nest_model <- function(x, name_prefix = "nogo_nest_model") {
  path <- file.path(
    "output",
    paste0(name_prefix, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".rds")
  )
  saveRDS(x, path)
  path
}
