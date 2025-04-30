save_nogo_nest_model <- function(x) {
  path <- file.path(
    "output",
    paste0("nogo_nest_model_", format(Sys.time(), "%Y%m%d%H%M%S"), ".rds")
  )
  saveRDS(x, path)
  path
}
