build_climate_ee_spec <- function() {
  months_list <- rgee::ee$List(list(
    "01",
    "02",
    "03",
    "04",
    "05",
    "06",
    "07",
    "08",
    "09",
    "10",
    "11",
    "12"
  ))

  # Helper to build a reducer and return the single-band result

  reduce_band <- function(band_name, reducer_fn) {
    rgee::ee$ImageCollection("OREGONSTATE/PRISM/Norm91m")$select(
      band_name
    )$filter(rgee::ee$Filter$inList("system:index", months_list))$reduce(
      reducer_fn
    )
  }

  # Compute each stat
  tmax_max <- reduce_band("tmax", rgee::ee$Reducer$max())$rename("tmax_max")
  tmin_min <- reduce_band("tmin", rgee::ee$Reducer$min())$rename("tmin_min")
  tmean_mean <- reduce_band("tmean", rgee::ee$Reducer$mean())$rename(
    "tmean_mean"
  )
  ppt_sum <- reduce_band("ppt", rgee::ee$Reducer$sum())$rename("ppt_sum")

  # Combine them into one stacked image
  annual_img <- tmax_max$addBands(tmin_min)$addBands(tmean_mean)$addBands(
    ppt_sum
  )

  return(annual_img)
}

