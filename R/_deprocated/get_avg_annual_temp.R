get_avg_annual_temp <- function(points, years = 1985:2020, scale = 4000) {
  ee_Initialize(
    email = "mschmidty@gmail.com",
    project = "learn-py-gee-mschmidty",
    drive = "FALSE",
    gcs = "FALSE"
  )
  tc <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")

  annual_temp_img <- function(year) {
    imgs <- tc$filter(ee$Filter$calendarRange(year, year, "year"))$select(c(
      "tmmx",
      "tmmn"
    ))$map(function(img) {
      tmean <- img$select("tmmx")$add(img$select("tmmn"))$divide(2)
    })

    imgs$reduce(ee$Reducer$mean())$rename(paste0("tmean_", year))
  }
  pts_ee <- sf_as_ee(points)

  # Store annual values
  annual_vals <- list()

  for (yr in years) {
    img <- annual_temp_img(yr)

    df <- rgee::ee_extract(
      x = img,
      y = pts_ee,
      fun = rgee::ee$Reducer$first(),
      scale = scale,
      sf = FALSE
    )

    annual_vals[[as.character(yr)]] <- df[[1]] # the single column of temps
  }

  # Long-term average per point
  avg_temp <- rowMeans(as.data.frame(annual_vals), na.rm = TRUE)

  # Return sf with new variable
  cbind(points, avg_annual_temp = avg_temp)
}
