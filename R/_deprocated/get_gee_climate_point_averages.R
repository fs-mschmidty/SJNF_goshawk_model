get_gee_climate_point_averages <- function(
  points_sf,
  years = 1985:2020,
  scale = 4000
) {
  init_ee()

  # Convert to EE FeatureCollection
  pts_ee <- sf_as_ee(points_sf)

  # Helper to extract per-point values for a single EE Image
  extract_vals <- function(img, bandname) {
    df <- rgee::ee_extract(
      x = img,
      y = points_sf,
      fun = rgee::ee$Reducer$first(),
      scale = scale,
      sf = FALSE
    )
    df[[bandname]]
  }

  # Empty lists to store annual values
  L_snow <- list()
  L_annual_temp <- list()
  L_min_winter <- list()
  L_max_summer <- list()
  L_mean_summer <- list()
  L_mean_spring <- list()
  L_max_spring <- list()
  L_annual_precip <- list()

  # Loop through all years
  for (yr in years) {
    snow_img <- gee_snow_amount(yr)
    annual_temp_img <- gee_avg_annual_temp(yr)
    min_winter_img <- gee_min_winter_temp(yr)
    max_summer_img <- gee_max_summer_temp(yr)
    mean_summer_img <- gee_mean_summer_temp(yr)
    mean_spring_img <- gee_mean_spring_temp(yr)
    max_spring_img <- gee_max_spring_temp(yr)
    annual_precip_img <- gee_annual_precip(yr)

    L_snow[[as.character(yr)]] <- extract_vals(snow_img, paste0("snow_", yr))
    L_annual_temp[[as.character(yr)]] <- extract_vals(
      annual_temp_img,
      paste0("temp_", yr)
    )
    L_min_winter[[as.character(yr)]] <- extract_vals(
      min_winter_img,
      paste0("min_win_", yr)
    )
    L_max_summer[[as.character(yr)]] <- extract_vals(
      max_summer_img,
      paste0("max_sum_", yr)
    )
    L_mean_summer[[as.character(yr)]] <- extract_vals(
      mean_summer_img,
      paste0("mean_sum_", yr)
    )
    L_mean_spring[[as.character(yr)]] <- extract_vals(
      mean_spring_img,
      paste0("mean_spr_", yr)
    )
    L_max_spring[[as.character(yr)]] <- extract_vals(
      max_spring_img,
      paste0("max_spr_", yr)
    )
    L_annual_precip[[as.character(yr)]] <- extract_vals(
      annual_precip_img,
      paste0("precip_", yr)
    )
  }

  # Compute long-term averages per point
  res <- data.frame(
    snow = rowMeans(as.data.frame(L_snow), na.rm = TRUE),
    avg_annual_temp = rowMeans(as.data.frame(L_annual_temp), na.rm = TRUE),
    min_winter_temp = rowMeans(as.data.frame(L_min_winter), na.rm = TRUE),
    max_summer_temp = rowMeans(as.data.frame(L_max_summer), na.rm = TRUE),
    mean_summer_temp = rowMeans(as.data.frame(L_mean_summer), na.rm = TRUE),
    mean_spring_temp = rowMeans(as.data.frame(L_mean_spring), na.rm = TRUE),
    max_spring_temp = rowMeans(as.data.frame(L_max_spring), na.rm = TRUE),
    annual_precip = rowMeans(as.data.frame(L_annual_precip), na.rm = TRUE)
  )

  # Return a combined sf with all new variables
  cbind(points_sf, res)
}
