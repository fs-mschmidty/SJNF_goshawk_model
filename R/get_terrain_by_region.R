get_terrain_by_region <- function(points, regions) {
  region_nums <- regions |>
    pull(REGION)

  get_cov_for_region <- function(region_num, all_points) {
    current_region <- regions |>
      filter(REGION == region_num) |>
      st_union()

    current_reg_points <- all_points |>
      st_intersection(current_region)

    reg_elev <- get_elevation_data(st_buffer(current_reg_points, 2000))

    get_covariates(reg_elev, current_reg_points)
  }
  lapply(region_nums, get_cov_for_region, points) |>
    bind_rows()
}
# source("R/elevation_data.R")
# region_nums <- fs_regions |>
#   pull(REGION)
#
# current_region <- fs_regions |>
#   filter(REGION == region_nums[2]) |>
#   st_union()
#
# current_reg_points <- psuedoabs |>
#   st_intersection(current_region)
#
#
# reg_elev <- get_elevation_data(st_buffer(current_reg_points, 2000))
#
# get_covariates(reg_elev, current_reg_points)
