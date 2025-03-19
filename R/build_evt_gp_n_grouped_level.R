build_evt_gp_n_grouped_level <- function(x, psuedoabs) {
  r <- rast(x)

  new_level <- cats(r)[[1]] |>
    as_tibble() |>
    mutate(
      EVT_GP_N = case_when(
        EVT_LF != "Tree" ~ "Non-tree",
        TRUE ~ EVT_GP_N
      )
    ) |>
    select(Value, EVT_GP_N)

  levels(r) <- list(new_level)

  n <- psuedoabs |>
    st_transform(crs(r))

  activeCat(r) <- "EVT_GP_N"

  evt_gp_n <- terra::extract(r, n) |>
    as_tibble()

  new_evt_tree_cl <- n |>
    as_tibble() |>
    bind_cols(evt_gp_n) |>
    mutate(
      evt_gp_n_grouped = case_when(
        EVT_GP_N %in%
          c(
            "Western Riparian Woodland and Shrubland",
            "Pinyon-Juniper Woodland",
            "Limber Pine Woodland",
            "Douglas-fir Forest and Woodland",
            "Developed-Upland Evergreen Forest",
            "Mountain Mahogany Woodland and Shrubland"
          ) ~
          "Other Forest",
        TRUE ~ EVT_GP_N
      )
    ) |>
    select(EVT_GP_N, evt_gp_n_grouped) |>
    count(EVT_GP_N, evt_gp_n_grouped)

  r2 <- rast(x)

  cats(r2)[[1]] |>
    as_tibble() |>
    mutate(
      EVT_GP_N = case_when(
        EVT_LF != "Tree" ~ "Non-tree",
        TRUE ~ EVT_GP_N
      )
    ) |>
    left_join(new_evt_tree_cl, by = "EVT_GP_N") |>
    mutate(
      evt_gp_n_grouped = case_when(
        is.na(evt_gp_n_grouped) ~ "Other Forest",
        TRUE ~ evt_gp_n_grouped
      )
    ) |>
    select(Value, evt_gp_n_grouped)
}
