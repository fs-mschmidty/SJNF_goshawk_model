build_evt_gp_n_grouped_level <- function(x, psuedoabs) {
  r <- rast(x)

  new_level <- cats(r)[[1]] |>
    as_tibble() |>
    mutate(
      EVT_GP_N = case_when(
        EVT_LF != "Tree" ~ EVT_LF,
        str_detect(EVT_GP_N, "Juniper|Pinyon") ~ "Pinyon-Juniper Woodland",
        TRUE ~ EVT_GP_N
      )
    ) |>
    select(Value, EVT_GP_N, EVT_LF)

  levels(r) <- list(new_level)

  r

  points <- psuedoabs |>
    st_transform(crs(r))

  t <- points |>
    bind_cols(terra::extract(r, points)) |>
    select(-ID)

  activeCat(r) <- "EVT_LF"

  t <- t |>
    bind_cols(terra::extract(r, points))

  new_group <- t |>
    as_tibble() |>
    group_by(EVT_GP_N) |>
    mutate(n = n()) |>
    ungroup() |>
    mutate(
      evt_gp_n_grouped = case_when(
        EVT_LF == "Tree" & n < 15 ~ EVT_LF,
        TRUE ~ EVT_GP_N
      )
    ) |>
    # mutate(evt_gp_n_grouped = fct_lump_min(evt_gp_n_grouped, min = 30, other_level = "Other")) |>
    count(EVT_GP_N, evt_gp_n_grouped) |>
    select(-n)

  count(new_group, evt_gp_n_grouped)

  evt_gp_n_grouped_level <- new_level |>
    left_join(new_group, by = "EVT_GP_N") |>
    mutate(
      evt_gp_n_grouped = case_when(
        is.na(evt_gp_n_grouped) ~ EVT_LF,
        TRUE ~ evt_gp_n_grouped
      ),
      evt_gp_n_grouped = ifelse(
        evt_gp_n_grouped == "Tree",
        "Other Tree",
        evt_gp_n_grouped
      )
    ) |>
    select(Value, evt_gp_n_grouped)

  evt_gp_n_grouped_level
}
