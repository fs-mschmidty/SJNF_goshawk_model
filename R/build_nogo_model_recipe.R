build_nogo_model_recipe <- function(all_covs) {
  all_covs |>
    spatial_recipe(
      formula = class ~ .
    ) |>
    step_mutate(
      ForTypName = case_when(
        str_detect(ForTypName, "(?i)oak") ~ "Oak Forest",
        str_detect(ForTypName, "Engelmann") ~
          "Engelmann spruce and or subalpine fir",
        str_detect(ForTypName, "(?i)juniper|(?i)pinyon") ~
          "Pinyon Juniper Forest",
        TRUE ~ ForTypName
      ),
      .pkgs = c("dplyr", "stringr")
    ) |>
    step_mutate(across(all_predictors(), as.double)) |>
    # Ensure categorical type (if ForTypName is still character)
    step_string2factor(ForTypName) |> # optional, if not already factor
    # 2) Lump infrequent levels to "Other Forest" by count
    step_other(
      ForTypName,
      threshold = 15,
      other = "Other Forest"
    ) |>
    # 3) Replace missing category with "Not Forest"
    step_unknown(ForTypName, new_level = "Not Forest") |>
    step_normalize(all_numeric_predictors()) |>
    step_novel(all_nominal_predictors()) |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors())
}
