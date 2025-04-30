sclass_clean <- function(x) {
  new_cats <- cats(x)[[1]] |>
    mutate(
      sclass = ifelse(
        str_detect(DESCRIPTIO, "^Succession"),
        DESCRIPTIO,
        "Non-Vegetation"
      )
    ) |>
    select(Value, sclass)

  sclass <- x

  levels(sclass) <- list(new_cats)

  sclass
}
