evh_to_num <- function(evh) {
  c <- cats(evh)[[1]] |>
    mutate(
      meters_h = str_extract(CLASSNAMES, "\\d+"),
      meters_h = ifelse(is.na(meters_h), 0, meters_h) |>
        as.numeric()
    ) |>
    select(Value, meters_h)

  levels(evh) <- list(c)
  activeCat(evh) <- "meters_h"

  catalyze(evh)
}
