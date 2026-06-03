#' Count card rarity by grouping variable
#'
#' @param grouping_var A column to group by (unquoted).
#' @param dat Optional pre-loaded data. If NULL, loads data internally.
#' @return A tibble with counts of each rarity by group.
#' @importFrom dplyr count mutate
#' @importFrom tidyr pivot_wider
#' @export
rarity_by_release <- function(grouping_var, dat = NULL) {
  if (is.null(dat)) dat <- load_data()
  dat |>
    count({{ grouping_var }}, rarity) |>
    mutate(pct = n / sum(n), .by = {{ grouping_var }}) |>
    pivot_wider(
      names_from  = rarity,
      values_from = pct,
      values_fill = 0
    )
}
