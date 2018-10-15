#' Automatically format column values
#'
#' @param x A vector of values to format automatically
#'
#' @return A formatted vector
#' @noRd
format_value_auto <- function(x){
  case_when(
    is.na(x) | x %in% c(Inf, -Inf) | is.nan(x) ~ ".",
    x < 10 ~ format(round(x, 2), trim = TRUE, nsmall = 2),
    x < 100 ~ format(round(x, 1), trim = TRUE, nsmall = 1),
    TRUE ~ format(round(x, 0), trim = TRUE, nsmall = 0)
  )
}
