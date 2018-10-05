#' Compute geometric mean, standard deviation, and variance
#' 
#' Functions modeled from qwraps package: \url{https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html}.
#' 
#' @param x A vector of values to format automatically
#'
#' @return A formatted vector
#' @noRd
#'
# add functions
gmean <- function(x, na_rm = TRUE) {
  if (na_rm) {
    x <- stats::na.omit(x)
  }
  if (sum(x<=0)>0){
    warning("Cannot compute geometric mean on values less than or equal to zero.")
    return(NA)
  } 
  return(exp(mean(log(x))))
}

gsd <- function(x, na_rm = TRUE) {
  if (na_rm) {
    x <- stats::na.omit(x)
  }
  if (sum(x<=0)>0){
    warning("Cannot compute geometric standard deviation on values less than or equal to zero.")
    return(NA)
  } 
  return(exp(sqrt((length(x) - 1) / length(x) * stats::var(log(x)))))
}

gvar <- function(x, na_rm = TRUE) {
  if (na_rm) {
    x <- stats::na.omit(x)
  }
  if (sum(x<=0)>0){
    warning("Cannot compute geometric variance on values less than or equal to zero.")
    return(NA)
  } 
  return(exp((length(x) - 1) / length(x) * stats::var(log(x))))
}



