#' Compute mean and standard error
#'
#' Calculates the sample mean and standard error for a numeric vector.
#'
#' @param x A numeric vector.
#' @param na_rm Logical. Should missing values be removed?
#'
#' @return A named vector with mean and standard error.
#' @export
#'
#' @examples
#' mean_se(c(1, 2, 3, 4, 5))
mean_se <- function(x, na_rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.")
  }

  if (na_rm) {
    x <- x[!is.na(x)]
  }

  n <- length(x)

  if (n == 0) {
    stop("`x` has no valid observations.")
  }

  m <- mean(x)
  se <- stats::sd(x) / sqrt(n)

  c(mean = m, se = se)
}
