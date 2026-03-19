#' Compute z-scores
#'
#' Standardizes a numeric vector into z-scores.
#'
#' @param x A numeric vector.
#' @param na_rm Logical. Should missing values be removed before calculation?
#'
#' @return A numeric vector of z-scores.
#' @export
#'
#' @examples
#' z_score(c(1, 2, 3, 4, 5))
z_score <- function(x, na_rm = FALSE) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.")
  }

  if (na_rm) {
    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
  } else {
    m <- mean(x)
    s <- stats::sd(x)
  }

  (x - m) / s
}
