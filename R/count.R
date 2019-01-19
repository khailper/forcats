#' Count entries in a factor
#'
#' @param f A factor (or character vector).
#' @param sort If `TRUE`, sort the result so that the most common values
#'   float to the top.
#' @return A tibble with columns `f` and `n`.
#' @export
#' @examples
#' f <- factor(sample(letters)[rpois(1000, 10)])
#' table(f)
#' fct_count(f)
#' fct_count(f, sort = TRUE)
fct_count <- function(f, sort = FALSE, prop = FALSE) {
  f <- check_factor(f)
  f2 <- addNA(f, ifany = TRUE)

  df <- tibble::tibble(
    f = fct_unique(f2),
    n = as.integer(table(f2))
  )

  if (sort && !prop) {
    df <- df[order(df$n, decreasing = TRUE), ]
  }

  if (prop){
    df$prop <- df$n/sum(df$n)
    df <- df[c("f", "prop")]

    if (sort){
      df <- df[order(df$prop, decreasing = TRUE), ]
    }

  }

  df
}
