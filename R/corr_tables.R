#' Lower-triangle correlation table (aligned decimals with significance marks)
#'
#' Create a lower-triangle correlation table for a set of variables.
#' Positive coefficients are padded with a leading space so decimal points align.
#' Significance is indicated as * (p < .05), ** (p < .01), *** (p < .001).
#'
#' @param df A data.frame containing the variables.
#' @param vars Character vector of variable names.
#' @return A data.frame with row/column names and lower-triangle cells filled.
#' @export
lower_triangle_corr_table <- function(df, vars) {
  vars <- vars[vars %in% names(df)]
  if (length(vars) < 2) {
    return(NULL)
  }
  mat <- matrix(" ", nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))
  for (i in seq_along(vars)) {
    for (j in seq_along(vars)) {
      if (i > j) {
        x <- df[[vars[i]]]
        y <- df[[vars[j]]]
        valid <- stats::complete.cases(x, y)
        if (sum(valid) > 2) {
          test <- stats::cor.test(x[valid], y[valid])
          r <- unname(test$estimate)
          p <- test$p.value
          sig <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
          mat[i, j] <- sprintf("% .2f%s", r, sig)
        }
      }
    }
  }
  as.data.frame(mat)
}

#' Correlation table with stars (row x column)
#'
#' Create a correlation table for specified rows and columns with
#' significance marks. Positive coefficients are padded with a leading space
#' so decimal points align.
#'
#' @param df A data.frame containing the variables.
#' @param rows Character vector of row variable names.
#' @param cols Character vector of column variable names.
#' @return A data.frame with row names and formatted correlation cells.
#' @export
nice_correlation <- function(df, rows, cols) {
  rows <- rows[rows %in% names(df)]
  cols <- cols[cols %in% names(df)]
  if (length(rows) == 0 || length(cols) == 0) {
    return(NULL)
  }

  mat <- matrix(" ", nrow = length(rows), ncol = length(cols), dimnames = list(rows, cols))
  for (i in seq_along(rows)) {
    for (j in seq_along(cols)) {
      x <- df[[rows[i]]]
      y <- df[[cols[j]]]
      valid <- stats::complete.cases(x, y)
      if (sum(valid) > 2) {
        test <- stats::cor.test(x[valid], y[valid])
        r <- unname(test$estimate)
        p <- test$p.value
        sig <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
        mat[i, j] <- sprintf("% .2f%s", r, sig)
      } else {
        mat[i, j] <- " "
      }
    }
  }
  as.data.frame(mat)
}
