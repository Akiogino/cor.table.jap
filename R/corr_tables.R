#' Lower-triangle correlation table (aligned decimals with significance marks)
#'
#' Create a lower-triangle correlation table for a set of variables.
#' Positive coefficients are padded with a leading space so decimal points align.
#' Significance is indicated as * (p < .05), ** (p < .01), *** (p < .001).
#'
#' @param df A data.frame containing the variables.
#' @param vars Character vector of variable names.
#' @param quote_text Logical. If TRUE, replaces the leading space in positive
#'   values with a non-breaking space to preserve alignment in spreadsheets.
#' @return A data.frame with row/column names and lower-triangle cells filled.
#' @export
lower_triangle_corr_table <- function(df, vars, quote_text = FALSE) {
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
          cell <- sprintf("% .2f%s", r, sig)
          if (quote_text) {
            cell <- sub("^ ", "\u00A0", cell)
          }
          mat[i, j] <- cell
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
#' @param quote_text Logical. If TRUE, replaces the leading space in positive
#'   values with a non-breaking space to preserve alignment in spreadsheets.
#' @return A data.frame with row names and formatted correlation cells.
#' @export
nice_correlation <- function(df, rows, cols, quote_text = FALSE) {
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
        cell <- sprintf("% .2f%s", r, sig)
        if (quote_text) {
          cell <- sub("^ ", "\u00A0", cell)
        }
        mat[i, j] <- cell
      } else {
        mat[i, j] <- " "
      }
    }
  }
  as.data.frame(mat)
}

#' Lower-triangle correlation outputs (r, p, stars)
#'
#' Create numeric r and p matrices plus a display matrix with significance stars.
#'
#' @param df A data.frame containing the variables.
#' @param vars Character vector of variable names.
#' @param quote_text Logical. If TRUE, replaces the leading space in positive
#'   values with a non-breaking space to preserve alignment in spreadsheets.
#' @return A list with data.frames: r, p, stars.
#' @export
lower_triangle_corr_outputs <- function(df, vars, quote_text = TRUE) {
  vars <- vars[vars %in% names(df)]
  if (length(vars) < 2) {
    return(NULL)
  }
  r_mat <- matrix(NA_real_, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))
  p_mat <- matrix(NA_real_, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))
  s_mat <- matrix(" ", nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))
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
          r_mat[i, j] <- r
          p_mat[i, j] <- p
          sig <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
          s_mat[i, j] <- sig
        }
      }
    }
  }
  list(r = as.data.frame(r_mat), p = as.data.frame(p_mat), stars = as.data.frame(s_mat))
}

#' Write lower-triangle correlation tables to CSV (r, p, stars)
#'
#' @param df A data.frame containing the variables.
#' @param vars Character vector of variable names.
#' @param file_base File path without extension (suffixes are added).
#' @param quote_text Logical. If TRUE, replaces the leading space in positive
#'   values with a non-breaking space to preserve alignment in spreadsheets.
#' @param rowname_label Label for the first column (row names).
#' @param file_encoding File encoding (default UTF-8).
#' @return A list with data.frames: r, p, stars (invisibly).
#' @export
write_lower_triangle_corr_tables <- function(df, vars, file_base, quote_text = TRUE,
                                             rowname_label = "項目名", file_encoding = "UTF-8",
                                             sig_colname = "") {
  out <- lower_triangle_corr_outputs(df, vars, quote_text = quote_text)
  if (is.null(out)) {
    return(NULL)
  }
  add_rowname <- function(d) {
    cbind(setNames(data.frame(rownames(d)), rowname_label), d)
  }
  utils::write.csv(add_rowname(out$r), paste0(file_base, "_r.csv"), row.names = FALSE, fileEncoding = file_encoding)
  utils::write.csv(add_rowname(out$p), paste0(file_base, "_p.csv"), row.names = FALSE, fileEncoding = file_encoding)
  utils::write.csv(add_rowname(out$stars), paste0(file_base, "_stars.csv"), row.names = FALSE, fileEncoding = file_encoding)
  interleave_cols <- function(r_df, sig_df, sig_name = "") {
    cols <- colnames(r_df)
    out_list <- vector("list", length(cols) * 2)
    out_names <- as.vector(rbind(cols, rep(sig_name, length(cols))))
    for (i in seq_along(cols)) {
      out_list[[2 * i - 1]] <- r_df[[cols[i]]]
      out_list[[2 * i]] <- sig_df[[cols[i]]]
    }
    out_df <- as.data.frame(out_list, check.names = FALSE)
    rownames(out_df) <- rownames(r_df)
    colnames(out_df) <- out_names
    out_df
  }
  r_sig <- interleave_cols(out$r, out$stars, sig_colname)
  r_sig_out <- add_rowname(r_sig)
  header <- c(rowname_label, as.vector(rbind(colnames(out$r), rep(sig_colname, length(colnames(out$r))))))
  utils::write.table(
    r_sig_out,
    paste0(file_base, "_r_sig.csv"),
    sep = ",",
    row.names = FALSE,
    col.names = header,
    quote = TRUE,
    fileEncoding = file_encoding
  )
  invisible(out)
}

#' Write lower-triangle correlation tables to Excel (r, p, stars)
#'
#' @param df A data.frame containing the variables.
#' @param vars Character vector of variable names.
#' @param file_out Output .xlsx file path.
#' @param quote_text Logical. If TRUE, replaces the leading space in positive
#'   values with a non-breaking space to preserve alignment in spreadsheets.
#' @param rowname_label Label for the first column (row names).
#' @param font_name Font name to apply in the Excel output (default: Times New Roman).
#' @return A list with data.frames: r, p, stars (invisibly).
#' @export
write_lower_triangle_corr_xlsx <- function(df, vars, file_out, quote_text = TRUE,
                                           rowname_label = "項目名", font_name = "Times New Roman",
                                           sig_colname = "") {
  out <- lower_triangle_corr_outputs(df, vars, quote_text = quote_text)
  if (is.null(out)) {
    return(NULL)
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for Excel output. Please install it.")
  }
  add_rowname <- function(d) {
    cbind(setNames(data.frame(rownames(d)), rowname_label), d)
  }
  interleave_cols <- function(r_df, sig_df, sig_name = "") {
    cols <- colnames(r_df)
    out_list <- vector("list", length(cols) * 2)
    out_names <- as.vector(rbind(cols, rep(sig_name, length(cols))))
    for (i in seq_along(cols)) {
      out_list[[2 * i - 1]] <- r_df[[cols[i]]]
      out_list[[2 * i]] <- sig_df[[cols[i]]]
    }
    out_df <- as.data.frame(out_list, check.names = FALSE)
    rownames(out_df) <- rownames(r_df)
    colnames(out_df) <- out_names
    out_df
  }
  r_sig <- interleave_cols(out$r, out$stars, sig_colname)
  sheets <- list(
    r_sig = add_rowname(r_sig),
    r = add_rowname(out$r),
    p = add_rowname(out$p)
  )
  dir.create(dirname(file_out), recursive = TRUE, showWarnings = FALSE)
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    openxlsx::addWorksheet(wb, nm)
    if (nm == "r_sig") {
      header <- c(rowname_label, as.vector(rbind(colnames(out$r), rep(sig_colname, length(colnames(out$r))))))
      header_df <- as.data.frame(t(header), check.names = FALSE)
      openxlsx::writeData(wb, nm, header_df, colNames = FALSE)
      openxlsx::writeData(wb, nm, sheets[[nm]], startRow = 2, colNames = FALSE)
    } else {
      openxlsx::writeData(wb, nm, sheets[[nm]], rowNames = FALSE)
    }
    n_rows <- nrow(sheets[[nm]]) + 1
    n_cols <- ncol(sheets[[nm]])
    style_all <- openxlsx::createStyle(fontName = font_name)
    style_num <- openxlsx::createStyle(fontName = font_name, halign = "right")
    style_sig <- openxlsx::createStyle(fontName = font_name, halign = "left")
    openxlsx::addStyle(
      wb, nm, style_all,
      rows = 1:n_rows, cols = 1:n_cols,
      gridExpand = TRUE, stack = TRUE
    )
    if (nm == "r_sig") {
      num_cols <- seq(2, n_cols, by = 2)
      sig_cols <- seq(3, n_cols, by = 2)
      openxlsx::addStyle(wb, nm, style_num, rows = 2:n_rows, cols = num_cols, gridExpand = TRUE, stack = TRUE)
      openxlsx::addStyle(wb, nm, style_sig, rows = 2:n_rows, cols = sig_cols, gridExpand = TRUE, stack = TRUE)
    } else {
      openxlsx::addStyle(wb, nm, style_num, rows = 2:n_rows, cols = 2:n_cols, gridExpand = TRUE, stack = TRUE)
    }
  }
  openxlsx::saveWorkbook(wb, file_out, overwrite = TRUE)
  invisible(out)
}
