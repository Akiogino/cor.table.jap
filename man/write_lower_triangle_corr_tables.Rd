\name{write_lower_triangle_corr_tables}
\alias{write_lower_triangle_corr_tables}
\title{Write lower-triangle correlation tables to CSV}
\usage{write_lower_triangle_corr_tables(df, vars, file_base, quote_text = TRUE, rowname_label = "項目名", file_encoding = "UTF-8", sig_colname = "")}
\arguments{
  \item{df}{A data.frame containing the variables.}
  \item{vars}{Character vector of variable names.}
  \item{file_base}{File path without extension (suffixes are added).}
  \item{quote_text}{If TRUE, replaces the leading space in positive values with a non-breaking space.}
  \item{rowname_label}{Label for the first column (row names).}
  \item{file_encoding}{File encoding (default UTF-8).}
  \item{sig_colname}{Column name for significance columns in r_sig (default: blank).}
}
\value{A list with data.frames: r, p, stars (invisibly).}
\description{Write r, p, stars, and r+sig tables to CSV with suffixes _r/_p/_stars/_r_sig.}
