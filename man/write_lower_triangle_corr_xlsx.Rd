\name{write_lower_triangle_corr_xlsx}
\alias{write_lower_triangle_corr_xlsx}
\title{Write lower-triangle correlation tables to Excel}
\usage{write_lower_triangle_corr_xlsx(df, vars, file_out, quote_text = TRUE, rowname_label = "項目名", font_name = "Times New Roman", sig_colname = "")}
\arguments{
  \item{df}{A data.frame containing the variables.}
  \item{vars}{Character vector of variable names.}
  \item{file_out}{Output .xlsx file path.}
  \item{quote_text}{If TRUE, replaces the leading space in positive values with a non-breaking space.}
  \item{rowname_label}{Label for the first column (row names).}
  \item{font_name}{Font name to apply in the Excel output (default: Times New Roman).}
  \item{sig_colname}{Column name for significance columns in r_sig (default: blank).}
}
\value{A list with data.frames: r, p, stars (invisibly).}
\description{Write r, p, and r_sig tables to an Excel file with three sheets.}
