\name{lower_triangle_corr_table}
\alias{lower_triangle_corr_table}
\title{Lower-triangle correlation table}
\usage{lower_triangle_corr_table(df, vars, quote_text = FALSE)}
\arguments{
  \item{df}{A data.frame containing the variables.}
  \item{vars}{Character vector of variable names.}
  \item{quote_text}{If TRUE, replaces the leading space in positive values with a non-breaking space.}
}
\value{A data.frame with formatted correlations in the lower triangle.}
\description{Create a lower-triangle correlation table with aligned decimals and significance marks.}
