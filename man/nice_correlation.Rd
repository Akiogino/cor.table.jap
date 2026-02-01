\name{nice_correlation}
\alias{nice_correlation}
\title{Correlation table with significance marks}
\usage{nice_correlation(df, rows, cols, quote_text = FALSE)}
\arguments{
  \item{df}{A data.frame containing the variables.}
  \item{rows}{Character vector of row variable names.}
  \item{cols}{Character vector of column variable names.}
  \item{quote_text}{If TRUE, replaces the leading space in positive values with a non-breaking space.}
}
\value{A data.frame with formatted correlations for the specified rows and columns.}
\description{Create a correlation table with aligned decimals and significance marks.}
