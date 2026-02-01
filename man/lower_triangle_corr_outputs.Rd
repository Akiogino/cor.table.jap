\name{lower_triangle_corr_outputs}
\alias{lower_triangle_corr_outputs}
\title{Lower-triangle correlation outputs}
\usage{lower_triangle_corr_outputs(df, vars, quote_text = TRUE)}
\arguments{
  \item{df}{A data.frame containing the variables.}
  \item{vars}{Character vector of variable names.}
  \item{quote_text}{If TRUE, replaces the leading space in positive values with a non-breaking space.}
}
\value{A list with data.frames: r, p, stars.}
\description{Create numeric r and p matrices plus a display matrix with significance stars.}
