% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{stats}
\alias{stats}
\title{Summary Statistics}
\usage{
stats(data, x, ...)
}
\arguments{
\item{data}{a data frame}

\item{x}{a numeric variable}

\item{...}{one or more categorical variables}
}
\value{
a tibble with n, mean, and standard deviation
}
\description{
Calculate simple statistics by group level
}
\examples{
stats(mtcars,mpg,am)
stats(mtcars, mpg, am, vs)
}
