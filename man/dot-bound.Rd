% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.bound}
\alias{.bound}
\title{Place boundaries around words}
\usage{
.bound(x, location = "end")
}
\arguments{
\item{x}{A vector of strings.}

\item{location}{Where to bound each word ("both" (default), "end" or
"start").}
}
\value{
A vector of bounded strings.
}
\description{
Returns each string with boundaries around it.
}
