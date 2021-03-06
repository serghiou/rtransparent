% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rt_coi_pmc.R
\name{.which_advisor_1}
\alias{.which_advisor_1}
\title{Identify advisors}
\usage{
.which_advisor_1(article, dict)
}
\arguments{
\item{article}{The text as a vector of strings.}

\item{dict}{A list of regular expressions for each concept.}
}
\value{
Index of elements with phrase of interest
}
\description{
Identify mentions such as: "SS is a scientific advisor of GSK."
}
