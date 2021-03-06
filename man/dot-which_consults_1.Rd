% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rt_coi_pmc.R
\name{.which_consults_1}
\alias{.which_consults_1}
\title{Identify mentions of consulting}
\usage{
.which_consults_1(article, dict)
}
\arguments{
\item{article}{The text as a vector of strings.}

\item{dict}{A list of regular expressions for each concept.}
}
\value{
Index of elements with phrase of interest
}
\description{
Identify mentions such as: "SS consults for GSK."
}
