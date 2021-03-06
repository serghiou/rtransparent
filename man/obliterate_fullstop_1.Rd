% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rt_fund.R
\name{obliterate_fullstop_1}
\alias{obliterate_fullstop_1}
\title{Remove fullstops that are unlikely to represent end of sentence}
\usage{
obliterate_fullstop_1(article)
}
\arguments{
\item{article}{A List with paragraphs of interest.}
}
\value{
The list of paragraphs without misleading fullstops.
}
\description{
Returns the list of paragraphs without potentially misleading fullstops.
}
