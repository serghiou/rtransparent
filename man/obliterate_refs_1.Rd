% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rt_fund.R, R/rt_register.R
\name{obliterate_refs_1}
\alias{obliterate_refs_1}
\title{Remove references}
\usage{
obliterate_refs_1(article)

obliterate_refs_1(article)
}
\arguments{
\item{article}{A List with paragraphs of interest.}
}
\value{
The list of paragraphs without misleading fullstops.

The list of paragraphs without mentions of financial COIs.
}
\description{
Returns the list of paragraphs without references.

Removes mentions such as: "An active o <- servational cohort study was
    conducted as previously reported (ClinicalTrials.gov identifier
    NCT01280162) [16]."
}
