% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rt_register_pmc.R
\name{.which_ct_1}
\alias{.which_ct_1}
\title{Identify mentions of registration on ClinicalTrials.gov}
\usage{
.which_ct_1(article)
}
\arguments{
\item{article}{A string or a list of strings.}
}
\value{
Index of element with phrase of interest
}
\description{
Extract the index of mentions such as: "The study is registered at
    www.clinicaltrials.gov (NCT01624883)."
}
