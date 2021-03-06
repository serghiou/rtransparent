% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rt_register_pmc.R
\name{.which_funded_ct_1}
\alias{.which_funded_ct_1}
\title{Identify mentions of funding followed by NCT}
\usage{
.which_funded_ct_1(article, dict)
}
\arguments{
\item{article}{A string or a list of strings.}

\item{dict}{A list of regular expressions for each concept.}
}
\value{
Index of element with phrase of interest
}
\description{
Extract the index of mentions such as: "Funded by: the National Heart, Lung,
    and Blood Institute, the National Institute of Diabetes and Digestive and
    Kidney Disease, and others; SPECS ClinicalTrials.gov number, NCT00443599;
    Nutrition and Obesity Center at Harvard; NIH 5P30DK040561-17"
}
