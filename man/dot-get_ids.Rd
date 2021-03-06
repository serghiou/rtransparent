% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xml_utils.R
\name{.get_ids}
\alias{.get_ids}
\title{Extract PMIDs and DOIs}
\usage{
.get_ids(article_xml)
}
\arguments{
\item{article_xml}{The article as an xml_document.}
}
\value{
A list of PubMed IDs
}
\description{
Reads a PMC XML as an xml_document and extracts the DOI, PMID and PMCID as
    a list.
}
