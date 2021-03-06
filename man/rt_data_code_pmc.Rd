% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rt_data_code_pmc.R
\name{rt_data_code_pmc}
\alias{rt_data_code_pmc}
\title{Identify and extract Data and Code sharing from PMC XML files.}
\usage{
rt_data_code_pmc(filename, remove_ns = T, specificity = "low")
}
\arguments{
\item{filename}{The filename of the XML file to be analyzed as a string.}

\item{remove_ns}{TRUE if an XML namespace exists, else FALSE (default).}

\item{specificity}{How specific should the extraction of text from the XML
be? If "low" then this is a as sensitive as possible (it extracts all
text). If "moderate", then it extracts all paragraphs. If "high", then it
only extracts text from specific locations (footnotes, methods,
supplements).}
}
\value{
A dataframe of results. It returns unique IDs of the article,
    whether this article is deemed a research article, whether it is deemed
    relevant to data or code, whether data or code was found, and if so, what
    the text that suggested the presence of data or code was. Takes a median
    of 200ms per article.
}
\description{
Takes a PMC XML file and returns data related to the presence of Data or
    Code, including whether Data or Code have been shared. If Data or Code
    exist, it will extract the relevant text for each.
}
\examples{
\dontrun{
# Path to PMC XML
filepath <- "../inst/extdata/00003-PMID26637448-PMC4737611.xml"

# Identify and extract meta-data and indicators of transparency
results_table <- rt_data_pmc(filepath, remove_ns = T)
}
}
