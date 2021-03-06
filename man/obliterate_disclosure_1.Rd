% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rt_fund.R, R/rt_fund_pmc.R
\name{obliterate_disclosure_1}
\alias{obliterate_disclosure_1}
\title{Remove disclosures with inappropriate sentences}
\usage{
obliterate_disclosure_1(article)

obliterate_disclosure_1(article)
}
\arguments{
\item{article}{A List with paragraphs of interest.}
}
\value{
The list of paragraphs without mentions of financial COIs.

The list of paragraphs without mentions of financial COIs.
}
\description{
Returns the text without potentially misleading disclsoures mentions. This
    is intended to solve problems with disclosures, such as: Disclosure.
    Authors have no conflict of interests, and the work was not supported
    or funded by any drug company. This project was funded by the Deanship
    of Scientific Research, King Abdulaziz University, Jeddah, Saudi Arabia
    (Grant No. 4/165/1431);

Returns the text without potentially misleading disclsoures mentions. This
    is intended to solve problems with disclosures, such as: Disclosure.
    Authors have no conflict of interests, and the work was not supported
    or funded by any drug company. This project was funded by the Deanship
    of Scientific Research, King Abdulaziz University, Jeddah, Saudi Arabia
    (Grant No. 4/165/1431);
}
