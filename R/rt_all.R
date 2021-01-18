#' Identify and extract statements of COI, Funding and Registration.
#'
#' Takes a TXT file and examines whether any statements of Conflicts of Interest
#'     (COI), Funding or Protocol Registration exist. If any such statements are
#'     found, it also extracts the relevant text.
#'
#' @param filename The name of the TXT as a string.
#' @return A dataframe of results. A dataframe of results. It returns the PMID
#'     of the article (if this was included in the filename and preced by
#'     "PMID"), whether each of 3 indicators of transparency (COI, Funding or
#'     Registration) was identified, the relevant text identified and whether
#'     each labelling function identified relevant text or not. The labeling
#'     functions are eturned to add flexibility in how this package is used; for
#'     example, future definitions of Registration may differ from the one we
#'     used. If a labelling function returns NA it means that it was not run.
#' @examples
#' \dontrun{
#' # Path to TXT.
#' filepath <- "../inst/extdata/00003-PMID26637448-PMC4737611.txt"
#'
#' # Identify and extract indicators of transparency.
#' results_table <- rt_all(filepath)
#' }
#' @export
rt_all <- function(filename) {

  # Avoid automated checking warning in R package development
  article <- pmid <- NULL

  # Extract indicators
  # TODO Modify functions to avoid loading the TXT file multiple times.
  out_ls <- list(
    coi_df = rt_coi(filename) %>% dplyr::select(!(article:pmid)),
    fund_df = rt_fund(filename) %>% dplyr::select(!(article:pmid)),
    register_df = rt_register(filename)
  )

  # Return dataframe of indicators
  out_ls %>%
    dplyr::bind_cols() %>%
    dplyr::select(article, pmid, tidyselect::everything())
}