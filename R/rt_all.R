#' Identify and extract statements of COI, Funding and Registration.
#'
#' Takes a TXT file and examines whether any statements of Conflicts of Interest
#'     (COI), Funding or Protocol Registration exist. If any such statements are
#'     found, it also extracts the relevant text.
#'
#' @param filename The name of the TXT as a string.
#' @return A dataframe of results.
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