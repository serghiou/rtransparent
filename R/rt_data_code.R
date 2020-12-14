#' Identify and extract Data and Code statements in TXT files.
#'
#' Takes a TXT file and returns data related to the presence of Data and/or Code
#'     statements, including whether Data and/or Code statements exist. If such
#'     statements exist, it extracts them.
#'
#' @param filename The name of the TXT file as a string.
#' @return A dataframe of results.
#' @examples
#' \dontrun{
#' # Path to PMC XML.
#' filepath <- "../inst/extdata/00003-PMID26637448-PMC4737611.txt"
#'
#' # Identify and extract meta-data and indicators of transparency.
#' results_table <- rt_data(filepath)
#' }
#' @export
rt_data_code <- function(filename) {

  # Read TXT file
  article <- readr::read_file(filename)

  # Tokenize
  article_tokens <-
    article %>%
    .obliterate_fullstop_1() %>%
    .tokenize() %>%
    list() %>%
    rlang::set_names("10.17605/OSF.IO/E58WS")  # otherwise oddpub throws error

  # Extract indicators
  out_df <- oddpub::open_data_search(article_tokens, detected_sentences = T)

  # Return
  return(out_df)
}