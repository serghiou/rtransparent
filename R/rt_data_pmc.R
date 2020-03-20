#' Tokenize NLM XML files
#'
#' The code for this was modified from the oddpub package.
#'
#' @param article The article as a string
#'
#' @return List with one element per document. Each document is split into its
#'     sentences and saved as a vector of strings.
.tokenize_xml <- function(article) {

  article %>%
    paste(collapse = " ") %>%
    tokenizers::tokenize_sentences(simplify = TRUE) %>%
    tolower %>%
    stringr::str_replace_all(pattern = ",", replacement = "") %>%
    oddpub:::.correct_tokenization()

}

# Runs 3x faster than sensitive (median: 1.4s vs 3.5s)
rt_data_pmc_specific <- function(filename) {

  xpath <- c(
    "front/article-meta/article-id[@pub-id-type = 'pmid']",
    "front/article-meta/article-id[@pub-id-type = 'pmc']",
    "front/article-meta/article-id[@pub-id-type = 'pmc-uid']",
    "front/article-meta/article-id[@pub-id-type = 'doi']"
  )

  index_any <- list(
    is_open_data = NA,
    is_open_code = NA,
    com_specific_db = "",
    com_general_db = "",
    com_github_data = "",
    dataset = "",
    com_code = "",
    com_suppl_code = "",
    com_file_formats = "",
    com_supplemental_data = "",
    com_data_availability = ""

  )

  out <- list(
    pmid = NA,
    pmcid_pmc = NA,
    pmcid_uid = NA,
    doi = NA,
    is_research = NA,
    is_review = NA,
    is_relevant_data = NA,
    is_relevant_code = NA
  )

  article_xml <- xml2::read_xml(filename) %>% xml2::xml_ns_strip()

  out %<>% purrr::list_modify(!!!map(xpath, ~ .get_text(article_xml, .x, T)))

  if (nchar(out$doi) == 0) {

    out$doi <- "not found"

  }


  # Check for type
  # Definitions at PMC -> Tagging Guidelines -> Document Objects
  research_types <- c(
    "research-article",
    "protocol",
    "letter",
    "brief-report",
    "data-paper"
  )

  review_types <- c(
    "review-article",  # SLRs can also be labelled as review-article...
    "systematic-review"
  )

  type <- article_xml %>% xml2::xml_attr("article-type")

  out$is_research <- magrittr::is_in(type, research_types)
  out$is_review <- magrittr::is_in(type, review_types)

  if (!out$is_research & !out$is_review) {

    return(tibble::as_tibble(c(out, index_any)))

  }


  unwanted_xpaths <- c(
    "front/article-meta/funding-group",
    "body//funding-source",
    "back//funding-source"
  )

  unwanted_xpath <- paste(unwanted_xpaths, collapse = " | ")

  article_xml %>%
    xml_find_all(unwanted_xpath) %>%
    xml_remove(free = T)

  # Processing time is only marginally higher than combining xpaths into one
  ack <- .xml_ack(article_xml)
  suppl <- .xml_suppl(article_xml)
  methods <- .xml_methods(article_xml, with_refs = F)
  footnotes <- .xml_footnotes(article_xml, all = T) %>% obliterate_contribs()
  article <- c(footnotes, methods, ack, suppl)


  # Check relevance
  data <- "\\b[Dd]ata\\b|[Dd]ataset|\\b[Ff]ile\\b|download|[Ss]har|[Aa]vailabl"
  code <- "\\b[Cc]ode\\b|\\b[Ss]cript\\b|GitHub|BitBucket"

  has_data <- stringr::str_detect(article, regex(data, ignore_case = T))
  has_code <- stringr::str_detect(article, regex(code, ignore_case = T))

  # has_data <- stringr::str_which(article, regex(data, ignore_case = T))
  # has_code <- stringr::str_which(article, regex(code, ignore_case = T))

  # out$is_relevant_data <- !!length(has_data)
  # out$is_relevant_code <- !!length(has_code)

  out$is_relevant_data <- any(has_data)
  out$is_relevant_code <- any(has_code)

  # No speed difference
  # article <- article[has_data]

  article_tokens <-
    article %>%
    .tokenize_xml() %>%
    list() %>%
    rlang::set_names(out$doi)

  # article_tokens <- list()
  # article_tokens[[1]] <- paste(article[unique(c(has_data, has_code))], collapse = " ")
  # names(article_tokens) <- out$doi

  indicators <- oddpub::open_data_search(article_tokens)
  sentences <- oddpub::open_data_sentences(article_tokens)

  adjudication <- suppressMessages(dplyr::full_join(indicators, sentences))
  out <- tibble::as_tibble(out)

  dplyr::bind_cols(out, adjudication) %>% dplyr::select(-article)
}



rt_data_pmc_sensitive <- function(filename) {

  xpath <- c(
    "front/article-meta/article-id[@pub-id-type = 'pmid']",
    "front/article-meta/article-id[@pub-id-type = 'pmc']",
    "front/article-meta/article-id[@pub-id-type = 'pmc-uid']",
    "front/article-meta/article-id[@pub-id-type = 'doi']"
  )

  index_any <- list(
    is_open_data = NA,
    is_open_code = NA,
    com_specific_db = "",
    com_general_db = "",
    com_github_data = "",
    dataset = "",
    com_code = "",
    com_suppl_code = "",
    com_file_formats = "",
    com_supplemental_data = "",
    com_data_availability = ""

  )

  out <- list(
    pmid = NA,
    pmcid_pmc = NA,
    pmcid_uid = NA,
    doi = NA,
    is_research = NA,
    is_review = NA,
    is_relevant_data = NA,
    is_relevant_code = NA
  )

  article_xml <-
    filename %>%
    read_xml() %>%
    xml_ns_strip()


  out %<>% purrr::list_modify(!!!map(xpath, ~ .get_text(article_xml, .x, T)))

  if (nchar(out$doi) == 0) {

    out$doi <- "not found"

  }


  # Check for type
  # Definitions at PMC -> Tagging Guidelines -> Document Objects
  research_types <- c(
    "research-article",
    "protocol",
    "letter",
    "brief-report",
    "data-paper"
  )

  review_types <- c(
    "review-article",  # SLRs can also be labelled as review-article...
    "systematic-review"
  )

  type <- article_xml %>% xml2::xml_attr("article-type")

  out$is_research <- magrittr::is_in(type, research_types)
  out$is_review <- magrittr::is_in(type, review_types)

  if (!out$is_research & !out$is_review) {

    return(tibble::as_tibble(c(out, index_any)))

  }

  unwanted_xpaths <- c(
    "front/journal-meta",
    "front//contrib-group",
    "front//aff",
    "front//abstract",
    "front/article-meta/funding-group",
    "body//xref",
    "body//table-wrap",
    "body//title",
    "body//funding-source",
    "back/ref-list",
    "back//funding-source",
    "floats-group"
  )

  unwanted_xpath <- paste(unwanted_xpaths, collapse = " | ")

  article_xml %>%
    xml_find_all(unwanted_xpath) %>%
    xml_remove(free = T)

  article <-
    article_xml %>%
    xml_find_all(".//text()[normalize-space()]") %>%  # adds space b/t elements
    xml_text() %>%
    paste(collapse = " ")


  # Check relevance
  data <- "\\b[Dd]ata\\b|[Dd]ataset|\\b[Ff]ile\\b|download|[Ss]har|[Aa]vailabl"
  code <- "\\b[Cc]ode\\b|\\b[Ss]cript\\b|GitHub|BitBucket"

  has_data <- stringr::str_detect(article, regex(data, ignore_case = T))
  has_code <- stringr::str_detect(article, regex(code, ignore_case = T))

  out$is_relevant_data <- any(has_data)
  out$is_relevant_code <- any(has_code)


  article_tokens <-
    article %>%
    .tokenize_xml() %>%
    list() %>%
    rlang::set_names(out$doi)


  indicators <- oddpub::open_data_search(article_tokens)
  sentences <- oddpub::open_data_sentences(article_tokens)

  adjudication <- suppressMessages(dplyr::full_join(indicators, sentences))
  out <- tibble::as_tibble(out)

  dplyr::bind_cols(out, adjudication) %>% dplyr::select(-article)

}