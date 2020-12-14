
.get_xml <- function(filename, remove_ns = F) {

  if (remove_ns) {

    article_xml <-
      filename %>%
      xml2::read_xml() %>%
      xml2::xml_ns_strip()

  } else {

    article_xml <-
      filename %>%
      xml2::read_xml()

  }
}



.get_coi_pmc <- function(article_xml, synonyms) {

  coi_text <- ""
  is_coi_pred <- FALSE
  is_coi_pmc_fn <- NA
  is_coi_pmc_title <- NA


  coi_text <- .get_coi_pmc_fn(article_xml)
  is_coi_pmc_fn <- nchar(coi_text) > 0
  is_coi_pred <- is_coi_pmc_fn

  if (!is_coi_pred) {

    coi_text <- .get_coi_pmc_title(article_xml, synonyms)
    is_coi_pmc_title <- nchar(coi_text) > 0
    is_coi_pred <- is_coi_pmc_title

  }

  return(list(
    "is_coi_pred" = is_coi_pred,
    "coi_text" = coi_text,
    "is_coi_pmc_fn" = is_coi_pmc_fn,
    "is_coi_pmc_title" = is_coi_pmc_title
  ))
}



.get_fund_pmc <- function(article_xml, synonyms) {

  fund_text <- ""
  fund_pmc_institute <- ""
  fund_pmc_source <- ""
  fund_pmc_anysource <- ""

  is_fund_pred <- FALSE
  is_fund_pmc_group <- NA
  is_fund_pmc_title <- NA
  is_fund_pmc_anysource <- NA


  group_ls <- .get_fund_pmc_group(article_xml)

  fund_text <- group_ls$fund_statement_pmc
  fund_pmc_institute <- group_ls$fund_institute_pmc
  fund_pmc_source <- group_ls$fund_source_pmc
  is_fund_pmc_group <- group_ls$is_fund_group_pmc

  if (nchar(fund_text) == 0) {

    fund_text <- .get_fund_pmc_title(article_xml)
    is_fund_pmc_title <- nchar(fund_text) > 0
    is_fund_pred <- is_fund_pmc_title

  } else {

    is_fund_pred <- TRUE

  }

  if (!is_fund_pred) {

    # TODO Consider removing the if-statement  to always capture this

    fund_pmc_anysource <- .get_fund_pmc_source(article_xml)
    is_fund_pmc_anysource <- nchar(fund_pmc_anysource) > 0

  }

  return(list(
    "is_fund_pred" = is_fund_pred,
    "fund_text" = fund_text,
    "fund_pmc_institute" = fund_pmc_institute,
    "fund_pmc_source" = fund_pmc_source,
    "fund_pmc_anysource" = fund_pmc_anysource,
    "is_fund_pmc_group" = is_fund_pmc_group,
    "is_fund_pmc_title" = is_fund_pmc_title,
    "is_fund_pmc_anysource" = is_fund_pmc_anysource
  ))
}



.get_register_pmc <- function(article_xml) {

  type <- ""
  is_research <- FALSE
  is_review <- FALSE

  register_text <- ""
  is_register_pred <- FALSE
  is_reg_pmc_title <- FALSE


  research_types <- c(
    "research-article",
    "protocol",
    "letter",
    "brief-report",
    "data-paper"
  )

  review_types <- c(
    "review-article",
    "systematic-review"
  )

  type <- article_xml %>% xml2::xml_attr("article-type")
  is_research <- magrittr::is_in(type, research_types)
  is_review <- magrittr::is_in(type, review_types)

  if (!is_research & !is_review) {

    return(list(
      "is_register_pred" = is_register_pred,
      "register_text" = register_text,
      "type" = type,
      "is_research" = is_research,
      "is_review" = is_review,
      "is_reg_pmc_title" = is_reg_pmc_title
    ))
  }


  register_text <- .get_register_pmc_title(article_xml)
  is_reg_pmc_title <- nchar(register_text) > 0
  is_register_pred <- is_reg_pmc_title


  return(list(
    "is_register_pred" = is_register_pred,
    "register_text" = register_text,
    "type" = type,
    "is_research" = is_research,
    "is_review" = is_review,
    "is_reg_pmc_title" = is_reg_pmc_title
  ))
}



#' @returns Article sections as a list
.get_article_txt <- function(article_xml) {

  # Tidier but takes a median 11.0 ms vs current, which takes 10.6 ms
  section_names <- c(
    "ack",
    "body",
    "methods",
    "abstract",
    "footnotes"
  )

  section_funs <- list(
    .xml_ack,
    .xml_body,
    .xml_methods,
    .xml_abstract,
    .xml_footnotes
  )

  article_xml %>%
    purrr::map(section_funs, rlang::exec, .) %>%
    rlang::set_names(section_names)
}



#' @returns A list of PubMed IDs
.get_ids <- function(article_xml, remove_ns = F) {

  xpath <- c(
    "front/article-meta/article-id[@pub-id-type = 'pmid']",
    "front/article-meta/article-id[@pub-id-type = 'pmc']",
    "front/article-meta/article-id[@pub-id-type = 'pmc-uid']",
    "front/article-meta/article-id[@pub-id-type = 'doi']"
  )

  xpath %>%
    purrr::map(~ .get_text(article_xml, .x, T)) %>%
    rlang::set_names(c("pmid", "pmcid_pmc", "pmcid_uid", "doi"))
}



#' @returns A vector of pre-processed strings
.preprocess_txt <- function(article) {

  article %>%
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT', sub = "") %>%   # keep first
    trimws() %>%
    obliterate_fullstop_1() %>%
    obliterate_semicolon_1() %>%  # adds minimal overhead
    obliterate_comma_1() %>%   # adds minimal overhead
    obliterate_apostrophe_1() %>%
    obliterate_punct_1() %>%
    obliterate_line_break_1()

}


# metadata-all: 78ms
# metadata-unique: 75ms (only the ones not captured already)
# metadata-lean: 60ms (only the absolute basics for cool analyses)
# no metadata: 56ms
rt_all_pmc <- function(filename, remove_ns = F, all_meta = F) {

  # A lot of the PMC XML files are malformed
  article_xml <- tryCatch(.get_xml(filename, remove_ns), error = function(e) e)

  if (inherits(article_xml, "error")) {

     return(tibble::tibble(filename, is_success = F))

  }


  dict <- .create_synonyms()
  id_ls <- .get_ids(article_xml)
  id_ls$filename <- filename

  # Meta-data
  if (all_meta) {

    meta_ls <- .xml_metadata_all(article_xml, as_list = T)

  } else{

    meta_ls <- .xml_metadata_lean(article_xml, as_list = T)

  }


  pmc_coi_ls <- .get_coi_pmc(article_xml, dict)
  pmc_fund_ls <- .get_fund_pmc(article_xml, dict)
  pmc_reg_ls <- .get_register_pmc(article_xml)


  article_ls <- .get_article_txt(article_xml)
  # TODO Uncomment when I implement the .is_relevant functions
  # article_processed_ls <- purrr::map(article_ls, .preprocess_txt)


  coi_out  <- .rt_coi_pmc(
    article_ls,
    pmc_coi_ls,
    dict
  )

  coi_ls <- purrr::list_modify(pmc_coi_ls, !!!coi_out)



  fund_out <- .rt_fund_pmc(
    article_ls,
    pmc_fund_ls
  )

  fund_ls <- purrr::list_modify(pmc_fund_ls, !!!fund_out)



  reg_out  <- .rt_register_pmc(
    article_ls,
    pmc_reg_ls,
    dict
  )

  reg_ls <- purrr::list_modify(pmc_reg_ls, !!!reg_out)


  status_ls <- list(is_success = T)
  tibble::as_tibble(c(id_ls, meta_ls, coi_ls, fund_ls, reg_ls, status_ls))
}