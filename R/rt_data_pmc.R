#' Tokenize NLM XML files
#'
#' The code for this was modified from the oddpub package.
#'
#' @param article The article as a string
#'
#' @return List with one element per document. Each document is split into its
#'     sentences and saved as a vector of strings.
.tokenize <- function(article) {

  article %>%
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT', sub = "") %>%
    paste(collapse = " ") %>%
    tokenizers::tokenize_sentences(simplify = TRUE) %>%
    tolower %>%
    stringr::str_replace_all(pattern = ",", replacement = "") %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("s\\s+([0-9])", "s\\1") %>%
    obliterate_contribs() %>%
    oddpub:::.correct_tokenization()

}



.get_type <- function(article_xml) {

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

  is_research <- magrittr::is_in(type, research_types)
  is_review <- magrittr::is_in(type, review_types)

  tibble::tibble(is_research, is_review)
}



.remove_elements <- function(article_xml, extensive = T) {

  if (extensive) {

    unwanted_xpaths <- c(
      "front/journal-meta",
      "front//contrib-group",
      "front//aff",
      # "front//abstract",  # led to false negatives (e.g. 1979)
      "front/article-meta/funding-group",
      "body//xref[@ref-type='bibr']",
      # "body//table-wrap", # led to false negatives (e.g. 1840)
      "body//title",
      "body//funding-source",
      "back/ref-list",
      "back//funding-source"
      # "floats-group"  # led to false negatives (e.g. 1156)
    )

  } else {

    unwanted_xpaths <- c(
      "front/article-meta/funding-group",
      "body//funding-source",
      "back//funding-source"
    )
  }

  unwanted_xpath <- paste(unwanted_xpaths, collapse = " | ")

  article_xml %>%
    xml_find_all(unwanted_xpath) %>%
    xml_remove()
}



.get_elements <- function(article_xml, specificity = "low") {

  if (specificity == "high") {

    # Processing time is only marginally higher than combining xpaths into one
    ack <- .xml_ack(article_xml)
    suppl <- .xml_suppl(article_xml)
    methods <- .xml_methods(article_xml, with_refs = F)
    footnotes <-
      .xml_footnotes(article_xml, all = T) %>%
      obliterate_contribs()

    article <- c(footnotes, methods, ack, suppl)

    return(article)

  }

  if (specificity == "moderate") {

    article1 <-
      article_xml %>%
      xml2::xml_find_all(".//p | .//ext-link") %>%
      # xml_find_all(".//text()[normalize-space()]") %>%
      xml2::xml_text()

    # Required - a version of footnotes
    article2 <-
      article_xml %>%
      xml2::xml_find_all(".//custom-meta | .//notes") %>%
      purrr::map(~ xml_contents(.x) %>% xml_text) %>%
      purrr::map_chr(paste, collapse = ": ")

    # Required!
    article3 <- .xml_suppl(article_xml)

    return(unique(c(article1, article2, article3)))
  }

  if (specificity == "low") {

    article <-
      article_xml %>%
      xml_find_all(".//text()[normalize-space()]") %>%
      xml_text() %>%
      paste(collapse = " ")

    return(article)
  }
}



.check_relevance <- function(article, as_tbl = F, as_vec = F) {

  data_synonyms <- c(
    "\\b[Dd]ata\\b",
    "[Dd]ataset",
    "[Dd]atabase",
    "\\b[Ff]ile\\b",
    "download",
    "[Ss]har",
    "[Aa]vailabl",
    "[Ss]equence",
    "[Dd]eposit",
    "genbank",
    "\\b[Tt]able(|s)\\b",  # this is to mimic the poor behavior seen in PDFs
    "\\bpdb\\b",
    "[Aa]ccession"
  )

  code_synonyms <- c(
    "\\b[Cc]ode\\b",
    "\\b[Ss]cript\\b",
    "[Gg]it[Hh]ub",
    "BitBucket",
    "[Ss]oftware",
    "[Tt]ool"
  )

  data_regex <-
    data_synonyms %>%
    paste(collapse = "|") %>%
    stringr::regex(ignore_case = T)

  code_regex <-
    code_synonyms %>%
    paste(collapse = "|") %>%
    stringr::regex(ignore_case = T)

  data_loc <- stringr::str_which(article, data_regex)
  code_loc <- stringr::str_which(article, code_regex)

  if (as_tbl) {

    is_relevant_data <- !!length(data_loc)
    is_relevant_code <- !!length(code_loc)

    return(tibble::tibble(is_relevant_data, is_relevant_code))

  }

  if (as_vec) {

    is_relevant_data <- !!length(data_loc)
    is_relevant_code <- !!length(code_loc)
    return(any(c(is_relevant_data, is_relevant_code)))
  }

  list(data_loc = data_loc, code_loc = code_loc)
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
    .tokenize() %>%
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


#' @param filename The filename of the XML file to be analyzed as a string.
#' @param remove_ns Whether the namespace of the XML should be removed (T or F).
#' @param specificity How specific should the extraction of text from the XML
#'     be? If "low" then this is a as sensitive as possible (it extracts all
#'     text). If "moderate", then it extracts all paragraphs. If "high", then it
#'     only extracts text from specific locations (footnotes, methods,
#'     supplements).
#' @return Index of element with phrase of interest. Takes a median of250ms per
#'     article.
rt_data_pmc <- function(filename, remove_ns = T, specificity = "low") {

  # A lot of the PMC XML files are malformed
  article_xml <- tryCatch(.get_xml(filename, remove_ns), error = function(e) e)

  if (inherits(article_xml, "error")) {

    return(tibble::tibble(filename, is_success = F))

  }


  # Extract IDs
  id_ls <- .get_ids(article_xml)
  id_ls$filename <- filename
  id_df <- tibble::as_tibble(id_ls)


  # Extract type
  type_df <- .get_type(article_xml)
  is_type <- type_df %>% unlist() %>% any()


  if (!is_type) {

    return(dplyr::bind_cols(id_df, type_df, is_success = T))

  }


  # Extract text
  article_xml %>% .remove_elements(extensive = T)
  article <- .get_elements(article_xml, specificity = specificity)


  # Tokenize
  article_tokens <-
    article %>%
    obliterate_fullstop_1() %>%
    .tokenize() %>%
    list() %>%
    rlang::set_names(id_df$doi)


  # Keep relevant
  relevant_ls <- .check_relevance(unlist(article_tokens))
  is_relevant_data <- !!length(relevant_ls$data_loc)
  is_relevant_code <- !!length(relevant_ls$code_loc)
  relevant_df <- tibble::tibble(is_relevant_data, is_relevant_code)

  if (!is_relevant_data & !is_relevant_code) {

    return(dplyr::bind_cols(id_df, type_df, relevant_df, is_success = T))

  }

  # Reduces median run time from 270 to 250 (7% reduction)
  # wanted_sentences <- unique(unlist(relevant_ls))
  # article_tokens %<>% purrr::map(magrittr::extract, wanted_sentences)


  # Extract indicators
  out_df <- open_data_search(article_tokens, detected_sentences = T)


  dplyr::bind_cols(id_df, type_df, relevant_df, out_df, is_success = T)
}


#' @param filename The filename of the XML file to be analyzed as a string.
#' @param remove_ns Whether the namespace of the XML should be removed (T or F).
#' @param specificity How specific should the extraction of text from the XML
#'     be? If "low" then this is a as sensitive as possible (it extracts all
#'     text). If "moderate", then it extracts all paragraphs. If "high", then it
#'     only extracts text from specific locations (footnotes, methods,
#'     supplements).
#' @return Index of element with phrase of interest. Takes a median of200ms per
#'     article.
rt_data_pmc_list <- function(filenames, remove_ns = T, specificity = "low") {

  article_xmls <-
    filenames %>%
    purrr::map(~ tryCatch(.get_xml(.x, remove_ns), error = function(e) e))


  is_success <- purrr::map_lgl(article_xmls, ~ !inherits(.x, "error"))
  article_xmls %<>% purrr::keep(is_success)

  if (!length(article_xmls)) {

    return(tibble::tibble(filenames, is_success = F))

  }


  # Extract IDs
  id_dfs <-
    article_xmls %>%
    purrr::map_dfr(.get_ids) %>%
    dplyr::mutate(doi = ifelse(nchar(doi) == 0, "not found", doi)) %>%
    dplyr::mutate(filename = filenames[is_success])


  # Extract type (not at the moment - tricky code for minimal gain in time)
  # type_df <- purrr::map(article_xmls, .get_type)
  # is_type <- purrr::map(type_df, ~ any(unlist(.x)))
  # article_xmls %<>% keep(is_type)


  # Extract text
  purrr::map(article_xmls, .remove_elements)
  articles <- purrr::map(article_xmls, .get_elements, specificity = specificity)


  # Tokenize (fast)
  articles_tokens <-
    articles %>%
    purrr::map(obliterate_fullstop_1) %>%
    purrr::map(.tokenize) %>%
    rlang::set_names(id_dfs$doi)


  # Keep relevant
  rel_ls <- purrr::map(articles_tokens, ~ .check_relevance(unlist(.x)))

  .get_inds <- function(x) {

    inds <- unlist(x)
    inds <- sort(unique(c(inds, inds + 1, inds - 1)))
    inds <- inds[inds > 0]

    # Do this b/c there may not be any relevant values, which messes the code
    if (!length(inds)) inds <- 1
    return(inds)
  }

  a <- purrr::map(rel_ls, .get_inds)
  articles_tokens %<>% purrr::map2(a, ~ magrittr::extract(.x, unlist(.y)))
  articles_tokens %<>% purrr::map(purrr::discard, is.na) %>% purrr::compact()
  # articles_tokens %<>% purrr::map2(rel_ls, ~ magrittr::extract(.x, unlist(.y)))
  # articles_tokens%<>%purrr::map2(rel_ls,~modify_at(.x, -unlist(.y), `<-`, ""))
  relevant_df <- purrr::map_dfr(articles, .check_relevance, as_tbl = T)


  # Extract indicators (slow)
  # article_tokens <- map(article_tokens, paste, collapse = " ")
  indicator_df <- open_data_search(articles_tokens)


  if (any(!is_success)) {

    a <- cbind(id_dfs, relevant_df, indicator_df, is_success = T)
    b <- tibble::tibble(filename = filenames[!is_success], is_success = F)
    out_df <- dplyr::bind_rows(a, b)

  } else {

    out_df <- cbind(id_dfs, relevant_df, indicator_df, is_success = T)

  }

  return(tibble::as_tibble(out_df))
}





# Runs 3x faster than sensitive (median: 1.4s vs 3.5s)
# This now extracts data better than the PDF, which I think is creating trouble
# for the code developed on the messy PDF. As such, even though much faster, I
# will default to the sensitive for now.
rt_data_pmc_specific_list <- function(filenames, remove_ns = T) {

  if (remove_ns) {

    article_xmls <-
      filenames %>%
      map(xml2::read_xml) %>%
      map(xml2::xml_ns_strip)  # This is slow

  } else {

    article_xmls <-
      filenames %>%
      map(xml2::read_xml)

  }


  .get_id <- function(article_xml) {

    xpath <- c(
      pmid = "front/article-meta/article-id[@pub-id-type = 'pmid']",
      pmcid_pmc = "front/article-meta/article-id[@pub-id-type = 'pmc']",
      pmcid_uid = "front/article-meta/article-id[@pub-id-type = 'pmc-uid']",
      doi = "front/article-meta/article-id[@pub-id-type = 'doi']"
    )

    purrr::map_dfc(xpath, ~ .get_text(article_xml, .x, T))

  }

  id_df <-
    article_xmls %>%
    purrr::map_dfr(.get_id) %>%
    mutate(doi = ifelse(nchar(doi) == 0, "not found", doi))


  .get_type <- function(article_xml) {

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

    is_research <- magrittr::is_in(type, research_types)
    is_review <- magrittr::is_in(type, review_types)

    tibble(is_research, is_review)
  }

  type_df <- purrr::map_dfr(article_xmls, .get_type)
  is_type <- type_df %>% mutate(a = is_research | is_review) %>% pull(a)


  .remove_elements <- function(article_xml, extensive = T) {

    if (extensive) {

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

    } else {

    unwanted_xpaths <- c(
      "front/article-meta/funding-group",
      "body//funding-source",
      "back//funding-source"
    )

    unwanted_xpath <- paste(unwanted_xpaths, collapse = " | ")

    article_xml %>%
      xml_find_all(unwanted_xpath) %>%
      xml_remove()
    }
  }

  purrr::map(article_xmls, .remove_elements)
  # purrr::map(article_xmls[is_type], .remove_elements)


  # Mentions of deposition in introduction, etc., so this is too restrictive
  .get_elements <- function(article_xml, specific = F) {

    if (specific) {

      # Processing time is only marginally higher than combining xpaths into one
      ack <- .xml_ack(article_xml)
      suppl <- .xml_suppl(article_xml)
      methods <- .xml_methods(article_xml, with_refs = F)
      footnotes <-
        .xml_footnotes(article_xml, all = T) %>%
        obliterate_contribs()

      article <- c(footnotes, methods, ack, suppl)

    } else {

      article1 <-
        article_xml %>%
        xml_find_all(".//p | .//ext-link") %>%
        # xml_find_all(".//text()[normalize-space()]") %>%
        xml_text()

      # Required - a version of footnotes
      article2 <-
        article_xml %>%
        xml_find_all(".//custom-meta | .//notes") %>%
        purrr::map(~ xml_contents(.x) %>% xml_text) %>%
        purrr::map_chr(paste, collapse = ": ")

      # Required!
      article3 <- .xml_suppl(article_xml)

      article <- c(article1, article2, article3)

    }
    return(article)
  }

  # A bit slow
  articles <- purrr::map(article_xmls, .get_elements)
  # articles <- purrr::map(article_xmls[is_type], .get_elements)


  .check_relevance <- function(article, any = F) {

    data_synonyms <- c(
      "\\b[Dd]ata\\b",
      "[Dd]ataset",
      "\\b[Ff]ile\\b",
      "download",
      "[Ss]har",
      "[Aa]vailabl",
      "[Ss]equence",
      "[Dd]eposit",
      "genbank",
      "\\bpdb\\b",
      "[Aa]ccession"
    )

    code_synonyms <- c(
      "\\b[Cc]ode\\b",
      "\\b[Ss]cript\\b",
      "[Gg]it[Hh]ub",
      "BitBucket",
      "[Ss]oftware",
      "[Tt]ool"
    )

    data <- paste(data_synonyms, collapse = "|")
    code <- paste(code_synonyms, collapse = "|")


    has_data <- stringr::str_detect(article, regex(data, ignore_case = T))
    has_code <- stringr::str_detect(article, regex(code, ignore_case = T))

    is_relevant_data <- any(has_data)
    is_relevant_code <- any(has_code)

    if (any) {

      return(any(c(is_relevant_data, is_relevant_code)))

    }

    tibble::tibble(is_relevant_data, is_relevant_code)
  }

  # The first is slow the second is fast
  # 8x faster when I include this step, than without, so very worth it
  articles %<>% purrr::map(~ keep(.x, .check_relevance, any = T))
  relevance_df <- purrr::map_dfr(articles, .check_relevance, any = F)


  articles %<>% purrr::map(obliterate_fullstop_1)

  # No speed difference
  # article <- article[has_data]
  # This is fast
  articles_tokenized <-
    articles %>%
    purrr::map(.tokenize) %>%
    rlang::set_names(id_df$doi)
    # rlang::set_names(id_df$doi[is_type])

  # These are slow
  indicator_df <- oddpub::open_data_search(articles_tokenized)
  sentences_df <- oddpub::open_data_sentences(articles_tokenized)


  meta_df <- bind_cols(id_df, type_df)

  relevant_df <-
    meta_df %>%
    # filter(is_type) %>%
    bind_cols(relevance_df, indicator_df, sentences_df)

  bind_rows(meta_df %>% filter(!is_type), relevant_df) %>% select(-article1)
}


.pdf_load <- function(filenames) {

  out_list <-
    filenames %>%
    lapply(read_lines) %>%
    lapply(function(x) iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT', sub = "")) %>%
    lapply(.tokenize) %>%
    {set_names(., sample(length(.)))}
}



.extract_txt_ns <- function(filename, remove_ns = F) {

  if (remove_ns) {

    article_xml <-
      filename %>%
      read_xml() %>%
      xml_ns_strip()

  } else {

    article_xml <-
      filename %>%
      read_xml()

  }

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

  is_research <- magrittr::is_in(type, research_types)
  is_review <- magrittr::is_in(type, review_types)

  if (!is_research & !is_review) {

    return(print("Not an empirical study"))

  }

  unwanted_xpaths <- c(
    "d1:front/d1:journal-meta",
    "d1:front//d1:contrib-group",
    "d1:front//d1:aff",
    "d1:front//d1:abstract",
    "d1:front/d1:article-meta/funding-group",
    "d1:body//d1:xref",
    "d1:body//d1:table-wrap",
    "d1:body//d1:title",
    "d1:body//d1:funding-source",
    "d1:back/d1:ref-list",
    "d1:back//d1:funding-source",
    "d1:floats-group"
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


  new_name <-
    filename %>%
    str_replace("xml", "txt") %>%
    str_replace("XMLs", "xml_to_txt")

  write(article, new_name)
}



rt_data_pmc_txt <- function(filename_txt) {

  filename_xml <-
    filename_txt %>%
    str_replace("xml_to_txt", "XMLs") %>%
    str_replace("txt", "xml")


  xpath <- c(
    "d1:front/d1:article-meta/d1:article-id[@pub-id-type = 'pmid']",
    "d1:front/d1:article-meta/d1:article-id[@pub-id-type = 'pmc']",
    "d1:front/d1:article-meta/d1:article-id[@pub-id-type = 'pmc-uid']",
    "d1:front/d1:article-meta/d1:article-id[@pub-id-type = 'doi']"
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


  article_xml <- filename_xml %>% read_xml()
  article <- filename_txt %>% read_lines()


  out %<>% purrr::list_modify(!!!map(xpath, ~ .get_text(article_xml, .x, T)))

  if (nchar(out$doi) == 0) {

    out$doi <- "not found"

  }


  # Check relevance
  data <- "\\b[Dd]ata\\b|[Dd]ataset|\\b[Ff]ile\\b|download|[Ss]har|[Aa]vailabl"
  code <- "\\b[Cc]ode\\b|\\b[Ss]cript\\b|GitHub|BitBucket"

  has_data <- stringr::str_detect(article, regex(data, ignore_case = T))
  has_code <- stringr::str_detect(article, regex(code, ignore_case = T))

  out$is_relevant_data <- any(has_data)
  out$is_relevant_code <- any(has_code)


  article_tokens <-
    article %>%
    .tokenize() %>%
    list() %>%
    rlang::set_names(out$doi)


  indicators <- oddpub::open_data_search(article_tokens)
  sentences <- oddpub::open_data_sentences(article_tokens)

  adjudication <- suppressMessages(dplyr::full_join(indicators, sentences))
  out <- tibble::as_tibble(out)

  dplyr::bind_cols(out, adjudication) %>% dplyr::select(-article)
}