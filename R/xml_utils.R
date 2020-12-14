#' NLM XMLs are split into front, body and back.

#' The front contains the journal-meta (e.g. journal name, publisher, etc.) and
#'     article-meta (e.g. title, authors, affiliations, correspondence, etc.).
#'     The body contains things such
#'     as the title, authors, affiliations, correspondence, etc. The body
#'     contains things such as the abstract,






.xml_metadata <- function(article_xml, as_list = F) {

  # TODO: Consider adding: word count, number of titles, number of sections
  # TODO: Improve author format, e.g.
  # "Omidvar Vahid; +420 - 58 563 4905vahid.omidvar@upol.cz;"
  # "Mohorianu Irina; i.mohorianu@uea.ac.uk;"

  meta <- list()

  # article_xml %>%
  #   xml_find_all(xpath = "//xref") %>%
  #   xml_remove(free = T)

  article_xml %>%
    xml_find_all(xpath = "//label") %>%
    xml_remove(free = T)

  article_xml %>%
    xml_find_all(xpath = "//sup") %>%
    xml_remove(free = T)

  xpath <- c(
    "front/journal-meta//journal-title",
    "front/journal-meta/journal-id[@journal-id-type = 'nlm-ta']",
    "front/journal-meta/journal-id[@journal-id-type = 'iso-abbrev']",
    "front/journal-meta/journal-id[@journal-id-type = 'publisher-id']",
    "front/journal-meta/publisher",
    "front/journal-meta/issn[@pub-type = 'ppub']",
    "front/journal-meta/issn[@pub-type = 'epub']",
    # "front/article-meta/article-id[@pub-id-type = 'pmid']",
    # "front/article-meta/article-id[@pub-id-type = 'pmc']",
    # "front/article-meta/article-id[@pub-id-type = 'pmc-uid']",
    # "front/article-meta/article-id[@pub-id-type = 'doi']",
    "front/article-meta/article-id[@pub-id-type = 'pii']",
    "front/article-meta//subject",
    "front/article-meta//article-title",
    "front/article-meta//aff",
    "front/article-meta//aff//institution",
    "front/article-meta//aff//country",
    "front/article-meta//pub-date[@pub-type = 'epub']",
    "front/article-meta//pub-date[@pub-type = 'epub']/year",
    "front/article-meta//pub-date[@pub-type = 'ppub']",
    "front/article-meta//pub-date[@pub-type = 'ppub']/year",
    "front/article-meta//license"
  )

  var_names <- c(
    "journal",
    "journal_nlm",
    "journal_iso",
    "publisher_id",
    "publisher",
    "issn_ppub",
    "issn_epub",
    # "pmid",
    # "pmcid_pmc",
    # "pmcid_uid",
    # "doi",
    "pii",
    "subject",
    "title",
    "affiliation_all",
    "affiliation_institution",
    "affiliation_country",
    "date_epub",
    "year_epub",
    "date_ppub",
    "year_ppub",
    "license"
  )


  meta <-
    xpath %>%
    lapply(.get_text, article_xml = article_xml, find_first = F) %>%
    set_names(var_names)


  meta[["author"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']/name") %>%
    lapply(function(x) xml_contents(x) %>% xml_text() %>% paste(collapse = " ")) %>%
    paste(collapse = "; ")


  meta[["author_aff_id"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']") %>%
    lapply(function(x) xml_find_all(x, "xref") %>% xml_attr("rid") %>% paste(collapse = ", ")) %>%
    paste(collapse = "; ")


  meta[["affiliation_aff_id"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//aff") %>%
    xml_attr("id") %>%
    paste(collapse = "; ")


  meta[["correspondence"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//author-notes/corresp") %>%
    xml_text() %>%
    paste(collapse = "; ")


  meta[["type"]] <-
    article_xml %>%
    xml_attr("article-type")


  meta[["n_auth"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']") %>%
    length()

  meta[["n_affiliation"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//aff") %>%
    length()

  meta[["n_ref"]] <-
    article_xml %>%
    xml_find_all("//back/ref-list/ref") %>%
    length()

  meta[["n_fig_body"]] <-
    article_xml %>%
    xml_find_all("body//fig") %>%
    length()

  meta[["n_fig_floats"]] <-
    article_xml %>%
    xml_find_all("floats-group/fig") %>%
    length()

  meta[["n_table_body"]] <-
    article_xml %>%
    xml_find_all("body//table-wrap") %>%
    length()

  meta[["n_table_floats"]] <-
    article_xml %>%
    xml_find_all("floats-group/table-wrap") %>%
    length()

  #
  supp <- "//*[self::supplementary-material or self::supplement]"
  meta[["is_supplement"]] <-
    article_xml %>%
    xml_find_all(supp) %>%
    rlang::is_empty() %>%
    magrittr::not()


  meta$pmid %<>% stringr::str_replace_all("; [0-9]+", "")
  meta$title %<>% stringr::str_replace_all("; ", "")
  meta$author_aff_id %<>% gsub(" ;", "", .)
  meta$affiliation_all %<>% stringr::str_replace_all("; ,", ",")
  meta$affiliation_all %<>% stringr::str_replace_all(" ;", "")
  meta$date_epub %<>% stringr::str_replace_all("; ", "-")
  meta$date_epub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)  # fixes 1st
  meta$date_epub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)  # fixes 2nd
  meta$date_ppub %<>% stringr::str_replace_all("; ", "-")
  meta$date_ppub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)
  meta$date_ppub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)

  if (!as_list) {

    meta <-
      meta %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(stringr::str_squish)

  }

  return(meta)
}


# Tiny improvement over all meta-data, so not worth it
.xml_metadata_unique <- function(article_xml, as_list = F) {

  meta <- list()

  article_xml %>%
    xml_find_all(xpath = "//label") %>%
    xml_remove(free = T)

  article_xml %>%
    xml_find_all(xpath = "//sup") %>%
    xml_remove(free = T)

  xpath <- c(
    "front/journal-meta//journal-title",
    "front/journal-meta/journal-id[@journal-id-type = 'nlm-ta']",
    "front/journal-meta/journal-id[@journal-id-type = 'iso-abbrev']",
    "front/journal-meta/journal-id[@journal-id-type = 'publisher-id']",
    "front/journal-meta/publisher",
    "front/journal-meta/issn[@pub-type = 'ppub']",
    "front/journal-meta/issn[@pub-type = 'epub']",
    "front/article-meta/article-id[@pub-id-type = 'pii']",
    "front/article-meta//subject",
    "front/article-meta//article-title",
    "front/article-meta//aff",
    "front/article-meta//aff//institution",
    "front/article-meta//aff//country",
    "front/article-meta//pub-date[@pub-type = 'epub']",
    "front/article-meta//pub-date[@pub-type = 'epub']/year",
    "front/article-meta//pub-date[@pub-type = 'ppub']",
    "front/article-meta//pub-date[@pub-type = 'ppub']/year",
    "front/article-meta//license"
  )

  var_names <- c(
    "journal",
    "journal_nlm",
    "journal_iso",
    "publisher_id",
    "publisher",
    "issn_ppub",
    "issn_epub",
    "pii",
    "subject",
    "title",
    "affiliation_all",
    "affiliation_institution",
    "affiliation_country",
    "date_epub",
    "year_epub",
    "date_ppub",
    "year_ppub",
    "license"
  )


  meta <-
    xpath %>%
    lapply(.get_text, article_xml = article_xml, find_first = F) %>%
    set_names(var_names)


  meta[["author"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']/name") %>%
    lapply(function(x) xml_contents(x) %>% xml_text() %>% paste(collapse = " ")) %>%
    paste(collapse = "; ")


  meta[["author_aff_id"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']") %>%
    lapply(function(x) xml_find_all(x, "xref") %>% xml_attr("rid") %>% paste(collapse = ", ")) %>%
    paste(collapse = "; ")


  meta[["affiliation_aff_id"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//aff") %>%
    xml_attr("id") %>%
    paste(collapse = "; ")


  meta[["correspondence"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//author-notes/corresp") %>%
    xml_text() %>%
    paste(collapse = "; ")


  meta[["n_auth"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']") %>%
    length()

  meta[["n_affiliation"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//aff") %>%
    length()

  meta[["n_ref"]] <-
    article_xml %>%
    xml_find_all("//back/ref-list/ref") %>%
    length()

  meta[["n_fig_body"]] <-
    article_xml %>%
    xml_find_all("body//fig") %>%
    length()

  meta[["n_fig_floats"]] <-
    article_xml %>%
    xml_find_all("floats-group/fig") %>%
    length()

  meta[["n_table_body"]] <-
    article_xml %>%
    xml_find_all("body//table-wrap") %>%
    length()

  meta[["n_table_floats"]] <-
    article_xml %>%
    xml_find_all("floats-group/table-wrap") %>%
    length()

  #
  supp <- "//*[self::supplementary-material or self::supplement]"
  meta[["is_supplement"]] <-
    article_xml %>%
    xml_find_all(supp) %>%
    rlang::is_empty() %>%
    magrittr::not()


  meta$title %<>% stringr::str_replace_all("; ", "")
  meta$author_aff_id %<>% gsub(" ;", "", .)
  meta$affiliation_all %<>% stringr::str_replace_all("; ,", ",")
  meta$affiliation_all %<>% stringr::str_replace_all(" ;", "")
  meta$date_epub %<>% stringr::str_replace_all("; ", "-")
  meta$date_epub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)  # fixes 1st
  meta$date_epub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)  # fixes 2nd
  meta$date_ppub %<>% stringr::str_replace_all("; ", "-")
  meta$date_ppub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)
  meta$date_ppub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)

  if (!as_list) {

    meta <-
      meta %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(stringr::str_squish)

  }

  return(meta)
}


.xml_metadata_lean <- function(article_xml, as_list = F) {

  meta <- list()

  xpath <- c(
    "front/journal-meta//journal-title",
    "front/journal-meta/publisher",
    "front/article-meta//aff//institution",
    "front/article-meta//aff//country",
    "front/article-meta//pub-date[@pub-type = 'epub']/year",
    "front/article-meta//pub-date[@pub-type = 'ppub']/year"
  )

  var_names <- c(
    "journal",
    "publisher",
    "affiliation_institution",
    "affiliation_country",
    "year_epub",
    "year_ppub"
  )


  meta <-
    xpath %>%
    lapply(.get_text, article_xml = article_xml, find_first = F) %>%
    rlang::set_names(var_names)


  if (!as_list) {

    meta <-
      meta %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(stringr::str_squish)

  }

  return(meta)
}


# The complement of metadata_lean
.xml_metadata_c <- function(article_xml, as_list = F) {

  # TODO: Consider adding: word count, number of titles, number of sections
  # TODO: Improve author format, e.g.
  # "Omidvar Vahid; +420 - 58 563 4905vahid.omidvar@upol.cz;"
  # "Mohorianu Irina; i.mohorianu@uea.ac.uk;"

  meta <- list()

  # article_xml %>%
  #   xml_find_all(xpath = "//xref") %>%
  #   xml_remove(free = T)

  article_xml %>%
    xml_find_all(xpath = "//label") %>%
    xml_remove(free = T)

  article_xml %>%
    xml_find_all(xpath = "//sup") %>%
    xml_remove(free = T)

  xpath <- c(
    "front/article-meta/article-id[@pub-id-type = 'pmid']",
    "front/article-meta/article-id[@pub-id-type = 'pmc']",
    "front/article-meta/article-id[@pub-id-type = 'doi']",
    "front/article-meta/article-id[@pub-id-type = 'pii']",
    "front/article-meta//pub-date[@pub-type = 'epub']",
    "front/article-meta//pub-date[@pub-type = 'ppub']",
    "front/journal-meta/journal-id[@journal-id-type = 'nlm-ta']",
    "front/journal-meta/journal-id[@journal-id-type = 'iso-abbrev']",
    "front/journal-meta/journal-id[@journal-id-type = 'publisher-id']",
    "front/journal-meta/issn[@pub-type = 'epub']",
    "front/journal-meta/issn[@pub-type = 'ppub']",
    "front/article-meta//aff",
    "front/article-meta//article-title",
    "front/article-meta//subject",
    "front/article-meta//license"
  )

  var_names <- c(
    "pmid",
    "pmcid_pmc",
    "doi",
    "pii",
    "date_epub",
    "date_ppub",
    "journal_nlm",
    "journal_iso",
    "publisher_id",
    "issn_ppub",
    "issn_epub",
    "affiliation_all",
    "title",
    "subject",
    "license"
  )


  meta <-
    xpath %>%
    lapply(.get_text, article_xml = article_xml, find_first = F) %>%
    set_names(var_names)


  meta[["author"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']/name") %>%
    lapply(function(x) xml_contents(x) %>% xml_text() %>% paste(collapse = " ")) %>%
    paste(collapse = "; ")


  meta[["author_aff_id"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']") %>%
    lapply(function(x) xml_find_all(x, "xref") %>% xml_attr("rid") %>% paste(collapse = ", ")) %>%
    paste(collapse = "; ")


  meta[["affiliation_aff_id"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//aff") %>%
    xml_attr("id") %>%
    paste(collapse = "; ")


  meta[["correspondence"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//author-notes/corresp") %>%
    xml_text() %>%
    paste(collapse = "; ")


  meta[["n_auth"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']") %>%
    length()

  meta[["n_affiliation"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//aff") %>%
    length()

  meta[["n_ref"]] <-
    article_xml %>%
    xml_find_all("//back/ref-list/ref") %>%
    length()

  meta[["n_fig_body"]] <-
    article_xml %>%
    xml_find_all("body//fig") %>%
    length()

  meta[["n_fig_floats"]] <-
    article_xml %>%
    xml_find_all("floats-group/fig") %>%
    length()

  meta[["n_table_body"]] <-
    article_xml %>%
    xml_find_all("body//table-wrap") %>%
    length()

  meta[["n_table_floats"]] <-
    article_xml %>%
    xml_find_all("floats-group/table-wrap") %>%
    length()

  #
  supp <- "//*[self::supplementary-material or self::supplement]"
  meta[["is_supplement"]] <-
    article_xml %>%
    xml_find_all(supp) %>%
    rlang::is_empty() %>%
    magrittr::not()


  meta$pmid %<>% stringr::str_replace_all("; [0-9]+", "")
  meta$title %<>% stringr::str_replace_all("; ", "")
  meta$author_aff_id %<>% gsub(" ;", "", .)
  meta$affiliation_all %<>% stringr::str_replace_all("; ,", ",")
  meta$affiliation_all %<>% stringr::str_replace_all(" ;", "")
  meta$date_epub %<>% stringr::str_replace_all("; ", "-")
  meta$date_epub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)  # fixes 1st
  meta$date_epub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)  # fixes 2nd
  meta$date_ppub %<>% stringr::str_replace_all("; ", "-")
  meta$date_ppub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)
  meta$date_ppub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)

  if (!as_list) {

    meta <-
      meta %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(stringr::str_squish)

  }

  return(meta)
}


# Use this when retrieving all indicators and meta-data at the same time
.xml_metadata_all <- function(article_xml, as_list = F) {

  # TODO: Consider adding: word count, number of titles, number of sections
  # TODO: Improve author format, e.g.
  # "Omidvar Vahid; +420 - 58 563 4905vahid.omidvar@upol.cz;"
  # "Mohorianu Irina; i.mohorianu@uea.ac.uk;"

  meta <- list()

  # article_xml %>%
  #   xml_find_all(xpath = "//xref") %>%
  #   xml_remove(free = T)

  article_xml %>%
    xml_find_all(xpath = "//label") %>%
    xml_remove(free = T)

  article_xml %>%
    xml_find_all(xpath = "//sup") %>%
    xml_remove(free = T)

  xpath <- c(
    "front/journal-meta//journal-title",
    "front/journal-meta/journal-id[@journal-id-type = 'nlm-ta']",
    "front/journal-meta/journal-id[@journal-id-type = 'iso-abbrev']",
    "front/journal-meta/journal-id[@journal-id-type = 'publisher-id']",
    "front/journal-meta/publisher",
    "front/journal-meta/issn[@pub-type = 'ppub']",
    "front/journal-meta/issn[@pub-type = 'epub']",
    "front/article-meta/article-id[@pub-id-type = 'pii']",
    "front/article-meta//subject",
    "front/article-meta//article-title",
    "front/article-meta//aff",
    "front/article-meta//aff//institution",
    "front/article-meta//aff//country",
    "front/article-meta//pub-date[@pub-type = 'epub']",
    "front/article-meta//pub-date[@pub-type = 'epub']/year",
    "front/article-meta//pub-date[@pub-type = 'ppub']",
    "front/article-meta//pub-date[@pub-type = 'ppub']/year",
    "front/article-meta//license"
  )

  var_names <- c(
    "journal",
    "journal_nlm",
    "journal_iso",
    "publisher_id",
    "publisher",
    "issn_ppub",
    "issn_epub",
    "pii",
    "subject",
    "title",
    "affiliation_all",
    "affiliation_institution",
    "affiliation_country",
    "date_epub",
    "year_epub",
    "date_ppub",
    "year_ppub",
    "license"
  )


  meta <-
    xpath %>%
    lapply(.get_text, article_xml = article_xml, find_first = F) %>%
    set_names(var_names)


  meta[["author"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']/name") %>%
    lapply(function(x) xml_contents(x) %>% xml_text() %>% paste(collapse = " ")) %>%
    paste(collapse = "; ")


  meta[["author_aff_id"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']") %>%
    lapply(function(x) xml_find_all(x, "xref") %>% xml_attr("rid") %>% paste(collapse = ", ")) %>%
    paste(collapse = "; ")


  meta[["affiliation_aff_id"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//aff") %>%
    xml_attr("id") %>%
    paste(collapse = "; ")


  meta[["correspondence"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//author-notes/corresp") %>%
    xml_text() %>%
    paste(collapse = "; ")


  meta[["n_auth"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']") %>%
    length()

  meta[["n_affiliation"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//aff") %>%
    length()

  meta[["n_ref"]] <-
    article_xml %>%
    xml_find_all("//back/ref-list/ref") %>%
    length()

  meta[["n_fig_body"]] <-
    article_xml %>%
    xml_find_all("body//fig") %>%
    length()

  meta[["n_fig_floats"]] <-
    article_xml %>%
    xml_find_all("floats-group/fig") %>%
    length()

  meta[["n_table_body"]] <-
    article_xml %>%
    xml_find_all("body//table-wrap") %>%
    length()

  meta[["n_table_floats"]] <-
    article_xml %>%
    xml_find_all("floats-group/table-wrap") %>%
    length()

  #
  supp <- "//*[self::supplementary-material or self::supplement]"
  meta[["is_supplement"]] <-
    article_xml %>%
    xml_find_all(supp) %>%
    rlang::is_empty() %>%
    magrittr::not()


  meta$title %<>% stringr::str_replace_all("; ", "")
  meta$author_aff_id %<>% gsub(" ;", "", .)
  meta$affiliation_all %<>% stringr::str_replace_all("; ,", ",")
  meta$affiliation_all %<>% stringr::str_replace_all(" ;", "")
  meta$date_epub %<>% stringr::str_replace_all("; ", "-")
  meta$date_epub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)  # fixes 1st
  meta$date_epub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)  # fixes 2nd
  meta$date_ppub %<>% stringr::str_replace_all("; ", "-")
  meta$date_ppub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)
  meta$date_ppub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)

  if (!as_list) {

    meta <-
      meta %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(stringr::str_squish)

  }

  return(meta)
}



# Tiny improvement over all meta-data, so not worth it
.xml_metadata_unique <- function(article_xml, as_list = F) {

  meta <- list()

  article_xml %>%
    xml_find_all(xpath = "//label") %>%
    xml_remove(free = T)

  article_xml %>%
    xml_find_all(xpath = "//sup") %>%
    xml_remove(free = T)

  xpath <- c(
    "front/journal-meta//journal-title",
    "front/journal-meta/journal-id[@journal-id-type = 'nlm-ta']",
    "front/journal-meta/journal-id[@journal-id-type = 'iso-abbrev']",
    "front/journal-meta/journal-id[@journal-id-type = 'publisher-id']",
    "front/journal-meta/publisher",
    "front/journal-meta/issn[@pub-type = 'ppub']",
    "front/journal-meta/issn[@pub-type = 'epub']",
    "front/article-meta/article-id[@pub-id-type = 'pii']",
    "front/article-meta//subject",
    "front/article-meta//article-title",
    "front/article-meta//aff",
    "front/article-meta//aff//institution",
    "front/article-meta//aff//country",
    "front/article-meta//pub-date[@pub-type = 'epub']",
    "front/article-meta//pub-date[@pub-type = 'epub']/year",
    "front/article-meta//pub-date[@pub-type = 'ppub']",
    "front/article-meta//pub-date[@pub-type = 'ppub']/year",
    "front/article-meta//license"
  )

  var_names <- c(
    "journal",
    "journal_nlm",
    "journal_iso",
    "publisher_id",
    "publisher",
    "issn_ppub",
    "issn_epub",
    "pii",
    "subject",
    "title",
    "affiliation_all",
    "affiliation_institution",
    "affiliation_country",
    "date_epub",
    "year_epub",
    "date_ppub",
    "year_ppub",
    "license"
  )


  meta <-
    xpath %>%
    lapply(.get_text, article_xml = article_xml, find_first = F) %>%
    set_names(var_names)


  meta[["author"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']/name") %>%
    lapply(function(x) xml_contents(x) %>% xml_text() %>% paste(collapse = " ")) %>%
    paste(collapse = "; ")


  meta[["author_aff_id"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']") %>%
    lapply(function(x) xml_find_all(x, "xref") %>% xml_attr("rid") %>% paste(collapse = ", ")) %>%
    paste(collapse = "; ")


  meta[["affiliation_aff_id"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//aff") %>%
    xml_attr("id") %>%
    paste(collapse = "; ")


  meta[["correspondence"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//author-notes/corresp") %>%
    xml_text() %>%
    paste(collapse = "; ")


  meta[["n_auth"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//contrib[@contrib-type = 'author']") %>%
    length()

  meta[["n_affiliation"]] <-
    article_xml %>%
    xml_find_all("front/article-meta//aff") %>%
    length()

  meta[["n_ref"]] <-
    article_xml %>%
    xml_find_all("//back/ref-list/ref") %>%
    length()

  meta[["n_fig_body"]] <-
    article_xml %>%
    xml_find_all("body//fig") %>%
    length()

  meta[["n_fig_floats"]] <-
    article_xml %>%
    xml_find_all("floats-group/fig") %>%
    length()

  meta[["n_table_body"]] <-
    article_xml %>%
    xml_find_all("body//table-wrap") %>%
    length()

  meta[["n_table_floats"]] <-
    article_xml %>%
    xml_find_all("floats-group/table-wrap") %>%
    length()

  #
  supp <- "//*[self::supplementary-material or self::supplement]"
  meta[["is_supplement"]] <-
    article_xml %>%
    xml_find_all(supp) %>%
    rlang::is_empty() %>%
    magrittr::not()


  meta$title %<>% stringr::str_replace_all("; ", "")
  meta$author_aff_id %<>% gsub(" ;", "", .)
  meta$affiliation_all %<>% stringr::str_replace_all("; ,", ",")
  meta$affiliation_all %<>% stringr::str_replace_all(" ;", "")
  meta$date_epub %<>% stringr::str_replace_all("; ", "-")
  meta$date_epub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)  # fixes 1st
  meta$date_epub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)  # fixes 2nd
  meta$date_ppub %<>% stringr::str_replace_all("; ", "-")
  meta$date_ppub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)
  meta$date_ppub %<>% gsub("(^|-)([0-9])(-.*)$", "\\10\\2\\3", .)

  if (!as_list) {

    meta <-
      meta %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(stringr::str_squish)

  }

  return(meta)
}




#' @param article_xml The article of interest as an xml_document
.xml_body <- function(article_xml,
                      get_last_two = T,
                      remove_refs = T,
                      remove_tables = T,
                      remove_titles = F) {

  # Removes references to citations, tables, figures and supplements
  if (remove_refs) {

    article_xml %>%
      xml_find_all(xpath = "body//xref") %>%
      xml_remove(free = T)

  }

  # Remove tables
  if (remove_tables) {

    article_xml %>%
      xml_find_all(xpath = "body//table-wrap") %>%
      xml_remove(free = T)

  }

  # Remove titles
  if (remove_titles) {

    article_xml %>%
      xml_find_all(xpath = "body//title") %>%
      xml_remove(free = T)

  }

  if (get_last_two) {

    article_txt <-
      article_xml %>%
      xml_find_all(xpath = "body//sec") %>%  # //sec b/c some have /sec/sec
      tail(2) %>%
      xml_contents() %>%
      xml_text()

    if (!length(article_txt)) {

      article_txt <-
        article_xml %>%
        xml_find_all(xpath = "body/p") %>%
        tail(2) %>%
        xml_contents() %>%
        xml_text()

    }

    return(article_txt)

    # Code to extract text only
    # article_xml %>%
    #   xml_find_all(xpath = "body/sec") %>%
    #   tail(1) %>%
    #   xml_contents() %>%
    #   xml_find_all("./text()") %>%
    #   ml_text() %>%
    #   paste(collapse = "")
  }

  article_txt <-
    article_xml %>%
    xml_find_all(xpath = "body/sec") %>%
    xml_contents() %>%
    xml_text()

  if (!length(article_txt)) {

    article_txt <-
      article_xml %>%
      xml_find_all(xpath = "body/p") %>%
      xml_contents() %>%
      xml_text()

  }

  return(article_txt)
}



.xml_methods <- function(article) {

  # Systematic reviews also have methods and are not consistently marked as such
  # is_research <-
  #   article %>%
  #   xml_find_first("//article[@article-type = 'research-article']") %>%
  #   rlang::is_empty()
  #
  # # Stop if not research
  # if (!is_research) {
  #
  #   return(character())
  #
  # }


  # TODO Misses methods sections such as "Experimental section"
  # Use the same approach as in title_pmc to better capture what is needed

  # "body/sec[@sec-type = 'materials|methods' or @sec-type = 'methods']"
  xpath_methods <- "body/sec[contains(@sec-type, 'methods')]"
  xpath_a <- "body/sec[title/"
  xpath_b <- "text()[contains(translate(., 'ETHOD', 'ethod'), 'ethod')]]"

  # Remove labels
  article %>%
    xml_find_all(xpath = "//label") %>%
    xml_remove(free = T)


  methods <-
    article %>%
    xml_find_all(xpath = xpath_methods)


  if (!!length(methods)) {

    a <-
      methods %>%
      xml_find_all(xpath = ".//p") %>%
      {magrittr::extract(., 1:min(2, length(.)))} %>%
      xml_text()

    return(a)

  }

  article %>%
    xml_find_all(xpath = paste0(xpath_a, xpath_b)) %>%
    xml_find_all(xpath = ".//p") %>%
    {magrittr::extract(., 1:min(2, length(.)))} %>%
    xml_text()

}







.xml_methods <- function(article_xml, with_refs = T) {

  # "body/sec[@sec-type = 'materials|methods' or @sec-type = 'methods']"
  xpath_methods <- "body/sec[contains(@sec-type, 'methods')]"
  xpath_a <- "body/sec[title/"
  xpath_b <- "text()[contains(translate(., 'ETHOD', 'ethod'), 'ethod')]]"

  # Remove labels
  # article_xml %>%
  #   xml_find_all(xpath = "//label") %>%
  #   xml_remove(free = T)

  methods <-
    article_xml %>%
    xml_find_all(xpath = xpath_methods)


  if (!!length(methods)) {

    if (with_refs) {

      methods %>%
        xml_find_all(".//xref") %>%
        xml_set_text("REFFF")

    }

    methods_txt <-
      methods %>%
      xml_find_all(xpath = ".//p") %>%
      # Not necessary b/c I only keep phrases with "regist" etc. in them anyway
      # {magrittr::extract(., 1:min(5, length(.)))} %>%
      xml_text()

    return(methods_txt)

  }

  methods <-
    article_xml %>%
    xml_find_all(xpath = paste0(xpath_a, xpath_b))

  if (with_refs) {

    methods %>%
      xml_find_all(".//xref") %>%
      xml_set_text("REFFF")

  }

  methods %>%
    xml_find_all(xpath = ".//p") %>%
    # Not necessary b/c I only keep phrases with "regist" etc. in them anyway
    # {magrittr::extract(., 1:min(5, length(.)))} %>%  # not necessary
    xml_text()

}


.xml_abstract <- function(article_xml) {

  # I am explicitly defining the path to avoid searching the whole tree
  xpath_abstract <- "front/article-meta/abstract"
  xpath_sec <- "front/article-meta/abstract/sec"

  abstract <- xml_find_all(article_xml, xpath_sec)

  if (!!length(abstract)) {

    a <-
      abstract %>%
      purrr::map(function(x) xml_contents(x) %>% xml_text()) %>%
      purrr::map_chr(paste, collapse = ": ")

    return(a)

  } else {

    a <-
      article_xml %>%
      xml_find_all(xpath_abstract) %>%
      xml_contents() %>%   # handles documents such as 00021-PMID30459190 better
      xml_text()

    return(a)
  }
}



.xml_ack <- function(article_xml) {

  # Look for the appropriately named element
  ack <-
    article_xml %>%
    xml_find_all("back/ack") %>%
    xml_contents() %>%
    xml_text()

  # Look for the title in the back matter if the element is not found
  if (!length(ack)) {

    xpath_a <- "back/sec[title/"
    xpath_b <- "text()[contains(translate(., 'ACKNOW', 'Acknow'), 'Acknow')]]"

    ack <-
      article_xml %>%
      xml_find_all(paste0(xpath_a, xpath_b)) %>%
      xml_contents() %>%
      xml_text()
  }

  # Look for the title in the body matter if the element is not found
  if (!length(ack)) {

    xpath_a <- "body/sec[title/"
    xpath_b <- "text()[contains(translate(., 'ACKNOW', 'Acknow'), 'Acknow')]]"

    ack <-
      article_xml %>%
      xml_find_all(paste0(xpath_a, xpath_b)) %>%
      xml_contents() %>%
      xml_text()
  }

  return(ack)
}


.xml_suppl <- function(article_xml) {

  article_xml %>%
    xml_find_all("//*[self::supplementary-material or self::supplement]") %>%
    xml_contents() %>%
    xml_text()

}



.xml_preprocess <- function(article_xml,
                            remove_refs = F,
                            modify_refs = T,
                            remove_tables = T,
                            remove_labels = F,
                            remove_titles = F) {

  # Removes references to citations, tables, figures and supplements
  if (remove_refs) {

    article_xml %>%
      xml_find_all(xpath = "//xref") %>%
      xml_remove(free = T)

  }


  # Modify references
  if (modify_refs) {

    article_xml %>%
      xml_find_all(xpath = "//xref") %>%
      xml_set_text("REFFF")

  }


  # Removes tables
  if (remove_tables) {

    article_xml %>%
      xml_find_all(xpath = "//table-wrap") %>%
      xml_remove(free = T)

  }

  # Removes labels
  if (remove_labels) {

    article_xml %>%
      xml_find_all(xpath = "//label") %>%
      xml_remove(free = T)

    article_xml %>%
      xml_find_all(xpath = "//sup") %>%
      xml_remove(free = T)

  }

  # Removes titles
  if (remove_titles) {

    article_xml %>%
      xml_find_all(xpath = "body//title") %>%
      xml_remove(free = T)

  }
}



.xml_footnotes <- function(article_xml, remove_labels = F, all = F) {

  # Remove labels if not already removed by a previous function
  if (remove_labels) {

    article_xml %>%
      xml_find_all(xpath = "//label") %>%
      xml_remove(free = T)

  }

  if (all) {

    xpath <- "front//*[self::fn or self::notes or self::author-notes or self::custom-meta-group]"

    first_page <-
      article_xml %>%
      xml_find_all(xpath) %>%
      purrr::map(function(x) xml_contents(x) %>% xml_text()) %>%
      purrr::map_chr(paste, collapse = ": ")

  } else {

    first_page <-
      article_xml %>%
      xml_find_all("front//fn") %>%  # (e.g. article-meta/author-notes, notes)
      xml_contents() %>%
      xml_text()

    # Get the //fn from any element with name that contains "note" - x5 slower
    # first_page <-
    #   article_xml %>%
    #   xml_find_all("//*[contains(name(),'note')]") %>%
    #   xml_find_all(".//fn") %>%
    #   xml_contents() %>%
    #   xml_text()
  }

  last_page <-
    article_xml %>%
    xml_find_all("back//*[self::fn or self::notes]") %>%
    purrr::map(function(x) xml_contents(x) %>% xml_text()) %>%
    purrr::map_chr(paste, collapse = ": ")

  if (!length(last_page)) {

    xpath_a <- "back/sec[title/text()[not(contains("
    xpath_b <- "translate(., 'ACKNOW', 'Acknow'), 'Acknow'))]]"

    last_page <-
      article_xml %>%
      xml_find_all(paste0(xpath_a, xpath_b)) %>%
      purrr::map(function(x) xml_contents(x) %>% xml_text()) %>%
      purrr::map_chr(paste, collapse = ": ")

  }

  c(first_page, last_page)
}



.node_exists <- function(xml_doc, node){

  xpath <- paste0("//", node)
  nodeset <- xml_doc %>% xml_find_all(xpath = xpath)
  return(!!length(nodeset))

}



#' Get the desired text from the xml_document
#'
#' Returns the text desired according to xpath.
#'
#' @param xpath The XPath as a character, e.g. "id_info/nct_id"
#' @param xml_doc An xml_document from `xml2::read_xml`
#' @return The desired text as a character; if not found, then `character()`
.get_text <- function(article_xml, xpath, find_first) {

  if (find_first) {

    article_xml %>%
      xml2::xml_find_first(xpath) %>%
      xml2::xml_contents() %>%
      xml2::xml_text() %>%
      paste(collapse = "; ")

  } else {

    article_xml %>%
      xml2::xml_find_all(xpath) %>%
      xml2::xml_contents() %>%
      xml2::xml_text() %>%
      paste(collapse = "; ")

  }

}