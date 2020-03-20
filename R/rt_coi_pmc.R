#' Identify standard metions of COI statements
#'
#' Extract mentions of COI statements that contain standard phrases, such as
#'     "Conflicts of Interest".
#'
#' @param article The text as a vector of strings.
#' @return Index of element with phrase of interest
.which_coi_1 <- function(article, synonyms) {

  a <-
    synonyms$conflict_title %>%
    .encase() %>%
    grep(article, ignore.case = T)

  if (!!length(a)) {

    a %<>% purrr::keep(~ .negate_coi_1(article[.x], synonyms))

  }

  return(a)
}



#' Identify less explicit mentions of COI statements.
#'
#' Extract mentions of COI statements that are less standard, e.g. "We declare
#'     that we have no financial or other conflicts of interest."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_coi_2 <- function(article, synonyms) {

  txt <- synonyms$txt

  conflict_1 <- "no "
  conflict_2 <- "(conflict|competing)"
  conflict_3 <- "interest"

  conflict_regex_1 <- paste0(conflict_1, txt, conflict_2, txt, conflict_3)
  is_conflict_1 <- grep(conflict_regex_1, article, perl = T, ignore.case = T)

  conflict_regex_2 <- paste0(conflict_1, txt, conflict_3, txt, conflict_2)
  is_conflict_2 <- grep(conflict_regex_2, article, perl = T, ignore.case = T)

  return(unique(c(is_conflict_1, is_conflict_2)))
}



#' Identify mentions of financial disclosures
#'
#' Extract mentions of financial disclosures, e.g. "Financial disclosure:
#'     Nothing to disclose.
#'
#' @param article The text as a vector of strings.
#' @return Index of element with phrase of interest
.which_disclosure_1 <- function(article, synonyms) {

  a <- agrep("disclosure", article, ignore.case = T)
  is_financial <- agrepl("financial disclosure", article[a])

  if (any(is_financial)) {

    a %<>% purrr::keep(~ .negate_disclosure_1(article[.x], synonyms))

  } else {

    a %<>% purrr::keep(~ .negate_disclosure_2(article[.x], synonyms))

  }

  return(a)
}



#' Identify less explicit mentions of commercial interest
#'
#' Extract mentions of COI statements that are less standard and mention
#'     commercial interests, e.g. "The authors of this study declare a financial
#'     relationship with GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_commercial_1 <- function(article, synonyms) {

  txt <- synonyms$txt

  commerce_0 <- "([Aa]uthor|[Ee]ditor|[Rr]eviewer|[Ss]tudy|party|has |have |No)"
  commerce_1 <- "(commerically|commercial|financial)"
  commerce_2 <- "(sponsored|financed|funded|interest(|s)|benefit|relationship)"

  commercial_regex_1 <- paste0(commerce_0, txt, commerce_1, txt, commerce_2)
  is_commercial_1 <- grep(commercial_regex_1, article, perl = T)

  commercial_regex_2 <- paste0(commerce_0, txt, commerce_2, txt, commerce_1)
  is_commercial_2 <- grep(commercial_regex_2, article, perl = T)

  return(unique(c(is_commercial_1, is_commercial_2)))
}



#' Identify mentions of receiving benefits
#'
#' Extract mentions of benefits, e.g. "SS has received benefits of commercial
#'     nature from GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_benefit_1 <- function(article, synonyms) {

  txt <- synonyms$txt

  benefit_regex_1 <-
    paste0("received", txt, "benefit", txt, "from", txt, "commercial")

  benefit_regex_2 <-
    paste0("benefit", txt, "received", txt, "from", txt, "commercial")

  benefit_regex_3 <-
    paste0("commercial", txt, "benefit|gain", txt, "received")

  is_benefit_1 <- grep(benefit_regex_1, article, perl = T)
  is_benefit_2 <- grep(benefit_regex_2, article, perl = T)
  is_benefit_3 <- grep(benefit_regex_3, article, perl = T)

  return(unique(c(is_benefit_1, is_benefit_2, is_benefit_3)))
}



#' Identify consultants
#'
#' Identify mentions such as "SS is a consultant for GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_consultant_1 <- function(article, synonyms) {

  txt <- synonyms$txt

  consultant_0 <- "( is a| are)"
  consultant_1 <- "consultant(|s)"
  consultant_2 <- "for"

  consultant_regex <- paste0(consultant_0, txt, consultant_1, txt, consultant_2)
  is_consultant <- grep(consultant_regex, article, perl = T)

  return(is_consultant)
}



#' Identify mentions of brief explanation
#'
#' Extract mentions of COI statements that mention "Brief explanation."
#'
#' @param article The text as a vector of strings.
#' @return Index of element with phrase of interest
.which_brief <- function(article) {

  grep("Brief explanation for [A-Za-z]+:", article, ignore.case = T, fixed = F)

}



#' Identify receipt of grants
#'
#' Identify mentions such as: "SS has received research grants from GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_grants_1 <- function(article) {

  grep("received grants", article, perl = T)

}



#' Identify mentions of paid fees
#'
#' Identify mentions such as: "SS has received fees from GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_fees_1 <- function(article, synonyms) {

  words <- c("received_strict", "stock", "fees")

  fees <-
    synonyms %>%
    magrittr::extract(words[c(2, 3)]) %>%
    unlist() %>%
    .bound() %>%
    .encase()

  received <-
    synonyms %>%
    magrittr::extract(words[1]) %>%
    unlist() %>%
    .bound() %>%
    .encase()

  c(received, fees) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify mentions of consulting
#'
#' Identify mentions such as: "SS consults for GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_consults_1 <- function(article, synonyms) {

  words <- c("consult_all", "speaker")

  consults <-
    synonyms %>%
    magrittr::extract(words) %>%
    unlist() %>%
    .bound() %>%
    .encase()

  of <-
    c("of", "for", "on" , "by", "from", "by", "under", "within") %>%
    .bound(location = "both") %>%
    .encase

  name_consults <- paste("[A-Z]\\s*[A-Z]", consults, of, sep = synonyms$txt)
  consults_name <- paste(consults, "[A-Z]+", sep = synonyms$txt)

  c(name_consults, consults_name) %>%
    .encase() %>%
    grep(article, perl = T)

}



#' Identify mentions of financial connections
#'
#' Identify mentions such as: "SS has a financial relationship with GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_connections_1 <- function(article, synonyms) {

  txt <- synonyms$txt
  words <- c("relationship", "related", "relationship_strict", "related_strict")
  commercial <- "\\bcommercial(|ly)"
  financial <- "\\bfinancial(|y)"

  commercial_relation <-
    synonyms %>%
    magrittr::extract(words[c(1, 2)]) %>%
    unlist %>%
    .bound %>%
    .encase

  financial_relation <-
    synonyms %>%
    magrittr::extract(words[c(3, 4)]) %>%
    unlist %>%
    .bound %>%
    .encase

  commercial_relation <- paste(commercial, commercial_relation, sep = txt)
  financial_relation <- paste(financial, financial_relation, sep = txt)

  c(commercial_relation, financial_relation) %>%
    .encase %>%
    grep(article, perl = T, ignore.case = T)

}



#' Identify declarations of connections
#'
#' Identify mentions such as: "SS declares a relationship with GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_connections_2 <- function(article, synonyms) {

  declare <- "(disclose(|s)|declare(|s)) (|a|an|no)"
  words <- c("disclose", "relationship")

  connection <-
    synonyms %>%
    magrittr::extract(words[2]) %>%
    unlist %>%
    .bound %>%
    .encase

  c(declare, connection) %>%
    paste(collapse = "\\s*") %>%
    grep(article, perl = T)

}



#' Identify mentions of commercial funding
#'
#' Identify mentions such as: "This study was commercially funded."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_commercial_ack_1 <- function(article, synonyms) {

  txt <- synonyms$txt
  words <- c("commercial", "funded", "interests")

  interests <-
    synonyms %>%
    magrittr::extract(words[c(2, 3)]) %>%
    unlist() %>%
    .bound() %>%
    .encase()

  commercial <-
    # synonyms %>%
    # magrittr::extract(words[1]) %>%
    "commercial(|ly)" %>%
    unlist() %>%
    .bound() %>%
    .encase()

  commercial_interests <- paste(interests, commercial, sep = txt)
  interests_commercial <- paste(commercial, interests, sep = txt)
  financialy_sponsored <- paste("financial(|y)", "sponsored", sep = txt)
  sponsored_financialy <- paste("sponsored", "financial(|y)", sep = txt)

  c(commercial_interests,
    interests_commercial,
    financialy_sponsored,
    sponsored_financialy
  ) %>%
    .encase() %>%
    grep(article, perl = T, ignore.case = T)

}



#' Identify mentions of proprietary material
#'
#' Identify mentions such as: "SS has the rights to the presented tool."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_rights_1 <- function(article, synonyms) {

  words <- c("received_strict", "proprietary")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T, ignore.case = T)

}



#' Identify founders
#'
#' Identify mentions such as: "SS is a founding member of GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_founder_1 <- function(article, synonyms) {

  words <- c("founder", "for_of")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T, ignore.case = T)

}



#' Identify advisors
#'
#' Identify mentions such as: "SS is a scientific advisor of GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_advisor_1 <- function(article, synonyms) {

  sci_advisor <- paste("scientific", "advisor", sep = synonyms$txt)
  of <- synonyms$for_of %>% unlist %>% .bound %>% .encase

  c(sci_advisor, of) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, ignore.case = T, perl = T)

}



#' Identify payments
#'
#' Identify mentions such as: "SS was paid by GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_paid_1 <- function(article, synonyms) {

  grep(" paid ", article, ignore.case = T, perl = T)

}



#' Identify board membership
#'
#' Identify mentions such as: "SS is a member of the board for GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_board_1 <- function(article, synonyms) {

  txt <- synonyms$txt

  partake_synonyms <- c("member(|ship)", "serve(|s|d)", "participate(|s|d)")
  partake <- paste0("(", paste(partake_synonyms, collapse = "|"), ") ")

  preps_synonyms <- c("on", "of", "in", "for")
  preps <- paste0("(", paste(preps_synonyms, collapse = "|"), ") ")

  any_board <- paste0(partake, txt, preps, txt, "[Bb]oard")

  grep(any_board, article, perl = T)
}



#' Identify statements of no COI
#'
#' Identify mentions such as: "SS was paid by GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_no_coi_1 <- function(article, synonyms) {

  grep("[Nn]o[a-zA-Z\\s]+conflict", article, perl = T)

}



#' Identify statements that the funders were not involved
#'
#' Identify mentions such as: "SS was paid by GSK."
#'
#' @param article The text as a vector of strings.
#' @return Index of elements with phrase of interest
.which_no_funder_role_1 <- function(article, synonyms) {

  words <- c("is_have", "role")
  funders <- "(The [Ff]unders|Funders)"
  no <- c("no", "not") %>% .bound(location = "both") %>% .encase

  had <-
    synonyms %>%
    magrittr::extract(words[1]) %>%
    unlist %>%
    .bound(location = "both") %>%
    .encase

  role <-
    synonyms %>%
    magrittr::extract(words[2]) %>%
    unlist %>%
    c("involved") %>%
    .bound %>%
    .encase

  in_prep <- "\\bin\\b"

  c(funders, had, no, role, in_prep) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Negate statements that mention conflict but are not COI
#'
#' Keep statements such as "No conficts of interest reported", but do not
#'      keep statements such as "The conflicts of interest are becoming
#'      commoner."
#'
#' @param article The text as a vector of strings.
#' @return A boolean indicating whether a disclosure should be retained.
.negate_coi_1 <- function(article, synonyms) {

  no <- synonyms$no %>% .bound() %>% .encase()
  no_conflict <- paste(no, "conflict|compet", sep = synonyms$txt)

  has_capital_c <- grepl("C(?i)onflict(?-i)|C(?i)ompet(?-i)", article, perl = T)
  has_negation <- grepl(no_conflict, article)
  has_author <- grepl("[Aa]uthor", article, perl = T)
  has_punct <- grepl("interest(|s)[.:;,]", article, ignore.case = T)

  any(c(has_capital_c, has_negation, has_author, has_punct))
}



#' Negate disclosures that are funding statements
#'
#' Keep statements such as "No financial disclosures reported.", but do not
#'      keep statements such as "The authors disclose not funding received."
#'
#' @param article The text as a vector of strings.
#' @return A boolean indicating whether a disclosure should be retained.
.negate_disclosure_1 <- function(article, synonyms) {

  words <- c("no_financial_disclosure", "consult_all", "speaker", "proprietary")

  synonyms %>%
    magrittr::extract(words) %>%
    unlist() %>%
    .encase() %>%
    grepl(article, perl = T)

}



#' Negate disclosures that are neither COI nor funding statements
#'
#' Remove mentions of disclosures such as "Patient Information Disclosure".
#'
#' @param article The text as a vector of strings.
#' @return A boolean indicating whether a disclosure should be retained.
.negate_disclosure_2 <- function(article, synonyms) {

  has_capital_d <- grepl("Disclos|DISCLOS", article)
  has_negation <- grepl(.encase(synonyms$No), article)
  has_author <- grepl("[Aa]uthor[a-zA-Z\\s-]*[dD]isclo", article, perl = T)
  has_punct <- grepl("disclosure(|s)[.:;,]", article, ignore.case = T)
  has_online <- grepl("\\bonline\\b", article, ignore.case = T)

  any(has_capital_d, has_negation, has_author, has_punct, has_online)
}



#' Remove irrelevat mentions of honoraria
#'
#' Remove mentions of honoraria such as "Patients received honoraria".
#'
#' @param article The text as a vector of strings.
#' @return A boolean indicating whether a disclosure should be retained.
.obliterate_honoraria_1 <- function(article, synonyms) {

  honorary_1 <- "([Pp]articipant|[Pp]rovider|[Ss]ubject|[Pp]atient|[Ff]amil)"
  honorary_2 <- "[Hh]onorari(um|a)"
  honorary_regex <- paste0(honorary_1, synonyms$txt, honorary_2)

  gsub(honorary_regex, "", article, perl = T)

}



#' Identify COI titles using XML labels
#'
#' Extract XML titles related to COI statements and all text children.
#'
#' @param article_xml The text as an xml_document.
#' @return The title and its related text as a string.
.get_coi_title_pmc <- function(article_xml, synonyms) {

  b <- character()
  words <- c("conflict_title", "disclosure_coi_title")

  # In case I want to programmatically add these
  # prefix <- "(C(?i)|(?-i)[A-Z](?i)[a-z]+ c)"
  # suffix <- "(?-i)(| \\(COI\\)| \\w+)"

  coi_titles <-
    synonyms %>%
    magrittr::extract(words) %>%
    unlist %>%
    # .title %>%  # If I uncomment, add the "disclosure" synonyms as postfix
    .encase


  # back_xpath <-
  #   c(
  #     "back/ack//*[self::title or self::bold or self::italic]",
  #     "back/fn-group//*[self::title or self::bold or self::italic]",
  #     "back/notes//*[self::title or self::bold or self::italic]"
  #   ) %>%
  #   paste(collapse = " | ")

  # If I had not stripped the d1 namespace:
  # "back//fn-group//*[self::d1:title or self::d1:bold or self::d1:italic]"
  # back_matter <-
  #   article_xml %>%
  #   xml_find_all(back_xpath)

  # TODO: Turn into a function applied to the back, front and body matters
  back_matter <-
    article_xml %>%
    xml_find_all("back/*[not(name()='ref-list')]") %>%
    xml_find_all(".//*[self::title or self::bold or self::italic or self::sup]")

  a <-
    back_matter %>%
    xml_text() %>%
    str_which(regex(coi_titles, ignore_case = T))

  if (!!length(a)) {

    # I can try to use ancestor to avoid the nested if statement in the future
    # ancestor_xpaths <- c(
    #   "./ancestor::fn",
    #   "./ancestor::sec",
    #   "./ancestor::notes",
    #   "./ancestor::author-notes"
    # )
    #
    # ancestor_xpath <- paste(ancestor_xpaths, collapse = " | ")

    a <-
      back_matter %>%
      magrittr::extract(a) %>%
      xml_parent() %>%
      xml_contents()

    if (length(a) == 1) {

      b <-
        a %>%
        xml_parent() %>%
        xml_parent() %>%
        xml_contents() %>%
        xml_text() %>%
        paste(collapse = ": ")

    } else {

      b <-
        a %>%
        xml_text() %>%
        paste(collapse = " ")
    }

    return(b)
  }


  front_matter <-
    article_xml %>%
    xml_find_all("front//fn//*[self::title or self::bold or self::italic or self::sup]")

  a <-
    front_matter %>%
    xml_text() %>%
    str_which(regex(coi_titles, ignore_case = T))

  if (!!length(a)) {

    a <-
      front_matter %>%
      magrittr::extract(a) %>%
      xml_parent() %>%
      xml_contents()

    if (length(a) == 1) {

      b <-
        a %>%
        xml_parent() %>%
        xml_parent() %>%
        xml_contents() %>%
        xml_text() %>%
        paste(collapse = ": ")

    } else {

      b <-
        a %>%
        xml_text() %>%
        paste(collapse = " ")

    }

    return(b)
  }


  coi_titles <-
    synonyms %>%
    magrittr::extract(words) %>%
    unlist %>%
    # .title_strict %>%
    .encase

  body_matter <-
    article_xml %>%
    xml_find_all("body/sec//*[self::title or self::bold or self::italic or self::sup]")

  a <-
    body_matter %>%
    xml_text() %>%
    str_which(regex(coi_titles, ignore_case = T))

  if (!!length(a)) {

    a <-
      body_matter %>%
      magrittr::extract(a) %>%
      xml_parent() %>%
      xml_contents()

    if (length(a) == 1) {

      b <-
        a %>%
        xml_parent() %>%
        xml_parent() %>%
        xml_contents() %>%
        xml_text() %>%
        paste(collapse = ": ")

    } else {

      b <-
        a %>%
        xml_text() %>%
        paste(collapse = " ")
    }
  }

  return(b)
}



#' Identify elements with COI attributes
#'
#' Extract elements with COI attributes from NLM XLMs.
#'
#' @param article_xml The text as an xml_document.
#' @return The title and its related text as a string.
.get_coi_fn_pmc <- function(article_xml) {

  fn_xpaths <- c(
    "back//fn[@fn-type = 'conflict' or @fn-type = 'COI-statement']",
    "front//fn[@fn-type = 'conflict' or @fn-type = 'COI-statement']"
  )

  fn_xpath <- paste(fn_xpaths, collapse = " | ")

  coi_fn <-
    article_xml %>%
    xml_find_all(fn_xpath) %>%
    xml_contents() %>%
    xml_text() %>%
    paste0(collapse = ": ")

  if (nchar(coi_fn) == 0) {

    sec_xpaths <- c(
      "back//sec[@sec-type = 'conflict' or @fn-type = 'COI-statement']",
      "front//sec[@sec-type = 'conflict' or @fn-type = 'COI-statement']"
    )

    sec_xpath <- paste(sec_xpaths, collapse = " | ")

    coi_fn <-
      article_xml %>%
      xml_find_all(sec_xpath) %>%
      purrr::map(function(x) xml_contents(x) %>% xml_text()) %>%
      purrr::map_chr(paste, collapse = ": ") %>%
      paste(collapse = " ")
  }

  return(coi_fn)
}



rt_coi_pmc <- function(filename) {

  index <- integer()
  synonyms <- .create_synonyms()

  xpath <- c(
    "front/article-meta/article-id[@pub-id-type = 'pmid']",
    "front/article-meta/article-id[@pub-id-type = 'pmc']",
    "front/article-meta/article-id[@pub-id-type = 'pmc-uid']",
    "front/article-meta/article-id[@pub-id-type = 'doi']"
  )

  # Way faster than index_any[["reg_title_pmc"]] <- NA
  index_any <- list(
    coi_fn_pmc = NA,
    coi_title_pmc = NA,
    coi_1 = NA,
    coi_2 = NA,
    disclosure_1 = NA,
    commercial_1 = NA,
    benefit_1 = NA,
    consultant_1 = NA,
    grant_1 = NA,
    brief_1 = NA
  )

  index_ack <- list(
    grants_1 = NA,
    fees_1 = NA,
    consults_1 = NA,
    connect_1 = NA,
    connect_2 = NA,
    commercial_ack_1 = NA,
    rights_1 = NA,
    founder_1 = NA,
    advisor_1 = NA,
    paid_1 = NA,
    board_1 = NA,
    no_coi_1 = NA,
    no_funder_role_1 = NA
  )

  out <- list(
    pmid = NA,
    pmcid_pmc = NA,
    pmcid_uid = NA,
    doi = NA,
    is_relevant = NA,
    is_relevant_hi = NA,
    is_relevant_lo = NA,
    is_coi_pred = FALSE,
    coi_text = "",
    is_explicit = NA
  )


  article_xml <- xml2::read_xml(filename) %>% xml2::xml_ns_strip()
  # .xml_preprocess(article_xml)  # 5x faster to obliterate within each section


  # Extract IDs
  out %<>% purrr::list_modify(!!!map(xpath, ~ .get_text(article_xml, .x, T)))


  # Capture coi fn elements
  out$coi_text <- .get_coi_fn_pmc(article_xml)
  index_any$coi_fn_pmc <- nchar(out$coi_text) > 0

  if (index_any$coi_fn_pmc) {

    out$is_relevant <- TRUE
    out$is_coi_pred <- TRUE

    return(tibble::as_tibble(c(out, index_any, index_ack)))
  }


  # Go through titles
  title_txt <- .get_coi_title_pmc(article_xml, synonyms)
  is_title <- !!length(title_txt)

  if (is_title) {

    index_any$coi_title_pmc <- TRUE
    out$coi_text <- title_txt

    out$is_relevant <- TRUE
    out$is_explicit <- TRUE
    out$is_coi_pred <- TRUE

    return(tibble::as_tibble(c(out, index_any, index_ack)))

  }


  # Extract article text into a vector
  ack <- .xml_ack(article_xml)
  body <- .xml_body(article_xml, get_last_two = T)
  footnotes <- .xml_footnotes(article_xml) %>% obliterate_contribs()
  article <- c(footnotes, body, ack)


  # Check relevance
  hi <- "conflict|compet|disclos|declar|\\bcommercial"
  lo <- "fee(|s)\\b|honorari|\\bboard\\b|consult|relation|connection|\\bfinancial|\\b(co|co-)founder|\\bpaid\\b|speaker|\\bemployee|member\\b|funder"

  hi_relevance <- str_detect(article, regex(hi, ignore_case = T))
  lo_relevance <- str_detect(article, regex(lo, ignore_case = T))

  article <- article[(hi_relevance + lo_relevance) > 0]
  # rel_regex <- paste(hi_regex, lo_regex, sep = "|")
  # article %<>% purrr::keep(~ str_detect(.x, regex(rel_regex, ignore_case = T)))

  out$is_relevant_hi <- any(hi_relevance)
  out$is_relevant_lo <- any(lo_relevance)
  out$is_relevant <- with(out, any(c(is_relevant_hi, is_relevant_lo)))

  # Check for relevance
  if (!out$is_relevant) {

    return(tibble::as_tibble(c(out, index_any, index_ack)))

  }


  # Text pre-processing
  article_processed <-
    article %>%
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT', sub = "") %>%   # keep first
    trimws() %>%
    obliterate_fullstop_1() %>%
    obliterate_semicolon_1() %>%  # adds minimal overhead
    obliterate_comma_1() %>%   # adds minimal overhead
    obliterate_apostrophe_1() %>%
    obliterate_punct_1() %>%
    obliterate_line_break_1() %>%
    .obliterate_honoraria_1(synonyms)

  index_any$coi_fn_pmc <- integer()
  index_any$coi_title_pmc <- integer()
  index_any$coi_1 <- .which_coi_1(article_processed, synonyms)
  index_any$coi_2 <- .which_coi_2(article_processed, synonyms)
  index_any$disclosure_1 <- .which_disclosure_1(article_processed, synonyms)
  index_any$commercial_1 <- .which_commercial_1(article_processed, synonyms)
  index_any$benefit_1 <- .which_benefit_1(article_processed, synonyms)
  index_any$consultant_1 <- .which_consultant_1(article_processed, synonyms)
  index_any$grant_1 <- .which_grants_1(article_processed)
  index_any$brief_1 <- .which_brief(article_processed)
  index <- unlist(index_any) %>% unique() %>% sort()


  if (!!length(index)) {

    out$is_explicit <- !!length(unlist(index_any))
    out$is_coi_pred <- !!length(index)
    out$coi_text <- article[index] %>% paste(collapse = " ")

    index_any %<>% purrr::map(function(x) !!length(x))


    if (length(index) == 1) {
      coi_only <- synonyms$conflict_title %>% .encase %>% .title_strict
      is_coi_only <- stringr::str_detect(out$coi_text, coi_only)

      if (is_coi_only & !is.na(article[index + 1])) {

        out$coi_text <- article[c(index, index + 1)] %>% paste(collapse = ": ")

      }
    }

    return(tibble::as_tibble(c(out, index_any, index_ack)))
  }


  # Identify potentially missed signals
  i <- which(article %in% c(ack, footnotes))

  if (!!length(i)) {

    index_ack$grants_1 <- .which_grants_1(article_processed[i])
    index_ack$fees_1 <- .which_fees_1(article_processed[i], synonyms)
    index_ack$consults_1 <- .which_consults_1(article_processed[i], synonyms)
    index_ack$connect_1 <- .which_connections_1(article_processed[i], synonyms)
    index_ack$connect_2 <- .which_connections_2(article_processed[i], synonyms)
    index_ack$commercial_ack_1 <-
      .which_commercial_ack_1(article_processed[i], synonyms)
    index_ack$rights_1 <- .which_rights_1(article_processed[i], synonyms)
    index_ack$founder_1 <- .which_founder_1(article_processed[i], synonyms)
    index_ack$advisor_1 <- .which_advisor_1(article_processed[i], synonyms)
    index_ack$paid_1 <- .which_paid_1(article_processed[i], synonyms)
    index_ack$board_1 <- .which_board_1(article_processed[i], synonyms)
    index_ack$no_coi_1 <- .which_no_coi_1(article_processed[i], synonyms)
    index_ack$no_funder_role_1 <-
      .which_no_funder_role_1(article_processed[i], synonyms)

    index <- i[unlist(index_ack) %>% unique() %>% sort()]
    index_ack %<>% purrr::map(function(x) !!length(x))

  }

  out$is_coi_pred <- !!length(index)
  out$coi_text <- article[index] %>% paste(collapse = " ")

  index_any %<>% purrr::map(function(x) !!length(x))

  if (out$is_coi_pred) {

    out$is_explicit <- FALSE

  }

  return(tibble::as_tibble(c(out, index_any, index_ack)))
}






remains <- function(index, splitted, wanted_disclosure, is_conflict) {

  # If in point form, sections are missed when they do not include the keywords.
  if (length(index) > 1 & any(diff(index) > 1)) {
    if (max(index) - min(index) < 10) {  # Safeguard
      index <- seq(min(index), max(index), 1)
    }
  }

  coi_text <- splitted[index] %>% unlist %>% paste(., collapse = " ")

  # Identify text that may have been  missed because it was in a new line
  if (length(index) == 1) {
    no_stop_words <- gsub(" of ", " ", coi_text,  ignore.case = T)
    if (length(strsplit(no_stop_words, " ")[[1]]) < 4) {
      if (!grepl("no", splitted[index], ignore.case = T)) {
        if (nchar(splitted[index + 1]) == 0) {
          index <- c(index, index + 2)
        } else {
          index <- c(index, index + 1)
        }
        new_str <- gsub("^.+(None.*$)", "\\1", splitted[index[2]])
        if (grepl("^.*\\.$", new_str)) {
          # make sure this is a whole sentence
          coi_text <- paste(coi_text, new_str)
        } else {
          coi_text <- paste(coi_text, new_str, splitted[index[2] + 1])
        }
      }
    }
  }

  # Exclude other mentions of disclosure that are not disclosures of interest
  # e.g. Patient information disclosure
  # If no capital D and no mention of competing/conflicts, or None
  # then false positive
  # I am doing this after the above b/c things like Disclosure/n None. would not be captured otherwise
  # if (length(is_disclosure) > 0 & length(the_conflicts) == 0 & length(wanted_disclosure) == 0) {
  #
  #   # Capital D
  #   is_capital <- grepl("Disclos|DISCLOS", coi_text)
  #   # Mention of conflict/competing
  #   # (this uses only mentions of conflict/competing interest deemed relevant)
  #   is_compconf <- length(the_conflicts) > 0
  #   # Mention negation
  #   is_no_1 <- grepl("None|Nothing|No|Nil", coi_text)
  #   is_no_2 <- grepl("NONE|NOTHING|NO|NIL", coi_text)
  #   is_no <- any(c(is_no_1, is_no_2))
  #   # Punctuation
  #   # (disclose. needed for 0245)
  #   is_punctuation <- grepl("disclosure.{0,1}[.:;,]", coi_text, ignore.case = T)
  #   # Mention of author
  #   is_author <- grepl("[Aa]uthor[a-zA-Z\\s-]*[dD]isclo", coi_text, perl = T)
  #
  #   if (!is_capital & !is_compconf & !is_no & !is_punctuation & !is_author) {
  #     index <- c()
  #     coi_text <- ""
  #   }
  # }

  # Adjudicate presence of COI
  is_coi_pred <- length(index) > 0

  # Remove preceding text that is not relevant
  coi_text <-
    gsub(  # lazy match to stop at first occurrence of a word
      "^.*?(Disclosure.*conflict.*$)|^.*?(Disclosure.*compet.*$)|^.*?(Declaration of Conflict.*$)|^.*?(Declaration of Interest.*$)|^.*?(Potential conflict.{0,1} of interest.*$)|^.*?(Conflict.*$)|^.*?(Competing.*$)",  # disclosure may refer to financial disclosures
      "\\1\\2\\3\\4\\5\\6\\7",
      coi_text,
      perl = T
    )




  # Capture disclosures that do not contain "conflict" but are relevant
  if (grepl("disclosure", coi_text, ignore.case = T)) {
    val <- "^.*conflict.*disclosure.*$|^.*compet.*disclosure.*$"
    if (!grepl(val, coi_text, ignore.case = T, perl = T)) {
      coi_text <- gsub( "^.*?(Disclosure.*$)", "\\1", coi_text, fixed = F)
    }
  }



  # If only None/No/Nothing appear, stop after the fullstop.
  coi_text <-
    gsub(
      "(^.+None\\.).*$|(^.+None disclosed\\.).*$|(^.+None reported\\.).*$|(^.+None declared\\.).*$|(^.+None mentioned\\.).*$|(^.+None aired\\.).*$|(^.+None communicated\\.).*$|(^.+None revealed\\.).*$|(^.+Nothing to declare\\.).*$",
      "\\1\\2\\3\\4\\5\\6\\7\\8\\9",
      coi_text,
      fixed = F, ignore.case = T
    )
  coi_text <-
    gsub(
      "(^.+No\\.).*$|(^.+Nil\\.).*$",
      "\\1\\2",
      coi_text,
      fixed = F # do not ignore case b/c Grant NO. 133234 avoided
    )

  # Correct statements with repeating sentences (e.g. 0036, 0405)
  # 0405 not fixed b/c it contains 3 slightly different versions - hard to fix
  new <- strsplit(coi_text, "\\. {0,1}")[[1]]
  if (length(new) > 1) {
    if (all(duplicated(new)[2:length(new)])) {
      coi_text <- paste0(new[1], ".")
    }
  }


  # If 'The authors declare no competing financial interests' appears, extract.
  # (solves e.g. 0077, 0098)
  # e.g. "no competing interests exist."
  val1 <- "^.*(The author.+no.*competing.+interest.*$)"
  val2 <- "^.*(The author.+no.*conflict.+interest.*$)"
  val3 <- "^.*(None of the author.+no.*competing.+interest.*$)"
  val4 <- "^.*(None of the author.+no.*conflict.+interest.*$)"
  val5 <- "^.*(No conflict.*$)"
  val6 <- "^.*(No competing.*$)"
  val7 <- "^.*[.;] ([a-zA-Z\\s]+no financial disclosures.*$)"
  vals <- paste(val1, val2, val3, val4, val5, val6, val7, sep = "|")
  if (grepl(vals, coi_text, perl = T)) {
    # Make sure that we are not missing the title
    new_n <- str_count(coi_text, "[cC]ompeting|[cC]onflict|[fF]inancial")
    # Identify all statements that are not preceded by a title
    # and extract everything from "The author" onwards
    if (new_n == 1 & (length(c(is_disclosure, is_declare, is_dual)) == 0 | length(wanted_disclosure) > 0)) {
      coi_text <- gsub(vals, "\\1\\2\\3\\4\\5\\6\\7", coi_text, perl = T)
    }

    # Do not extract info after the last such sentence, if such info exists.
    # Only do this if "author" is mentioned once.
    # (e.g. 0089, 0097, 0137, 0231)
    if (str_count(coi_text, "author") < 2) {
      val1 <- "(^.*The author.+no.*competing.+interest.*?\\.) [A-Z].*$"
      val2 <- "(^.*The author.+no.*conflict.+interest.*?\\.) [A-Z].*$"
      val3 <- "(^.*All authors.+no.*conflict.+interest.*?\\.) [A-Z].*$"
      val4 <- "(^.*Both authors.+no.*conflict.+interest.*?\\.) [A-Z].*$"
      val5 <- "(^.*No conflicts of interest.{0,12}?\\.) [A-Z].*$"
      val6 <- "(^.*No conflicting.{0,12} interest.{0,12}?\\.) [A-Z].*$"
      val7 <- "(^.*No competing.{0,12} interest.{0,12}?\\.) [A-Z].*$"
      vals <- paste(val1, val2, val3, val4, val5, val6, val7, sep = "|")
      coi_text <- gsub(vals, "\\1\\2\\3\\4\\5\\6\\7", coi_text)

      val1 <- "(^.*The author.+no.*financial.+disclosure.*?\\.) [A-Z].*$"
      val2 <- "(^.*All authors.+no.*financial.+disclosure.*?\\.) [A-Z].*$"
      val3 <- "(^.*Both authors.+no.*financial.+disclosure.*?\\.) [A-Z].*$"
      val4 <- "(^.*There are no [a-zA-Z\\s] financial.+disclosure.*?\\.) [A-Z].*$"
      vals <- paste(val1, val2, val3, val4, sep = "|")
      coi_text <- gsub(vals, "\\1\\2\\3\\4", coi_text, perl = T)
    }
  }

  coi_text %<>% trimws()

  data.frame(article, pmid, is_coi_pred, coi_text, stringsAsFactors = F)
}