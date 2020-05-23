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
.get_coi_pmc_title <- function(article_xml, synonyms) {

  b <- ""
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
.get_coi_pmc_fn <- function(article_xml, remove_ns = T) {

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



.is_relevant_coi <- function(article) {

  hi_synonyms <- c(
    "conflict",
    "compet",
    "disclos",
    "declar",
    "\\commercial"
  )

  lo_synonyms <- c(
    "fee(|s)\\b",
    "honorari",
    "\\bboard\\b",
    "consult",
    "relation",
    "connection",
    "\\bfinancial",
    "\\b(co|co-)founder",
    "\\bpaid\\b",
    "speaker",
    "\\bemployee",
    "member\\b",
    "funder"
  )

  hi_regex <- paste(hi_synonyms, collapse = "|")
  lo_regex <- paste(lo_synonyms, collapse = "|")


  hi_relevance <-
    article %>%
    stringr::str_detect(stringr::regex(hi_regex, ignore_case = T))

  lo_relevance <-
    article %>%
    stringr::str_detect(stringr::regex(lo_regex, ignore_case = T))


  is_relevant_hi = any(hi_relevance)
  is_relevant_lo = any(lo_relevance)

  list(
    is_relevant_coi = any(is_relevant_hi, is_relevant_lo),
    is_relevant_coi_hi = is_relevant_hi,
    is_relevant_coi_lo = is_relevant_lo,
    index = hi_relevance | lo_relevance
  )
}


rt_coi_pmc <- function(filename, remove_ns = F) {

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
    grants_1 = NA,
    brief_1 = NA
  )

  index_ack <- list(
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
  # .xml_preprocess(article_xml)  # 5x faster to obliterate within each section


  # Extract IDs
  out %<>% purrr::list_modify(!!!map(xpath, ~ .get_text(article_xml, .x, T)))


  # Capture coi fn elements
  out$coi_text <- .get_coi_pmc_fn(article_xml)
  index_any$coi_fn_pmc <- nchar(out$coi_text) > 0

  if (index_any$coi_fn_pmc) {

    out$is_relevant <- TRUE
    out$is_coi_pred <- TRUE

    return(tibble::as_tibble(c(out, index_any, index_ack)))
  }


  # Go through titles
  title_txt <- .get_coi_pmc_title(article_xml, synonyms)
  is_title <- nchar(title_txt) > 0

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
  index_any$grants_1 <- .which_grants_1(article_processed)
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



.rt_coi_pmc <- function(article_ls, pmc_coi_ls, dict) {

  index <- integer()

  # Way faster than index_any[["reg_title_pmc"]] <- NA
  index_any <- list(
    coi_1 = NA,
    coi_2 = NA,
    coi_disclosure_1 = NA,
    commercial_1 = NA,
    benefit_1 = NA,
    consultant_1 = NA,
    grants_1 = NA,
    brief_1 = NA
  )

  index_ack <- list(
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

  relevance_ls <- list(
    is_relevant_coi = NA,
    is_relevant_coi_hi = NA,
    is_relevant_coi_lo = NA
  )

  out <- list(
    is_coi_pred = FALSE,
    coi_text = "",
    is_explicit_coi = NA
  )


  if (pmc_coi_ls$is_coi_pred) {

    out$is_coi_pred <- TRUE
    out$coi_text <- pmc_coi_ls$coi_text

    relevance_ls$is_relevant_coi <- TRUE
    relevance_ls$is_relevant_coi_hi <- TRUE

    return(c(relevance_ls, out, index_any, index_ack))

  }

  # TODO Consider adding unique
  article <-
    article_ls[c("ack", "body", "footnotes")] %>%
    unlist()
    # unique()


  relevance_ls <- .is_relevant_coi(article)

  if (!relevance_ls$is_relevant_coi) {

    return(c(relevance_ls[-4], out, index_any, index_ack))

  }


  article %<>% purrr::keep(relevance_ls$index)
  relevance_ls$index <- NULL

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
    .obliterate_honoraria_1(dict)


  # No change in speed by recreating the list
  index_any <- list(
    coi_1 = .which_coi_1(article_processed, dict),
    coi_2 = .which_coi_2(article_processed, dict),
    coi_disclosure_1 = .which_disclosure_1(article_processed, dict),
    commercial_1 = .which_commercial_1(article_processed, dict),
    benefit_1 = .which_benefit_1(article_processed, dict),
    consultant_1 = .which_consultant_1(article_processed, dict),
    grants_1 = .which_grants_1(article_processed),
    brief_1 = .which_brief(article_processed)
  )

  index <- unlist(index_any) %>% unique() %>% sort()


  if (!!length(index)) {

    out$is_explicit_coi <- !!length(unlist(index_any))
    out$is_coi_pred <- !!length(index)
    out$coi_text <- article[index] %>% paste(collapse = " ")

    index_any %<>% purrr::map(function(x) !!length(x))


    if (length(index) == 1) {
      coi_only <- dict$conflict_title %>% .encase %>% .title_strict
      is_coi_only <- stringr::str_detect(out$coi_text, coi_only)

      if (is_coi_only & !is.na(article[index + 1])) {

        out$coi_text <- article[c(index, index + 1)] %>% paste(collapse = ": ")

      }
    }

    return(c(out, index_any, index_ack))
  }


  # Identify potentially missed signals
  i <- which(article %in% c(article_ls$ack, article_ls$footnotes))

  if (!!length(i)) {

    index_ack <- list(
      fees_1 = .which_fees_1(article_processed[i], dict),
      consults_1 = .which_consults_1(article_processed[i], dict),
      connect_1 = .which_connections_1(article_processed[i], dict),
      connect_2 = .which_connections_2(article_processed[i], dict),
      commercial_ack_1 = .which_commercial_ack_1(article_processed[i], dict),
      rights_1 = .which_rights_1(article_processed[i], dict),
      founder_1 = .which_founder_1(article_processed[i], dict),
      advisor_1 = .which_advisor_1(article_processed[i], dict),
      paid_1 = .which_paid_1(article_processed[i], dict),
      board_1 = .which_board_1(article_processed[i], dict),
      no_coi_1 = .which_no_coi_1(article_processed[i], dict),
      no_funder_role_1 = .which_no_funder_role_1(article_processed[i], dict)
    )

    index <- i[unlist(index_ack) %>% unique() %>% sort()]
    index_ack %<>% purrr::map(function(x) !!length(x))

  }

  out$is_coi_pred <- !!length(index)
  out$coi_text <- article[index] %>% paste(collapse = " ")

  index_any %<>% purrr::map(function(x) !!length(x))

  if (out$is_coi_pred) {

    out$is_explicit_coi <- FALSE

  }

  return(c(relevance_ls, out, index_any, index_ack))
}




rt_coi_pmc2 <- function(article_xml) {

  index <- integer()
  synonyms <- .create_synonyms()


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
    grants_1 = NA,
    brief_1 = NA
  )

  index_ack <- list(
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

  # Capture coi fn elements
  out$coi_text <- .get_coi_pmc_fn(article_xml)
  index_any$coi_fn_pmc <- nchar(out$coi_text) > 0

  if (index_any$coi_fn_pmc) {

    out$is_relevant <- TRUE
    out$is_coi_pred <- TRUE

    return(tibble::as_tibble(c(out, index_any, index_ack)))
  }


  # Go through titles
  title_txt <- .get_coi_pmc_title(article_xml, synonyms)
  is_title <- nchar(title_txt) > 0

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
  index_any$grants_1 <- .which_grants_1(article_processed)
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
