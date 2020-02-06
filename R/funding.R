#' Identify mentions of support
#'
#' Returns the index with the elements of interest.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_support_1 <- function(article) {

  # synonyms <- .create_synonyms()
  # words <- c("This", "research", "is_have", "funded", "by")
  #
  # this_research <-
  #   synonyms %>%
  #   magrittr::extract(words[1:2]) %>%
  #   lapply(.bound) %>%
  #   lapply(.encase) %>%
  #   paste(collapse = " ")
  #
  # synonyms %>%
  #   magrittr::extract(words[3:5]) %>%
  #   lapply(.bound) %>%
  #   lapply(.encase) %>%
  #   # lapply(.max_words) %>%
  #   paste(collapse = synonyms$txt) %>%
  #   paste(this_research, .) %>%
  #   grep(article, perl = T)


  synonyms <- .create_synonyms()
  words <- c("This_singular", "research_singular", "is_singular", "funded", "by")

  singular <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound, location = "end") %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

  if (!length(singular)) {

    words <- c("These", "researches", "are", "funded", "by")

    synonyms %>%
      magrittr::extract(words) %>%
      lapply(.bound, location = "end") %>%
      lapply(.encase) %>%
      # lapply(.max_words) %>%
      paste(collapse = synonyms$txt) %>%
      grep(article, perl = T)

  } else {

    return(singular)

  }
}


#' Identify mentions of support
#'
#' Returns the index with the elements of interest.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_support_2 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("funded", "this_singular", "research_singular")

  singular <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound, location = "end") %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

  if (!length(singular)) {

    words <- c("funded", "these", "researches")

    synonyms %>%
      magrittr::extract(words) %>%
      lapply(.bound, location = "end") %>%
      lapply(.encase) %>%
      # lapply(.max_words) %>%
      paste(collapse = synonyms$txt) %>%
      grep(article, perl = T)

  } else {

    return(singular)

  }
}


#' Identify mentions of support
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_support_3 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("research", "is_have", "funded", "by")

  research_is <-
    synonyms %>%
    magrittr::extract(words[1:2]) %>%
    lapply(.bound, location = "end") %>%
    lapply(.encase) %>%
    paste(collapse = " ")

  synonyms %>%
    magrittr::extract(words[3:4]) %>%
    lapply(.bound, location = "end") %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
    paste(collapse = synonyms$txt) %>%
    paste(research_is, ., sep = synonyms$txt) %>%
    grep(article, perl = T)
}


#' Identify mentions of support
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_support_4 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("is", "funded", "by", "award")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)
}


#' Identify mentions of received
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_received_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("received", "funds_award_financial", "by")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = " ") %>%
    grep(article, perl = T)
}


#' Identify mentions of received
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_received_2 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("received", "support", "by", "agency")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)
}


#' Identify mentions of "the authors ... financial support
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_authors_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("This", "author", "funds_award_financial")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)


  # "[Ss]upport",
  # "[Ff]inancial assistance",
  # "\\b[Aa]id\\b",

  # The authors thank X for funding
  # The authors received no/did not receive financial support
  # The authors acknowledge the/disclosed receipt of support\\b/financial support of
  # The authors are supported by

  # The authors received no funds
  # The authors have no support or funding to report
}


#' Identify mentions of "the authors have no funding ..."
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_authors_2 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("This", "author", "have", "no", "funding")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)
}


#' Identify mentions of "thank ... financial support
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_thank_1 <- function(article) {

  synonyms <- .create_synonyms()

  synonyms$financial <- c(synonyms$financial, "for supporting")
  words <- c("We", "thank", "financial")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)
}


#' Identify mentions of "funding for this study was..."
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_fund_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("funding_financial_award", "for", "research", "received")

  funding_for <-
    synonyms %>%
    magrittr::extract(words[1:2]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = " ")

  synonyms %>%
    magrittr::extract(words[3:4]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    paste(funding_for, ., sep = synonyms$txt) %>%
    grep(article, perl = T)
}


#' Identify mentions of Funding titles
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_fund_2 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("funding_title")

  a <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.title) %>%
    lapply(.encase) %>%
    paste() %>%
    grep(article, perl = T)

  if (length(a) > 0) {

    if (nchar(article[a + 1]) == 0) {
      return(c(a, a + 2))
    } else {
      return(c(a, a + 1))
    }

  } else {

    synonyms %>%
      magrittr::extract(words) %>%
      lapply(.title, within_text = T) %>%
      lapply(.encase) %>%
      paste() %>%
      grep(article, perl = T)

  }
}


#' Identify mentions of Funding titles
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_fund_3 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("any_title")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.encase) %>%
    paste("[A-Z]") %>%
    grep(article, perl = T)

}


#' Identify mentions of funds in acknowledgements
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_fund_acknow <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("funds", "funded", "award")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    unlist() %>%
    .encase %>%
    grep(article, perl = T, ignore.case = T)

}


#' Identify mentions of "Supported by ..."
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_supported_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("Supported", "by")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = " ") %>%
    paste("[a-zA-Z]+") %>%
    grep(article, perl = T)

}

#' Identify mentions of Financial support titles
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_financial_1 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  txt_0 <- "(^Financial support(|s)(|:|\\.)$|^Financial or other support(|s)(|:|\\.)$|^Support statement(|s)(|:|\\.)$|^Financial assistance(|:|\\.)$^Financial sponsorship(|s)(|:|\\.)$|^Financial support(|s) and sponsorship(|s)(|:|\\.)$|^Financial disclosure(|s)(|:|\\.)$|^Financiamento(|:|\\.)$)"

  total_txt <- c(txt_0)
  indicator_regex <- paste0(total_txt)

  a <- grep(indicator_regex, article, perl = T, ignore.case = T)


  if (length(a) > 0) {

    if (nchar(article[a + 1]) == 0) {
      return(c(a, a + 2))
    } else {
      return(c(a, a + 1))
    }

  } else {

    txt_1 <- "(Financial [Ss]upport(|s)(:|\\.)|FINANCIAL SUPPORT(|S)(:|\\.)|Financial or other [Ss]upport(|s)(:|\\.)|FINANCIAL OR OTHER SUPPORT(|S)(:|\\.)|S(?i)upport statement(|s)(:|\\.)(?-i)|Financial [Aa]ssistance(:|\\.)|FINANCIAL ASSISTANCE(:|\\.)Financial [Ss]ponsorship(|s)(:|\\.)|FINANCIAL SPONSORSHIP(|S)(:|\\.)|Financial [Ss]upport(|s) [Aa]nd [Ss]ponsorship(|s)(:|\\.)|FINANCIAL SUPPORT(|S) AND SPONSORSHIP(|S)(:|\\.)|Financial [Dd]isclosure(|s)(:|\\.)|FINANCIAL DISCLOSURE(|S)(:|\\.)|Financiamento(:|\\.)|FINANCIAMENTO(:|\\.))"

    total_txt <- c(txt_1)
    indicator_regex <- paste0(total_txt)

    grep(indicator_regex, article, perl = T)
  }
}


#' Identify mentions of Financial support titles followed by specific text
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_financial_2 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  financial_synonyms <- c(
    "F(?i)inancial support(|s)(?-i)",
    "F(?i)inancial or other support(|s)(?-i)",
    "F(?i)inancial assistance(?-i)",
    "F(?i)inancial aid(?-i)",
    "F(?i)inancial sponsorship(|s)(?-i)",
    "F(?i)inancial support(|s) and sponsorship(|s)(?-i)",
    "F(?i)inancial disclosure(|s)(?-i)",
    "F(?i)inanciamento(?-i)"
  )

  no_synonyms <- "(No|Nil|None)"

  regex <- paste0("(",
                  paste0(financial_synonyms, collapse = "|"),
                  ") ",
                  no_synonyms)

  grep(regex, article, perl = T)
}


#' Identify mentions of Financial support titles followed by specific text
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_financial_3 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  financial_synonyms <- c(
    "F(?i)inancial support(|s)(?-i)",
    "F(?i)inancial or other support(|s)(?-i)",
    "F(?i)inancial assistance(?-i)",
    "F(?i)inancial aid(?-i)",
    "F(?i)inancial sponsorship(|s)(?-i)",
    "F(?i)inancial support(|s) and sponsorship(|s)(?-i)",
    "F(?i)inancial disclosure(|s)(?-i)",
    "F(?i)inanciamento(?-i)"
  )

  this_synonyms <- c(
    "this",
    "these"
  )

  study_synonyms <- c(
    "work\\b",
    "research\\b",
    "stud(y|ies)\\b",
    "project(|s)\\b",
    "trial(|s)\\b",
    "publication(|s)\\b",
    "\\breport(|s)\\b",
    "program(|s)\\b"
  )

  finance <- .encase(financial_synonyms)
  this <- .encase(this_synonyms)
  study <- .encase(study_synonyms)

  regex <- paste(finance, this, study, sep = txt)
  grep(regex, article, perl = T, ignore.case = T)
}


#' Identify mentions of Disclosure statements
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_disclosure_1 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  disclosure_synonyms <- c(
    "D(?i)isclosure(|s)(?-i)"
  )

  funds_synonyms <- c(
    "[Ff]und(|s)",
    "[Ff]unding",
    "[Ff]inanced",
    "[Ss]upport",
    "[Aa]ssistance",
    "\\b[Aa]id\\b",
    "[Ff]ellowship",
    "[Aa]ward",
    "[Ss]tipend",
    "[Ss]cholar",
    "[Gg]rant"
  )

  regex <- paste0("^", disclosure_synonyms, "$")
  a <- grep(regex, article, perl = T)

  out <- integer()
  if (!!length(a)) {

    for (i in 1:length(a)) {

      if (nchar(article[a[i] + 1]) == 0) {
        b <- a[i] + 2
      } else {
        b <- a[i] + 1
      }

      regex <- paste0(funds_synonyms, collapse = "|")
      d <- grep(regex, article[b], perl = T)

      if (!!length(d)) out <- c(out, a[i], b)
    }
  }
  return(out)
}


#' Identify mentions of Disclosure statements
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_disclosure_2 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  disclosure_synonyms <- c(
    "D(?i)isclosure(|s)(?-i)"
  )

  # funds_synonyms <- c(
  #   "[Ff]und(|s)",
  #   "[Ff]unding",
  #   "[Ff]inanced",
  #   "[Ss]upport",
  #   "[Aa]ssistance"
  # )
  funds_synonyms <- c(
    "[Ff]und(|s)",
    "[Ff]unding",
    "[Ff]inanced",
    "[Ss]upport",
    "[Aa]ssistance",
    "\\b[Aa]id\\b",
    "[Ff]ellowship",
    "[Aa]ward",
    "[Ss]tipend",
    "[Ss]cholar",
    "[Gg]rant"
  )

  funds <- paste0(funds_synonyms, collapse = "|")

  regex <- paste0("(", disclosure_synonyms, ")", txt, "(", funds, ")")
  grep(regex, article, perl = T)
}


#' Identify mentions of Financial support titles
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_grant_1 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  txt_0 <- "(^Grant(|s)(|:|\\.)$|^Grant sponsor(|s)(|:|\\.)$|^Grant sponsorship(|s)(|:|\\.)$|^Grant support(|:|\\.)$)"  # removed Sponsorship and sponsors b/c FP without TP

  total_txt <- c(txt_0)
  indicator_regex <- paste0(total_txt)

  a <- grep(indicator_regex, article, perl = T, ignore.case = T)


  if (length(a) > 0) {

    if (nchar(article[a + 1]) == 0) {
      return(c(a, a + 2))
    } else {
      return(c(a, a + 1))
    }

  } else {

    txt_1 <- "(Grant sponsor(|s)(:|\\.)|GRANT SPONSOR(|S)(:|\\.)|Grant [Ss]ponsorship(|s)(:|\\.)|GRANT SPONSORSHIP(|S)(:|\\.)|Grant [Ss]upport(|s)(:|\\.)|GRANT SUPPORT(|S)(:|\\.))"  # removed Sponsorship and sponsors b/c FP without TP

    total_txt <- c(txt_1)
    indicator_regex <- paste0(total_txt)

    grep(indicator_regex, article, perl = T)
  }
}


#' Identify mentions of Grant numbers in the Funding/Acknowledgements
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_project_acknow <- function(article) {

  grep("project (no|num)", article, perl = T, ignore.case = T)

}


#' Get common phrases
#'
#' Returns the index with the elements of interest.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_common_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("no", "funding_financial_award", "is", "received")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = " ") %>%
    grep(article, perl = T)
}


#' Get common phrases
#'
#' Returns the index with the elements of interest.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_common_2 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("No", "funding_financial_award", "received")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)
}




#' Identify mentions of "Acknowledgement and"
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_acknow_1 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  txt_0 <- "^Acknowledg(|e)ment(|s)"
  txt_1 <- "(of|and)"
  txt_2 <- "([Ss]upport |\\b[Ff]unding|\\b[Ff]inancial)"

  total_txt <- c(txt_0, txt_1, txt_2)
  indicator_regex <- paste0(total_txt, collapse = " ")

  a <- grep(indicator_regex, article, perl = T, ignore.case = T)

  if (length(a) > 0) {
    if (!is.na(article[a + 1])) {
      # TODO: Change in all functions
      if (nchar(article[a + 1]) == 0) {
        return(c(a, a + 2))
      } else {
        return(c(a, a + 1))
      }
    } else {
      return(a)
    }
  } else {
    return(a)
  }
}


#' Identify acknowledgements
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_acknow_2 <- function(article) {

  txt_0 <- "(^A(?i)cknowledg(|e)ment(|s)(?-i))"

  txt_1 <- "(^Acknowledg(|e)ment(|s)"
  txt_2 <- "(of|and)"
  txt_3 <- "([Ss]upport |\\b[Ff]unding|\\b[Ff]inancial))"

  total_txt <- c(txt_1, txt_2, txt_3)
  indicator_regex <- paste0(total_txt, collapse = " ")
  indicator_regex <- paste(txt_0, indicator_regex, sep = "|")

  grep(indicator_regex, article, perl = T)
}


#' Avoid disclosures that are in fact COI statements
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
negate_disclosure_1 <- function(article) {

  synonyms <- .create_synonyms()

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  disclose_synonyms <- c(
    "[Dd]isclose(|s)(|:|\\.)",
    "[Dd]isclosure(|s)(|:|\\.)"
  )

  conflict_synonyms <- c(
    "conflict(|s) of interest",
    "conflicting interest",
    "conflicting financial interest",
    "conflicting of interest",
    "conflits d'int",
    "conflictos de Inter"
  )

  compete_synonyms <- c(
    "competing interest",
    "competing of interest",
    "competing financial interest"
  )

  and_synonyms <- c(
    "and",
    "&",
    "or"
  )

  not_synonyms <- c(
    "not"
  )

  funded_synonyms <- c(
    "\\bfunded",
    "\\bfinanced",
    "\\bsupported",
    "\\bsponsored",
    "\\bresourced"
  )

  disclose <- .encase(disclose_synonyms)
  conflict <- .encase(c(conflict_synonyms, compete_synonyms))
  and <- .encase(and_synonyms)
  not <- .encase(not_synonyms)
  funded <- .encase(funded_synonyms)

  regex <- paste(disclose, conflict, and, not, funded, sep = txt)
  a <- grepl(regex, article, perl = T)

  if (any(a)) {

    return(a)

  } else {

    funded <- .encase(c(funded_synonyms, synonyms$funding))
    regex <- paste(disclose, funded, conflict, sep = txt)
    grepl(regex, article, perl = T)
  }
}


#' Avoid financial that is part of COI statements
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
negate_conflict_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("conflict_title")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.title) %>%
    lapply(.encase) %>%
    paste() %>%
    grepl(article, perl = T)
}


#' Avoid mentions that no funding information was provided
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
negate_absence_1 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  no_synonyms <- c(
    "No\\b",  # not \\bNo to capture "(No details)"
    "Nil\\b",
    "None\\b"
  )

  info_synonyms <- c(
    "info(|rmation)\\b",
    "detail(|s)",
    "particulars",
    "data\\b",
    "material\\b"
  )

  of_synonyms <- c(
    "of",
    "about"
  )

  funding_synonyms <- c(
    "F(?i)unding(?-i)",
    "F(?i)unding source(|s)(?-i)",
    "F(?i)unding source(|s) for the stud(y|ies)(?-i)",
    "F(?i)unding information(?-i)",
    "F(?i)unding statement(|s)(?-i)"
  )

  financial_synonyms <- c(
    "F(?i)inancial support(|s)(?-i)",
    "F(?i)inancial or other support(|s)(?-i)",
    "F(?i)inancial assistance(?-i)",
    "F(?i)inancial aid(?-i)",
    "F(?i)inancial sponsorship(|s)(?-i)",
    "F(?i)inancial support(|s) and sponsorship(|s)(?-i)",
    "F(?i)inancial disclosure(|s)(?-i)",
    "F(?i)inanciamento(?-i)"
  )

  is_synonyms <- c(
    "is",
    "been",
    "was",
    "were"
  )

  provided_synonyms <- c(
    "provided",
    "presented",
    "received",
    "given",
    "offered",
    "supplied"
  )

 no <- .encase(no_synonyms)
 info <- .encase(info_synonyms)
 of <- .encase(of_synonyms)
 funding <- .encase(c(funding_synonyms, financial_synonyms))
 is <- .encase(is_synonyms)
 provided <- .encase(provided_synonyms)

 regex <- paste(no, info, of, funding, is, provided, sep = txt)
 grepl(regex, article, perl = T, ignore.case = T)
}


#' Remove mentions of COIs that may cause FPs
#'
#' Returns the text without potentially misleading mentions of COIs.
#'
#' @param articles A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
obliterate_conflict_1 <- function(articles) {

  # Good for finding, but not for substituting b/c it's  a lookahead
  # words <- c(
  #   # positive lookahead makes these phrases interchangeable
  #   "(?=[a-zA-Z0-9\\s,()-]*(financial|support))",
  #   "(?=[a-zA-Z0-9\\s,()-]*(conflict|competing))"
  # )

  words <- c(
    "((funding|financial|support)[a-zA-Z0-9\\s,()/-]*(association|relationship)[a-zA-Z0-9\\s,()/-]*(conflict|competing))",
    "((conflict|competing)[a-zA-Z0-9\\s,()/-]*(association|relationship)[a-zA-Z0-9\\s,()/-]*(financial))",
    "financial(?:\\s+\\w+){0,3} interest"
  )

  words %>%
    paste(collapse = "|") %>%
    gsub("", articles, perl = T)
}


#' Remove fullstops that are unlikely to represent end of sentence
#'
#' Returns the list of paragraphs without potentially misleading fullstops.
#'
#' @param articles A List with paragraphs of interest.
#' @return The list of paragraphs without misleading fullstops.
obliterate_fullstop_1 <- function(articles) {

  articles <- gsub("([A-Z])(\\.) ([A-Z])(\\.) ([A-Z])(\\.)", "\\1 \\3 \\5", articles)
  articles <- gsub("([A-Z])(\\.) ([A-Z])(\\.)", "\\1 \\3", articles)
  articles <- gsub("(\\.) ([a-z])", " \\2", articles)
  articles <- gsub("(\\.) ([0-9])", " \\2", articles)
  articles <- gsub("(\\.) ([A-Z]+[0-9])", " \\2", articles)
  articles <- gsub("(\\.)([a-zA-Z0-9])", "\\2", articles)
  articles <- gsub("\\.([^\\s])", "\\1", articles, perl = T)

  return(articles)
}


#' Remove references
#'
#' Returns the list of paragraphs without references.
#'
#' @param articles A List with paragraphs of interest.
#' @return The list of paragraphs without misleading fullstops.
obliterate_refs_1 <- function(articles) {

  # Built like this to avoid distabilizing the algorithm
  articles <- gsub("^.*\\([0-9]{4}\\).*$", "References", articles)
  articles <- gsub("^.* et al\\..*$", "References", articles)

  return(articles)
}


#' Find the index of the references
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the start and finish of this section.
find_refs <- function(article) {

  ref_synonyms <- c(
    "R(?i)eferences(?-i)(| [A-Z0-9]+.*)",
    "L(?i)terature(?-i)(| [A-Z0-9]+.*)",
    "L(?i)iterature Cited(?-i)(| [A-Z0-9]+.*)",
    "N(?i)otes and References(?-i)(| [A-Z0-9]+.*)",
    "W(?i)orks Cited(?-i)(| [A-Z0-9]+.*)",
    "^C(?i)itations(?-i)(| [A-Z0-9]+.*)",
    "R(?i)eferences and recommended reading(?-i)(| [A-Z0-9]+.*)"
  )

  # no "^" b/c of UTF-8 characters, e.g. "\\fReferences"
  regex <- paste0("(", paste(ref_synonyms, collapse = "|"), ")$")
  ref_index <- grep(regex, article, perl = T)

  if (!!length(ref_index)) {

    ref_index <- ref_index[length(ref_index)]

  } else {

    ref_index <- grep("^1.\\s+[A-Z]", article)
    if (!!length(ref_index)) ref_index <- ref_index[length(ref_index)]

  }
  return(ref_index)
}


#' Restrict to text after the body of the article and references
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the start and finish of this section.
find_acknows <- function(article) {

  acknow_index <- get_acknow_2(article)
  fund_index <- get_fund_2(article)
  finance_index <- get_financial_1(article)
  grant_index <- get_grant_1(article)

  if (length(acknow_index) > 0) acknow_index <- acknow_index[length(acknow_index)]
  if (length(fund_index) > 0) fund_index <- fund_index[1]
  if (length(finance_index) > 0) finance_index <- finance_index[1]
  if (length(grant_index) > 0) grant_index <- grant_index[1]

  all <- c(acknow_index, fund_index, finance_index, grant_index)

  from <- integer()
  if (!!length(all)) {

    all_max <- max(all)
    all_min <- min(all)

    if (all_max - all_min <= 10) {

      from <- all_min

    } else {

      from <- all_max

    }
  }
  return(from)
}


#' Identify funding statements
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param filename A List with paragraphs of interest.
#' @return A dataframe indicating whether a funding statement has been
#'     identified and the funding statement.
#' @export
is_funding <- function(filename) {

  # TODO: Consider removing all :punct: apart from dots (e.g. author(s))

  index <- integer()
  disclosure <- integer()
  diff  <- integer()

  paragraphs <-
    readr::read_file(filename) %>%
    purrr::map(strsplit, "\n| \\*") %>%
    unlist() %>%
    utf8::utf8_encode()


  # Remove potentially misleading sequences
  utf <- "(\\\\[a-z0-9]{3})+"   # remove \\xfc\\xbe etc
  paragraphs_pruned <-
    paragraphs %>%
    purrr::map_chr(gsub, pattern = utf, replacement = " ", perl = T) %>%
    obliterate_fullstop_1() %>%
    obliterate_conflict_1()
  # paragraphs_pruned <- obliterate_refs_1(paragraphs_pruned)
  # to <- find_refs(paragraphs_pruned)
  # if (!length(to)) to <- length(paragraphs_pruned)  # TODO: prevent early mention


  # Identify sequences of interest
  index_any <- list()
  index_any[['support_1']] <- get_support_1(paragraphs_pruned)
  index_any[['support_2']] <- get_support_2(paragraphs_pruned)
  index_any[['support_3']] <- get_support_3(paragraphs_pruned)
  index_any[['support_4']] <- get_support_4(paragraphs_pruned)
  index_any[['received_1']] <- get_received_1(paragraphs_pruned)
  index_any[['received_2']] <- get_received_2(paragraphs_pruned)
  index_any[['authors_1']] <- get_authors_1(paragraphs_pruned)
  index_any[['authors_2']] <- get_authors_2(paragraphs_pruned)
  index_any[['thank_1']] <- get_thank_1(paragraphs_pruned)
  index_any[['fund_1']] <- get_fund_1(paragraphs_pruned)
  index_any[['fund_2']] <- get_fund_2(paragraphs_pruned)
  index_any[['fund_3']] <- get_fund_3(paragraphs_pruned)
  index_any[['supported_1']] <- get_supported_1(paragraphs_pruned)
  index_any[['financial_1']] <- get_financial_1(paragraphs_pruned)
  index_any[['financial_2']] <- get_financial_2(paragraphs_pruned)
  index_any[['financial_3']] <- get_financial_3(paragraphs_pruned)
  index_any[['grant_1']] <- get_grant_1(paragraphs_pruned)
  index_any[['common_1']] <- get_common_1(paragraphs_pruned)
  index_any[['common_2']] <- get_common_2(paragraphs_pruned)
  index_any[['acknow_1']] <- get_acknow_1(paragraphs_pruned)
  index_any[['disclosure_1']] <- get_disclosure_1(paragraphs_pruned)
  index_any[['disclosure_2']] <- get_disclosure_2(paragraphs_pruned)
  index <- unlist(index_any) %>% unique() %>% sort()


  # Remove potential mistakes
  if (!!length(index)) {

    if (length(unlist(index_any[c("authors_2")]))) {
      is_coi <- negate_conflict_1(paragraphs_pruned[min(index) - 1])
      index <- index[!is_coi]
    }

    is_coi_disclosure <- negate_disclosure_1(paragraphs_pruned[index])
    index <- index[!is_coi_disclosure]

    is_absent <- negate_absence_1(paragraphs_pruned[index])
    index <- index[!is_absent]

  }


  # Identify potentially missed signals
  if (!length(index)) {

    from <- find_acknows(paragraphs_pruned)
    to <- find_refs(paragraphs_pruned)

    if (!!length(from) & !!length(to)) {

      diff <- to - from

      if (diff < 0) {
        to <- min(length(paragraphs_pruned), from + 100)
        diff <- to - from
      }

      if (diff <= 100) {

        index_fund <- list()
        index_fund[['fund']] <- get_fund_acknow(paragraphs_pruned[from:to])
        index_fund[['project']] <- get_project_acknow(paragraphs_pruned[from:to])
        index <- unlist(index_fund) %>% magrittr::add(from - 1)
      }
    }
  }

  index <- sort(unique(index))
  is_funded_pred <- !!length(index)
  funding_text <- paragraphs[index] %>% paste(collapse = " ")

  article <- basename(filename) %>% stringr::word(sep = "\\.")
  pmid <- gsub("^.*PMID([0-9]+).*$", "\\1", filename)
  tibble::tibble(article, pmid, is_funded_pred, funding_text)
}


#' Encace words within parentheses and OR statements
#'
#' Returns a string of words separated by OR statements within parentheses.
#'
#' @param x A vector of strings.
#' @return A string of words separate by OR statements within parentheses.
.encase <- function(x) {

  paste0("(", paste0(x, collapse = "|"), ")")

}


#' Place boundaries around words
#'
#' Returns each string with boundaries around it.
#'
#' @param x A vector of strings.
#' @param location Where to bound each word ("both" (default), "end" or
#'     "start").
#' @return A vector of bounded strings.
.bound <- function(x, location = "end") {

  if (location == "both") {

    return(paste0("\\b[[:alnum:]]{0,1}", x, "\\b"))
    # Using alnum instead of . halved the time it takes to run the algorithm!

  }

  if (location == "end") {

    return(paste0(x, "\\b"))

  }

  if (location == "start") {

    return(paste0("\\b[[:alnum:]]{0,1}", x))

  }
}


#' Place boundaries around words
#'
#' Returns each string with boundaries around it.
#'
#' @param x A vector of strings.
#' @param n_max Number of maximum words allowed
#' @return A vector of bounded strings.
.max_words <- function(x, n_max = 3) {

  # This increases time by at least a few seconds each time used!
  paste0(x, "(?:\\s+\\w+){0,", n_max, "}")
}


#' Create a regex for titles
#'
#' Returns words designed to identify titles.
#'
#' @param x A vector of strings.
#' @param within_text Boolean defines whether a regex typical to a title found      within text should be created or not.
#' @return A vector of strings with a suffix attached.
.title <- function(x, within_text = F) {

  if (within_text) {

    return(paste0(x, "(:|\\.)"))

  } else {

    return(paste0("^", x, "(|:|\\.)$"))

  }
}


#' A list of word synonyms
#'
#' Contains the synonyms to words being used throughout the package.
#'
#' @return A list of synonyms to words of interest
.create_synonyms <- function() {

  synonyms <- list()

  synonyms[["txt"]] <- "[a-zA-Z0-9\\s,()/-]*"  # order matters

  synonyms[["This"]] <- c(
    "This",
    "These",
    "The",
    "Our"
  )

  synonyms[["This_singular"]] <- c(
    "This",
    "The",
    "Our"
  )

  synonyms[["These"]] <- c(
    "These",
    "Our"
  )

  synonyms[["this"]] <- c(
    "this",
    "these",
    "the",
    "our"
  )

  synonyms[["this_singular"]] <- c(
    "this",
    "the"
  )

  synonyms[["these"]] <- c(
    "these"
  )

  synonyms[["is"]] <- c(
    "is",
    "been",
    "are",
    "was",
    "were"
  )

  synonyms[["is_singular"]] <- c(
    "is",
    "have",
    "has",
    "was"
  )

  synonyms[["are"]] <- c(
    "is",
    "have",
    "has",
    "were"
  )

  synonyms[["have"]] <- c(
    "have",
    "has"
  )

  synonyms[["is_have"]] <- c(
    synonyms[["is"]],
    synonyms[["have"]]
  )

  synonyms[["We"]] <- c(
    "We"
  )

  synonyms[["by"]] <- c(
    "by",
    "from"
  )

  synonyms[["for"]] <- c(
    "for"
  )

  synonyms[["no"]] <- c(
    "[Nn]o",
    "[Nn]il",
    "[Nn]one"
  )

  synonyms[["No"]] <- c(
    "No",
    "Nil",
    "None"
  )

  synonyms[["author"]] <- c(
    "author(|s|\\(s\\))",
    "researcher(|s|\\(s\\))",
    "investigator(|s|\\(s\\))",
    "scientist(|s|\\(s\\))"
  )

  synonyms[["research"]] <- c(
    "[Ww]ork(|s)",
    "[Rr]esearch",
    "[Ss]tud(y|ies)",
    "[Pp]roject(|s)",
    "[Tt]rial(|s)",
    "[Pp]ublication(|s)",
    "[Rr]eport(|s)",
    "[Pp]rogram(|s)"
  )

  synonyms[["research_singular"]] <- c(
    "[Ww]ork",
    "[Rr]esearch",
    "[Ss]tudy",
    "[Pp]roject",
    "[Tt]rial",
    "[Pp]ublication",
    "[Rr]eport",
    "[Pp]rogram"
  )

  synonyms[["researches"]] <- c(
    "[Ww]orks",
    "[Ss]tudies",
    "[Pp]rojects",
    "[Tt]rials",
    "[Pp]ublications",
    "[Rr]eports",
    "[Pp]rograms"
  )

  synonyms[["funded"]] <- c(
    "[Ff]unded",
    "[Ss]elf-funded",
    "[Ff]inanced",
    "[Ss]upported",
    "[Ss]ponsored",
    "[Rr]esourced"
  )

  synonyms[["funds"]] <- c(
    "[Ff]und(|s)",
    "[Ff]unding",
    "[Ss]elf-funding"
  )

  synonyms[["funding"]] <- c(
    "[Ff]unding",
    "[Ff]unds",
    "[Ss]elf-funding",
    "[Ff]inancial",
    "[Ss]upport",
    "[Ss]ponsorship",
    "[Aa]id",
    "[Rr]esources"
  )

  synonyms[["funding_title"]] <- c(
    "F(?i)unding(?-i)",
    "F(?i)unding/Support(?-i)",
    "F(?i)unding source(|s)(?-i)",
    "S(?i)ource(|s) of funding(?-i)",
    "F(?i)unding source(|s) for the stud(y|ies)(?-i)",
    "F(?i)unding information(?-i)",
    "F(?i)unding statement(|s)(?-i)",
    "S(?i)upport statement(|s)(?-i)"
  )

  synonyms[["financial"]] <- c(
    "[Ff]inancial support(|s)",
    "[Ff]inancial or other support(|s)",
    "[Ff]inancial assistance",
    "[Ff]inancial aid(|s)",
    "[Ff]inancial sponsorship(|s)",
    "[Ff]inancial support(|s) and sponsorship(|s)",
    "[Ff]inancial disclosure(|s)",
    "[Ff]inanciamento"
  )

  synonyms[["financial_title"]] <- c(
    "F(?i)inancial support(|s)(?-i)",
    "F(?i)inancial or other support(|s)(?-i)",
    "F(?i)inancial assistance(?-i)",
    "F(?i)inancial aid(?-i)",
    "F(?i)inancial sponsorship(|s)(?-i)",
    "F(?i)inancial support(|s) and sponsorship(|s)(?-i)",
    "F(?i)inancial disclosure(|s)(?-i)",
    "F(?i)inanciamento(?-i)"
  )

  synonyms[["any_title"]] <- c(
    synonyms[["funding_title"]],
    synonyms[["financial_title"]]
  )

  synonyms[["support"]] <- c(
    "[Ss]upport(|s)"
  )

  synonyms[["Supported"]] <- c(
    "Supported"
  )

   synonyms[["award"]] <- c(
     "[Gg]rant(|s)",
     "[Ff]ellowship(|s)",
     "[Aa]ward(|s|ing)",
     "[Ss]cholar(|s|ship|ships)",
     "[Ee]ndowment(|s)",
     "[Ss]tipend(|s)",
     "[Bb]ursar(y|ies)"
   )

   synonyms[["funds_award_financial"]] <- c(
     synonyms[["funds"]],
     synonyms[["financial"]],
     synonyms[["award"]]
   )

   synonyms[["funding_financial_award"]] <- c(
     synonyms[["funding"]],
     synonyms[["financial"]],
     synonyms[["award"]]
   )

   synonyms[["agency"]] <- c(
     "[Aa]gency",
     "[Ff]oundation",
     "[Ii]nstitute",
     synonyms[["award"]]
   )

   synonyms[["received"]] <- c(
     "[Rr]eceived",
     "[Aa]ccepted",
     "[Aa]cquired",
     "[Pp]rovided",
     "[Gg]ranted",
     "[Aa]warded",
     "[Gg]iven",
     "[Oo]ffered",
     "[Aa]llotted",
     "[Dd]isclosed"
   )

   synonyms[["thank"]] <- c(
     "[Tt]hank",
     "[Aa]cknowledge",
     "[Dd]isclose"
   )

   synonyms[["conflict"]] <- c(
     "[Cc]onflict(|ing)",
     "[Cc]onflits",
     "[Cc]onflictos",
     "[Cc]ompeting"
   )

   synonyms[["conflict_title"]] <- c(
     "C(?i)onflict(|s) of interest(?-i)",
     "C(?i)onflicting interest(?-i)",
     "C(?i)onflicting financial interest(?-i)",
     "C(?i)onflicting of interest(?-i)",
     "C(?i)onflits d'int(?-i)",
     "C(?i)onflictos de Inter(?-i)",
     "C(?i)ompeting interest(?-i)",
     "C(?i)ompeting of interest(?-i)",
     "C(?i)ompeting financial interest(?-i)",
     "D(?i)eclaration of interest(?-i)",
     "D(?i)uality of interest(?-i)"
   )

  return(synonyms)
}
