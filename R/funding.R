# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


.encase <- function(txt) {

  paste0("(", paste0(txt, collapse = "|"), ")")

}


#' Identify mentions of support
#'
#' Returns the index with the elements of interest.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_support_1 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  support_0 <- "(The|This|These|Our|Research)"
  support_1 <- "(\\b[Ww]ork|\\b[Rr]esearch |\\b[Ss]tudy|\\b[Ss]tudies|\\b[Pp]roject|\\b[Tt]rial|\\bpublication|\\breport(|s)\\b|[Pp]rogram\\b)"
  support_2 <- "(\\bis|\\bwas|\\bwere|\\bhas|\\bhave)"
  support_3 <- "(\\bsupported|\\bfunded|\\bfinanced|\\bsponsored|resourced)"
  support_4 <- "(\\bby|from)"

  support_txt <- c(support_0, support_1, support_2, support_3, support_4)
  support_regex <- paste0(support_txt, collapse = txt)

  grep(support_regex, article, perl = T)

}


#' Identify mentions of support
#'
#' Returns the index with the elements of interest.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_support_2 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  support_0 <- "(\\bsupported|\\bfunded|\\bfinanced|\\bsponsored|resourced)"
  support_1 <- "(the|this|these)"
  support_2 <- "(work|research|study|studies|project|trial|publication|\\breport |program )"

  support_txt <- c(support_0, support_1, support_2)
  support_regex <- paste0(support_txt, collapse = " ")

  grep(support_regex, article, perl = T)

}


#' Identify mentions of support
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_support_3 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  support_1 <- "(\\bwork|\\bresearch\\b|\\bstudy|\\bstudies|\\bproject|\\btrial|\\bpublication|\\breport\\b|program\\b)"
  support_2 <- "(\\bis|\\bwas|\\bwere|\\bhas|\\bhave)"
  support_3 <- "(\\bsupported|\\bfunded|\\bfinanced|\\bsponsored|resourced)"
  support_4 <- "(\\bby|from)"

  support_txt <- c(support_1, support_2, support_3, support_4)
  support_regex <- paste0(support_txt, collapse = txt)

  grep(support_regex, article, perl = T, ignore.case = T)  # e.g. Trial

}


#' Identify mentions of support
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_support_4 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  is_synonyms <- c(
    "\\bis",
    "\\bwas",
    "\\bwere"
  )

  funded_synonyms <- c(
    "\\bfunded",
    "\\bfinanced",
    "\\bsupported",
    "\\bsponsored",
    "\\bresourced"
  )

  by_synonyms <- c(
    "\\bby"
  )

  award_synonyms <- c(
    "[Gg]rant",
    "[Ff]ellowship",
    "[Aa]ward",
    "[Ss]cholar",
    "[Ee]ndowment",
    "[Bb]ursary"
  )

  is <- .encase(is_synonyms)
  funded <- .encase(funded_synonyms)
  by <- .encase(by_synonyms)
  award <- .encase(award_synonyms)

  regex <- paste(is, funded, by, award, sep = txt)
  grep(regex, article, perl = T)
}


#' Identify mentions of received
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_received_1 <- function(article) {

  received_synonyms <- c(
    "received",
    "accepted",
    "acquired"
  )

  funds_synonyms <- c(
    "[Ff]und(|s)\\b",
    "[Ff]unding\\b",
    "[Ff]inanced\\b",
    "[Ff]ellowship(|s)\\b",
    "[Aa]ward(|s|ing)\\b",
    "[Ss]tipend(|s)",
    "[Ss]cholar(|s|ship|ships)\\b",
    "[Gg]rant\\b"
  )

  financial_synonyms <- c(
    "financial support(|s)",
    "financial or other support(|s)",
    "financial assistance",
    "financial aid(|s)",
    "financial sponsorship(|s)",
    "financial support(|s) and sponsorship(|s)",
    "financial disclosure(|s)",
    "financiamento"
  )

  from_synonyms <- c(
    "from"
  )

  received <- .encase(received_synonyms)
  funds <- .encase(c(funds_synonyms, financial_synonyms))
  from <- .encase(from_synonyms)

  regex <- paste(received, funds, from)
  grep(regex, article, perl = T)
}


#' Identify mentions of received
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_received_2 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  received_synonyms <- c(
    "received",
    "accepted",
    "acquired"
  )

  support_synonyms <- c(
    "[Ss]upport(|s)\\b"
  )

  from_synonyms <- c(
    "from"
  )

  agency_synonyms <- c(
    "[Aa]gency",
    "[Ff]oundation",
    "[Ii]nstitute",
    "[Gg]rant(|s)\\b"
  )

  received <- .encase(received_synonyms)
  support <- .encase(support_synonyms)
  from <- .encase(from_synonyms)
  agency <- .encase(agency_synonyms)

  regex <- paste(paste(received, support, from), agency, sep = txt)
  grep(regex, article, perl = T)
}

#' Identify mentions of "the authors ... financial support
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_authors_1 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  the <- "The"

  # TODO: Consider removing all :punct" from the document to easy analysis
  author_synonyms <- c(
    "author(|s|\\(s\\))",
    "researcher(|s|\\(s\\))",
    "investigator(|s|\\(s\\))",
    "scientist(|s|\\(s\\))"
  )

  funds_synonyms <- c(
    "[Ff]und(|s)",
    "[Ff]unding",
    "[Ff]inanced",
    "[Ff]ellowship",
    "[Aa]ward",
    "[Ss]tipend",
    "[Ss]cholar",
    "[Gg]rant"
  )

  financial_synonyms <- c(
    "financial support(|s)",
    "financial or other support(|s)",
    "financial assistance",
    "financial aid(|s)",
    "financial sponsorship(|s)",
    "financial support(|s) and sponsorship(|s)",
    "financial disclosure(|s)",
    "financiamento"
  )


  the <- .encase(the)
  author <- .encase(author_synonyms)
  funds <- .encase(c(funds_synonyms, financial_synonyms))

  total_txt <- c(the, author, funds)
  regex <- paste0(total_txt, collapse = txt)

  grep(regex, article, perl = T)

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

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  txt_0 <- "The"
  txt_1 <- "(author|researcher|investigator)"
  txt_2 <- "(has|have)"
  txt_3 <- "(no)"
  txt_4 <- "(funding|support|sponsorship|aid\\b)"

  total_txt <- c(txt_0, txt_1, txt_2, txt_3, txt_4)
  indicator_regex <- paste0(total_txt, collapse = txt)

  grep(indicator_regex, article, perl = T)
}



#' Identify mentions of "thank ... financial support
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_thank_1 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  txt_0 <- "We"
  txt_1 <- "(thank|acknowledge|disclose)"
  txt_2 <- "(financial support|for supporting)"

  total_txt <- c(txt_0, txt_1, txt_2)
  indicator_regex <- paste0(total_txt, collapse = txt)

  grep(indicator_regex, article, perl = T)
}


#' Identify mentions of "funding for this study was..."
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_fund_1 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  txt_0 <- "([Ff]unding|[Ff]unds|[Ff]inancial|[Ss]ponsorship|[Ss]upport) for"
  txt_1 <- "(\\b[Ww]ork|\\b[Rr]esearch\\b|\\b[Ss]tudy|\\b[Ss]tudies|\\b[Pp]roject|\\b[Tt]rial| publication|\\breport\\b|[Pp]rogram\\b)"
  txt_2 <- "(provided|received|granted)"

  total_txt <- c(txt_0, txt_1, txt_2)
  indicator_regex <- paste0(total_txt, collapse = txt)

  grep(indicator_regex, article, perl = T)
}


#' Identify mentions of Funding titles
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_fund_2 <- function(article) {

  txt <- "[a-zA-Z0-9\\s,()-]*"  # order matters

  txt_0 <- "(^Funding(|:|\\.)$|^Funding source(|s)(|:|\\.)$|^Funding source(|s) for the stud(y|ies)(|:|\\.)$|^Source(|s) of funding(|:|\\.)$|^Funding information(|:|\\.)$|^Funding statement(|s)(|:|\\.)$)"

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

    txt_1 <- "(Funding(:|\\.)|FUNDING(:|\\.)|Funding source(|s)(:|\\.)|FUNDING SOURCE(|S)(:|\\.)|Funding source(|s) for the stud(y|ies)(:|\\.)|FUNDING SOURCE(|S) FOR THE STUD(Y|IES)(:|\\.)|Source(|s) of funding(:|\\.)|SOURCE(|S) OF FUNDING(:|\\.)|Funding information(:|\\.)|FUNDING INFORMATION(:|\\.)|Funding statement(|s)(:|\\.)|FUNDING STATEMENT(|S)(:|\\.))"

    total_txt <- c(txt_1)
    indicator_regex <- paste0(total_txt)

    grep(indicator_regex, article, perl = T)
  }
}


#' Identify mentions of Funding titles
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_fund_3 <- function(article) {

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

  phrases <- c(funding_synonyms, financial_synonyms)
  regex <- paste0("(", paste0(phrases, collapse = "|"), ") [A-Z]")
  grep(regex, article, perl = T)

}


#' Identify mentions of funds in acknowledgements
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_fund_acknow <- function(article) {

  funds_synonyms <- c(
    "[Ff]und(|s)\\b",
    "[Ff]unding\\b",
    "[Ff]inanced\\b",
    "[Ff]ellowship(|s)\\b",
    "[Aa]ward(|s|ing)\\b",
    "[Ss]tipend(|s)",
    "[Ss]cholar(|s|ship|ships)\\b",
    "[Gg]rant\\b"
  )

  award_synonyms <- c(
    "[Gg]rant(|s)\\b",
    "[Ff]ellowship(|s)\\b",
    "[Aa]ward(|s)\\b",
    "[Ss]cholar(|s|ship|ships)\\b",
    "[Ee]ndowment(|s)\\b",
    "[Bb]ursar(y|ies)"
  )

  funded_synonyms <- c(
    "\\bfunded",
    "\\bfinanced",
    "\\bsupported",
    "\\bsponsored",
    "\\bresourced"
  )

  any_funds <- .encase(c(funds_synonyms, award_synonyms, funded_synonyms))
  grep(any_funds, article, perl = T, ignore.case = T)
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
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_common_1 <- function(article) {

  no_synonyms <- c(
    "No",
    "Nil",
    "None"
  )

  funds_synonyms <- c(
    "[Ff]und(|s)",
    "[Ff]unding",
    "[Ff]inanced",
    "[Ss]upport",
    "[Aa]ssistance",
    "[Ff]ellowship",
    "[Aa]ward",
    "[Ss]tipend",
    "[Ss]cholar",
    "[Gg]rant"
  )

  was_synonyms <- c(
    "was",
    "were"
  )

  received_synonyms <- c(
    "received",
    "provided",
    "given",
    "awarded",
    "offered",
    "allotted"
  )

  no <- .encase(no_synonyms)
  funds <- .encase(funds_synonyms)
  was <- .encase(was_synonyms)
  received <- .encase(received_synonyms)

  regex <- paste(no, funds, was, received)
  grep(regex, article, perl = T)
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
    "&"
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
  grepl(regex, article, perl = T)
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
#' @param article A List with paragraphs of interest.
#' @return A dataframe indicating whether a funding statement has been
#'     identified and the funding statement.
is_funding <- function(filename) {

  index <- integer()
  disclosure <- integer()
  diff  <- integer()

  paragraphs <-
    readr::read_file(filename) %>%
    {strsplit(., "\n| \\*")[[1]]} %>%
    utf8::utf8_encode()

  index_any <- list()
  index_any[['support_1']] <- get_support_1(paragraphs)
  index_any[['support_2']] <- get_support_2(paragraphs)
  index_any[['support_3']] <- get_support_3(paragraphs)
  index_any[['support_4']] <- get_support_4(paragraphs)
  index_any[['received_1']] <- get_received_1(paragraphs)
  index_any[['received_2']] <- get_received_2(paragraphs)
  index_any[['authors_1']] <- get_authors_1(paragraphs)
  index_any[['authors_2']] <- get_authors_2(paragraphs)
  index_any[['thank_1']] <- get_thank_1(paragraphs)
  index_any[['fund_1']] <- get_fund_1(paragraphs)
  index_any[['fund_2']] <- get_fund_2(paragraphs)
  index_any[['fund_3']] <- get_fund_3(paragraphs)
  index_any[['financial_1']] <- get_financial_1(paragraphs)
  index_any[['financial_2']] <- get_financial_2(paragraphs)
  index_any[['financial_3']] <- get_financial_3(paragraphs)
  index_any[['grant_1']] <- get_grant_1(paragraphs)
  index_any[['common_1']] <- get_common_1(paragraphs)
  index_any[['acknow_1']] <- get_acknow_1(paragraphs)
  index_any[['disclosure_1']] <- get_disclosure_1(paragraphs)
  index_any[['disclosure_2']] <- get_disclosure_2(paragraphs)
  index <- unlist(index_any) %>% unique() %>% sort()

  if (!!length(index)) {

    is_coi <- negate_disclosure_1(paragraphs[index])
    index <- index[!is_coi]

    is_absent <- negate_absence_1(paragraphs[index])
    index <- index[!is_absent]

  }


  if (!length(index)) {

    from <- find_acknows(paragraphs)
    to <- find_refs(paragraphs)

    if (!!length(from) & !!length(to)) {

      diff <- to - from

      if (diff < 0) {
        to <- min(length(paragraphs), from + 100)
        diff <- to - from
      }

      if (diff <= 100) {

        index_fund <- list()
        index_fund[['fund']] <- get_fund_acknow(paragraphs[from:to])
        index_fund[['project']] <- get_project_acknow(paragraphs[from:to])
        index <- unlist(index_fund) %>% add(from - 1)
      }
    }
  }

  index <- sort(unique(index))
  is_funded_pred <- !!length(index)
  funding_text <- paragraphs[index] %>% paste(collapse = " ")

  article <- basename(filename) %>% stringr::word(sep = "\\.")
  pmid <- gsub("^.*PMID([0-9]+).*$", "\\1", filename)
  tibble(article, pmid, is_funded_pred, funding_text)
}




#' Sum of vector elements.
#'
#' \code{sum} returns the sum of all the values present in its arguments.
#'
#' This is a generic function: methods can be defined for it directly
#' or via the \code{\link{Summary}} group generic. For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.
#'
#' @param ... Numeric, complex, or logical vectors.
#' @param na.rm A logical scalar. Should missing values (including NaN)
#'   be removed?
#' @return If all inputs are integer and logical, then the output
#'   will be an integer. If integer overflow
#'   \url{http://en.wikipedia.org/wiki/Integer_overflow} occurs, the output
#'   will be NA with a warning. Otherwise it will be a length-one numeric or
#'   complex vector.
#'
#'   Zero-length vectors have sum 0 by definition. See
#'   \url{http://en.wikipedia.org/wiki/Empty_sum} for more details.
#' @examples
#' sum(1:10)
#' sum(1:5, 6:10)
#' sum(F, F, F, T, T)
#'
#' sum(.Machine$integer.max, 1L)
#' sum(.Machine$integer.max, 1)
#'
#' \dontrun{
#' sum("a")
#' }
# sum <- function(..., na.rm = TRUE) {}