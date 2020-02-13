# TODO: First find the paragraphs that contain intersting words and then
#    apply the obliteration and rest of functions and test if this saves
#    time!



#' Identify mentions of support
#'
#' Identifies mentions of "This work was funded by ..." and of "This work was
#'     completed using funds from ..."
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
  words <- c("This_singular", "research_singular", "is_singular", "funded_funding", "by")

  this_research <-
    synonyms %>%
    magrittr::extract(words[1:2]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
    paste(collapse = ".{0,15}")

  was_funded_by <-
    synonyms %>%
    magrittr::extract(words[3:5]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
    paste(collapse = synonyms$txt)

  singular <-
    c(this_research, was_funded_by) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

  if (!length(singular)) {

    words <- c("These", "researches", "are", "funded_funding", "by")

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
  words <- c("funded_funding", "this_singular", "research_singular")

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

  # TODO: I have now made this function much more general than before, by
  #    removing the requirement for "is" from the start. It seems that this now
  #    became more sensitive with no loss in specificity. If it remains so in
  #    further testing, remove all previews functions, which are basically more
  #    specific versions of this!

  synonyms <- .create_synonyms()
  words <- c("funded", "by", "award")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)
}


get_support_5 <- function(article) {

  # TODO: This is trying to capture some phrases missed by get_support_4 b/c of
  #    the requirement for is, e.g. "Project supported by X". I am introducing
  #    a 3 word limit at the start to make it stricter - upon tests, there were
  #    very very few mistakes (1/200 FP) without introducing this restriction.

  synonyms <- .create_synonyms()
  words <- c("funded", "by", "foundation")

  funded_by_award <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt)

  start_of_sentence <- "(^\\s*|(:|\\.)\\s*|[A-Z][a-zA-Z]+\\s*)"
  .max_words(start_of_sentence, n_max = 4, space_first = F) %>%
    paste0(funded_by_award) %>%
    grep(article, perl = T)
}


#' Identify mentions of support
#'
#' Return the index of support statements such as: We gratefully acknowledge
#'     support from the UK Engineering and Physical Sciences Research Council.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_support_6 <- function(article) {

  synonyms <- .create_synonyms()
  words_1 <- c("acknowledge", "support_only", "foundation_award")
  words_2 <- c("support_only", "foundation_award", "acknowledged")

  .max_words(c("acknowledge", "support_only"), 2)

  acknowledge <-
    synonyms %>%
    magrittr::extract(words_1[1]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    lapply(.max_words)

  support_foundation <-
    synonyms %>%
    magrittr::extract(words_1[2:3]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt)

  a <-
    c(acknowledge, support_foundation) %>%
    paste(collapse = " ") %>%
    grep(article, perl = T)

  b <-
    synonyms %>%
    magrittr::extract(words_2) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

  return(unique(c(a, b)))
}


#' Identify mentions of support
#'
#' Return the index of support statements such as: Support was provided by the
#'     U.S. Department of Agriculture (USDA) Forest Service.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_support_7 <- function(article) {

  synonyms <- .create_synonyms()

  a <- "Support"
  b <- .encase(synonyms$received)
  d <- .encase(synonyms$by)
  grep(paste(a, b, d, sep = synonyms$txt), article, perl = T)

}


#' Identify mentions of support
#'
#' Return the index of support statements such as: We are grateful to the
#'     National Institute of Mental Health (MH091070: PI’s Dante Cicchetti and
#'     Sheree L. Toth) for support of this work.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_support_8 <- function(article) {

  # TODO: This function was onboarded to capture a tiny proportion of
  #    statements. Consider (1) activating this function only if no hit by the
  #    more prevalent functions, (2) placing it in Acknowledgements (all missed
  #    articles that prompted this are in the acknowledgements) and (3) to
  #    change this function into processing sentence by sentence to cover for
  #    all permutations of these terms.

  synonyms <- .create_synonyms()
  words <- c("foundation", "provide", "funding_financial", "for_of", "research")
  max_words <- .max_words(" ", n_max = 3, space_first = F)

  # foundation <-
  #   synonyms %>%
  #   magrittr::extract(words[1]) %>%
  #   lapply(.bound) %>%
  #   lapply(.encase)
  #
  # funding_for_research <-
  #   synonyms %>%
  #   magrittr::extract(words[2:4]) %>%
  #   lapply(.bound) %>%
  #   lapply(.encase) %>%
  #   paste(collapse = max_words)
#
#   for_research <-
#     synonyms %>%
#     magrittr::extract(words[4:5]) %>%
#     lapply(.bound) %>%
#     lapply(.encase) %>%
#     paste(collapse = max_words)

  # c(foundation, funding_for_research) %>%
  #   paste(collapse = synonyms$txt) %>%
  #   grep(article, perl = T)

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify mentions of support
#'
#' Return the index of support statements such as: The US Environmental
#'     Protection Agency’s (USEPA) Office of Research and Development funded and
#'     managed the research described in the present study.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_support_9 <- function(article) {

  # Only done for words other than "supported" b/c this is very generic

  synonyms <- .create_synonyms()
  words <- c("foundation", "funded", "research")

  foundation <-
    synonyms %>%
    magrittr::extract(words[1]) %>%
    lapply(.bound) %>%
    lapply(.encase)

  funding <-
    synonyms %>%
    magrittr::extract(words[2]) %>%
    lapply(grep, pattern = "upport", value = T, invert = T) %>%
    lapply(.bound) %>%
    lapply(.encase)

  research <-
    synonyms %>%
    magrittr::extract(words[3]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste0(".{0,20}\\.")  # only match to words at the end of the sentence.

  c(foundation, funding, research) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify mentions of support
#'
#' Return the index of support statements such as: We are grateful to the
#'     National Institute of Mental Health (MH091070: PI’s Dante Cicchetti and
#'     Sheree L. Toth) for support of this work.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_support_10 <- function(article) {

  # TODO: This function was onboarded to capture a tiny proportion of
  #    statements. Consider (1) activating this function only if no hit by the
  #    more prevalent functions, (2) placing it in Acknowledgements (all missed
  #    articles that prompted this are in the acknowledgements) and (3) to
  #    change this function into processing sentence by sentence to cover for
  #    all permutations of these terms.

  synonyms <- .create_synonyms()
  words <- c("thank", "foundation", "funding_financial", "research")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify mentions of developed
#'
#' Return the index of statements such as: This publication was developed
#'     under Assistance Agreement No. 83563701-0 awarded by the U.S.
#'     Environmental Protection Agency to the University of Michigan.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_developed_1 <- function(article) {

  # TODO: This function was onboarded to capture a tiny proportion of
  #    statements. Consider (1) activating this function only if no hit by the
  #    more prevalent functions, (2) placing it in Acknowledgements (all missed
  #    articles that prompted this are in the acknowledgements) and (3) to
  #    change this function into processing sentence by sentence to cover for
  #    all permutations of these terms.

  synonyms <- .create_synonyms()
  words <- c("This", "research", "is", "developed", "by", "foundation")

  this_research_is <-
    synonyms %>%
    magrittr::extract(words[1:3]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
    paste(collapse = synonyms$txt)

  developed <- words[4]

  by_foundation <-
    synonyms %>%
    magrittr::extract(words[5:6]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
    paste(collapse = synonyms$txt)

  c(this_research_is, developed, by_foundation) %>%
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
  words <- c("received", "funding_financial", "by", "foundation_award")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)
}


#' Identify mentions of recepient
#'
#' Returns the index of mentions such as: "Recipient of National Institutes of
#'     Health Grants AG044552, AI121621, and DK112365.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_recipient_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("recipient", "award")

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
  words <- c("This", "author", "have", "no", "funding_financial_award")

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


#' Identify mentions of thank you statements
#'
#' Returns the index with the elements related to "The authors acknowledge
#'     Worldwide Cancer Research (AICR) 15-1002; Blueprint 282510; ..."
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_thank_2 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("thank")
  txt <- "[a-zA-Z0-9\\s,()/:;-]*"

  thank <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt)

  thank %>%
    paste("[0-9]{5}", sep = txt) %>%
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
  words <- c("funded", "funds_award_financial")

  funded_synonyms <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    unlist()

  c(funded_synonyms, "NIH (|\\()(R|P)[0-9]{2}", "awarded by") %>%
    .encase %>%
    grep(article, perl = T, ignore.case = T)

}


#' Identify mentions of funds in acknowledgements
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_fund_acknow_new <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("acknowledge", "support_only", "grant|foundation|insititute|organization")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    unlist() %>%
    .encase %>%
    grep(article, perl = T, ignore.case = T)

}


# get_fund_acknow_new <- function(article) {
#
#   synonyms <- .create_synonyms()
#   words <- c("acknowledge")
#
#   a <- synonyms %>%
#     magrittr::extract(words) %>%
#     lapply(.bound) %>%
#     lapply(.encase)
#
#   grep(paste0(a, synonyms$txt, "[0-9]{3,10}"), article, perl = T)
#
# }


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

  synonyms <- .create_synonyms()
  words <- c("financial_title")

  a <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.title) %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
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
      # lapply(.max_words) %>%
      paste() %>%
      grep(article, perl = T)

  }
}


#' Identify mentions of Financial support titles followed by specific text
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_financial_2 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("financial_title", "No")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
    paste(collapse = " ") %>%
    grep(article, perl = T)

}


#' Identify mentions of Financial support titles followed by specific text
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_financial_3 <- function(article) {


  synonyms <- .create_synonyms()
  words <- c("financial_title", "this", "research")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify mentions of Disclosure statements
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_disclosure_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("disclosure_title")

  a <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.title) %>%
    lapply(.encase) %>%
    paste(collapse = "|") %>%
    grep(article, perl = T)

  out <- integer()
  if (!!length(a)) {

    for (i in 1:length(a)) {

      if (nchar(article[a[i] + 1]) == 0) {
        b <- a[i] + 2
      } else {
        b <- a[i] + 1
      }

      d <-
        synonyms$funding_financial_award %>%
        lapply(.bound) %>%
        paste0(collapse = "|") %>%
        grep(article[b], perl = T)

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

  synonyms <- .create_synonyms()
  words <- c("disclosure_title", "funding_financial_award")

  disclosure <-
    synonyms %>%
    magrittr::extract(words[1]) %>%
    lapply(.title, within_text = T)

  funding <-
    synonyms %>%
    magrittr::extract(words[2]) %>%
    lapply(.bound)

  c(disclosure, funding) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify mentions of Financial support titles
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_grant_1 <- function(article) {

  # TODO: This whole function takes a LOT of time to run, but it is ONLY
  #    activated for "(Grant [Ss]ponsor(|s):)|)Contract [Gg]rant
  #    [Ss]ponsor(|s):). Consider replacing it with just this!!!

  synonyms <- .create_synonyms()
  words <- c("grant_title")

  a <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.title) %>%
    lapply(.encase) %>%
    paste() %>%
    grep(article, perl = T)


  if (!!length(a)) {

    if (nchar(article[a + 1]) == 0) {
      return(c(a, a + 2))
    } else {
      return(c(a, a + 1))
    }

  } else {

    # This is done to avoid mentions such as "Grant AK, Brown AZ, ..."
    grant <- c("G(?i)rant ", "^[A-Z](?i)\\w+ grant ")

    support <-
      synonyms %>%
      magrittr::extract(c("support", "funder")) %>%
      lapply(paste0, "(?-i)") %>%
      lapply(.title, within_text = T) %>%
      unlist()

    grant %>%
      lapply(paste0, support) %>%
      unlist() %>%
      .encase() %>%
      grep(article, perl = T)

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

  no_funding <-
    synonyms %>%
    magrittr::extract(words[1:2]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = " ")

  was_received <-
    synonyms %>%
    magrittr::extract(words[3:4]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = " ")

  no_funding %>%
    paste(was_received, sep = synonyms$txt) %>%
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


#' Get common phrases
#'
#' Identify statements of the following type: "All authors are required to
#'     disclose all affiliations, funding sources and financial or management
#'     relationships that could be perceived as potential sources of bias. The
#'     authors disclosed none.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_common_3 <- function(article) {

  grep("required to disclose.*disclosed none", article)

}


#' Get common phrases
#'
#' Identify statements of the following type: "There were no external funding
#'     sources for this study"
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
get_common_4 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("no", "funding_financial_award", "for", "this", "research")

  no_funding <-
    synonyms %>%
    magrittr::extract(words[1:2]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt)

  for_this <-
    synonyms %>%
    magrittr::extract(words[3:4]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = " ")

  research <-
    synonyms %>%
    magrittr::extract(words[5]) %>%
    lapply(.bound) %>%
    lapply(.encase)

  c(no_funding, for_this, research) %>%
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


#' Identify "Funding disclosure. Nothing to declare" statements
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the paragraph of interest.
negate_disclosure_2 <- function(article) {

  synonyms <- .create_synonyms()

  Disclosure_synonyms <- c(
    "F(?i)inancial disclosure(|s)(?-i)(|:|\\.)",
    "F(?i)inancial declaration(|s)(?-i)(|:|\\.)",
    "Disclosure(|:|\\.)",
    "Declaration(|:|\\.)"
  )

  disclosure_synonyms <- c(
    "financial disclosure(|s)",
    "financial declaration(|s)",
    "disclosure",
    "declaration"
  )

  disclose_synonyms <- c(
    "to disclose",
    "to declare",
    "to report"
  )

  Disclosure <- .encase(Disclosure_synonyms)
  disclosure <- .encase(disclosure_synonyms)
  disclose   <- .encase(disclose_synonyms)
  no <- .encase(synonyms$No)
  no_1 <- "(no|not have any)"
  no_2 <- "(nil|nothing)"

  regex_1 <-  # Financial disclosure: Nothing to declare
    paste(Disclosure, no) %>%
    .encase()
  regex_2 <-  # Financial disclosure: The authors have no financial disclosures
    paste(Disclosure, paste(no_1, disclosure), sep = synonyms$txt) %>%
    .encase()
  regex_3 <-  # Financial disclosure: The author has nothing to declare
    paste(Disclosure, paste(no_2, disclose), sep = synonyms$txt) %>%
    .encase()

  regex <- paste(regex_1, regex_2, regex_3, sep = "|")
  grepl(regex, article, perl = T)

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

  synonyms <- .create_synonyms()
  words <- c("No", "info", "of", "funding_financial_award", "is", "received")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    grepl(article, perl = T)

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

  synonyms <- .create_synonyms()

  financial_1 <- "(funding|financial|support)"
  financial_2 <- "(financial|support)"
  relationship <- synonyms$relationship %>% .bound() %>% .encase()
  conflict <- synonyms$conflict %>% .bound() %>% .encase()
  financial_interest <- "(financial(?:\\s+\\w+){0,3} interest)"

  regex_1 <- paste(financial_1, relationship, conflict, sep = synonyms$txt)
  regex_2 <- paste(conflict, relationship, financial_2, sep = synonyms$txt)
  regex_3 <- paste(relationship, financial_interest,  sep = synonyms$txt)

  c(regex_1, regex_2, regex_3) %>%
    lapply(.encase) %>%
    paste(collapse = "|") %>%
    gsub("", articles, perl = T)

}


#' Remove disclosures with inappropriate sentences
#'
#' Returns the text without potentially misleading disclsoures mentions. This
#'     is intended to solve problems with disclosures, such as: Disclosure.
#'     Authors have no conflict of interests, and the work was not supported
#'     or funded by any drug company. This project was funded by the Deanship
#'     of Scientific Research, King Abdulaziz University, Jeddah, Saudi Arabia
#'     (Grant No. 4/165/1431);
#'
#' @param articles A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
obliterate_disclosure_1 <- function(articles) {

  synonyms <- .create_synonyms()
  words <- c("disclosure_title", "conflict", "and", "not", "funded")

  # disclosure_title <-
  #   synonyms %>%
  #   magrittr::extract(words[1]) %>%
  #   lapply(.title) %>%
  #   lapply(.encase)

  synonyms %>%
    magrittr::extract(words[2:5]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    paste0(synonyms$txt, ., synonyms$txt, "($|.)") %>%
    gsub("", articles, perl = T)

  # disclosure_title %>%
  #   paste0("(", synonyms$txt, conflict_funded, synonyms$txt, ")") %>%
  #   gsub("\\1", articles, perl = T)


  # if (any(a)) {
  #
  #   return(a)
  #
  # } else {
  #
  #   funded <- .encase(c(funded_synonyms, synonyms$funding))
  #   regex <- paste(disclose, funded, conflict, sep = txt)
  #   grepl(regex, article, perl = T)
  # }

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
  articles <- gsub("(\\s[A-Z])(\\.) ([A-Z][a-z]+)", "\\1 \\3", articles)
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
    "^B(?i)ibliographic references(?-i)(| [A-Z0-9]+.*)",
    "R(?i)eferences and recommended reading(?-i)(| [A-Z0-9]+.*)"
  )

  # no "^" b/c of UTF-8 characters, e.g. "\\fReferences"
  regex <- paste0("(", paste(ref_synonyms, collapse = "|"), ")$")
  ref_index <- grep(regex, article, perl = T)

  if (!!length(ref_index)) {

    ref_index <- ref_index[length(ref_index)]

  } else {

    ref_index <- grep("^1(|\\.)\\s+[A-Z]", article)
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
  diff <- integer()

  # Fix PDF to txt bugs
  broken_1 <- "([a-z]+)-\n([a-z]+)"
  broken_2 <- "([a-z]+)(|,|;)\n([a-z]+)"
  paragraphs <-
    readr::read_file(filename) %>%
    purrr::map(gsub, pattern = broken_1, replacement = "\\1\\2") %>%
    purrr::map(gsub, pattern = broken_2, replacement = "\\1 \\3") %>%
    purrr::map(strsplit, "\n| \\*") %>%
    unlist() %>%
    utf8::utf8_encode()


  # Remove potentially misleading sequences
  utf_1 <- "(\\\\[a-z0-9]{3})"   # remove \\xfc\\xbe etc
  utf_2 <- "(\\\\[a-z])"   # \\f or
  paragraphs_pruned <-
    paragraphs %>%
    purrr::map_chr(gsub, pattern = utf_1, replacement = " ", perl = T) %>%
    purrr::map_chr(gsub, pattern = utf_2, replacement = "",  perl = T) %>%
    obliterate_fullstop_1() %>%
    obliterate_conflict_1() %>%
    obliterate_disclosure_1()  # Adds 30s overhead! TODO: Place elsehwere
  # paragraphs_pruned <- obliterate_refs_1(paragraphs_pruned)
  # to <- find_refs(paragraphs_pruned)
  # if (!length(to)) to <- length(paragraphs_pruned)  # TODO: prevent early mention


  # Identify sequences of interest
  index_any <- list()
  index_any[['support_1']] <- get_support_1(paragraphs_pruned)
  # index_any[['support_2']] <- get_support_2(paragraphs_pruned)
  index_any[['support_3']] <- get_support_3(paragraphs_pruned)
  index_any[['support_4']] <- get_support_4(paragraphs_pruned)
  index_any[['support_5']] <- get_support_5(paragraphs_pruned)
  index_any[['support_6']] <- get_support_6(paragraphs_pruned)
  index_any[['support_7']] <- get_support_7(paragraphs_pruned)
  index_any[['support_8']] <- get_support_8(paragraphs_pruned)
  index_any[['support_9']] <- get_support_9(paragraphs_pruned)
  index_any[['support_10']] <- get_support_10(paragraphs_pruned)
  index_any[['developed_1']] <- get_developed_1(paragraphs_pruned)
  index_any[['received_1']] <- get_received_1(paragraphs_pruned)
  index_any[['received_2']] <- get_received_2(paragraphs_pruned)
  index_any[['recipient_1']] <- get_recipient_1(paragraphs_pruned)
  index_any[['authors_1']] <- get_authors_1(paragraphs_pruned)
  index_any[['authors_2']] <- get_authors_2(paragraphs_pruned)
  index_any[['thank_1']] <- get_thank_1(paragraphs_pruned)
  index_any[['thank_2']] <- get_thank_2(paragraphs_pruned)
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
  index_any[['common_3']] <- get_common_3(paragraphs_pruned)
  index_any[['common_4']] <- get_common_4(paragraphs_pruned)
  index_any[['acknow_1']] <- get_acknow_1(paragraphs_pruned)
  index_any[['disclosure_1']] <- get_disclosure_1(paragraphs_pruned)
  index_any[['disclosure_2']] <- get_disclosure_2(paragraphs_pruned)
  index <- unlist(index_any) %>% unique() %>% sort()

  # Remove potential mistakes
  if (!!length(index)) {

    # Funding info can be within COI statements, as per Ioannidis
    # Comment out until problems arise
    # if (length(unlist(index_any[c("authors_2")]))) {
    #   is_coi <- negate_conflict_1(paragraphs_pruned[min(index) - 1])
    #   index <- index[!is_coi]
    # }


# Difficult to make it work properly because it does not
    # disclosures <- c("disclosure_1", "disclosure_2")
    # if (!!length(unlist(index_any[disclosures]))) {
    #
    #   for (i in 1:seq_along(disclosures)) {
    #
    #     ind <- index_any[[disclosures[i]]]
    #     is_coi_disclosure <- negate_disclosure_1(paragraphs_pruned[ind])
    #     index_any[[disclosures[i]]] <- ind[!is_coi_disclosure]
    #
    #   }
    #
    # }

    is_absent <- negate_absence_1(paragraphs_pruned[index])
    index <- index[!is_absent]

    # Currently removed b/c I made the disclosure functions more robust to
    #     statements like "Financial disclosure. Nothing to disclose.
    # disclosures <- unique(unlist(index_any[c("disclosure_1", "disclosure_2")]))
    # if (!!length(disclosures)) {
    #
    #   if (length(disclosures) == 1) {
    #
    #     is_disclosure <- negate_disclosure_2(paragraphs[index])
    #     index <- index[!is_disclosure]
    #
    #   } else {
    #
    #     disclosure_text <- paste(paragraphs_pruned[disclosures], collapse = " ")
    #     is_disclosure <- negate_disclosure_2(disclosure_text)
    #     index <- setdiff(index, disclosures)
    #
    #   }
    # }
  }


  # Identify potentially missed signals
  if (!length(index)) {

    from <- find_acknows(paragraphs_pruned)
    to <- find_refs(paragraphs_pruned) - 1

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
.max_words <- function(x, n_max = 3, space_first = T) {

  if (space_first) {

    # This increases time by at least a few seconds each time used!
    paste0(x, "(?:\\s+\\w+){0,", n_max, "}")

  } else {

    paste0(x, "(?:\\w+\\s+){0,", n_max, "}")

  }
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

    return(paste0(x, "(|:|\\.)"))
    # stricter: "( [A-Z][a-zA-Z]|:|\\.)", avoided b/c Funding sources none.

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

  synonyms[["txt"]] <- "[a-zA-Z0-9\\s,()/:-]*"  # order matters

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
    "Our",
    "Research"
  )

  synonyms[["this"]] <- c(
    "[Tt]his",
    "[Tt]hese",
    "[Tt]he",
    "[Oo]ur"
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
    "are",
    "have",
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
    "from",
    "within",
    "under"
  )

  synonyms[["and"]] <- c(
    "and",
    "&",
    "or"
  )

  synonyms[["for"]] <- c(
    "for"
  )

  synonyms[["of"]] <- c(
    "of",
    "about"
  )

  synonyms[["for_of"]] <- c(
    synonyms[["for"]],
    synonyms[["of"]]
  )

  synonyms[["no"]] <- c(
    "[Nn]o",
    "[Nn]il",
    "[Nn]one",
    "[Nn]othing"
  )

  synonyms[["No"]] <- c(
    "N(?i)o(?-i)",
    "N(?i)il(?-i)",
    "N(?i)one(?-i)",
    "N(?i)othing(?-i)"
  )

  synonyms[["not"]] <- c(
    "not"
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
    "[Pp]rogram(|s)",
    "[Pp]aper(|s)",
    "[Mm]anuscript(|s)"
  )

  synonyms[["research_singular"]] <- c(
    "[Ww]ork",
    "[Rr]esearch",
    "[Ss]tudy",
    "[Pp]roject",
    "[Tt]rial",
    "[Pp]ublication",
    "[Rr]eport",
    "[Pp]rogram",
    "[Pp]aper",
    "[Mm]anuscript"
  )

  synonyms[["researches"]] <- c(
    "[Ww]orks",
    "[Ss]tudies",
    "[Pp]rojects",
    "[Tt]rials",
    "[Pp]ublications",
    "[Rr]eports",
    "[Pp]rograms",
    "[Pp]apers",
    "[Mm]anuscripts"
  )

  synonyms[["funder"]] <- c(
    "[Ff]under",
    "[Ss]ponsor",
    "[Ss]upporter"
  )

  synonyms[["funds"]] <- c(
    "[Ff]und(|s)",
    "[Ff]unding",
    "[Ss]elf-funding"
  )

  synonyms[["funded"]] <- c(
    "[Ff]unded",
    "[Ss]elf-funded",
    "[Ff]inanced",
    "[Ss]upported",
    "[Ss]ponsored",
    "[Rr]esourced",
    "[Aa]ided"
  )

  synonyms[["funding"]] <- c(
    "[Ff]unding",
    "[Ff]unds",
    "[Ss]elf-funding",
    # "[Ff]inancial",
    "[Ff]und support(|s)",
    "[Ss]upport",
    "[Ss]ponsorship",
    "[Aa]id",
    "[Rr]esources"
  )

  synonyms[["funded_funding"]] <- c(
    synonyms[["funded"]],
    synonyms[["funding"]]
  )

  synonyms[["funding_financial"]] <- c(
    synonyms[["funding"]],
    synonyms[["financial"]]
  )

  synonyms[["funding_title"]] <- c(
    "F(?i)unding(?-i)",
    "F(?i)unding/Support(?-i)",
    "F(?i)unding source(|s)(?-i)",
    "S(?i)ource(|s) of funding(?-i)",
    "F(?i)unding source(|s) for the stud(y|ies)(?-i)",
    "F(?i)unding information(?-i)",
    "F(?i)unding statement(|s)(?-i)",
    "S(?i)upport statement(|s)(?-i)",
    "S(?i)ource(|s) of support(|s)(?-i)",
    "S(?i)ource(|s) of funding(?-i)"
  )

  synonyms[["financial"]] <- c(
    "[Ff]inancial support(|s)",
    "[Ff]inancial or other support(|s)",
    "[Ff]inancial assistance",
    "[Ff]inancial aid(|s)",
    "[Ff]inancial sponsorship(|s)",
    "[Ff]inancial support(|s) and sponsorship(|s)",
    # "[Ff]inancial disclosure(|s)",
    # "[Ff]inancial declaration(|s)",
    "[Ff]inanciamento",
    "[Gg]rant support(|s)",
    "[Gg]rant assistance",
    "[Gg]rant aid(|s)",
    "[Gg]rant sponsorship(|s)"
  )

  synonyms[["financial_title"]] <- c(
    "F(?i)inancial support(|s)(?-i)",
    "S(?i)ource(|s) of financial support(|s)(?-i)",
    "F(?i)inancial or other support(|s)(?-i)",
    "F(?i)inancial assistance(?-i)",
    "F(?i)inancial aid(?-i)",
    "F(?i)inancial sponsorship(|s)(?-i)",
    "F(?i)inancial support(|s) and sponsorship(|s)(?-i)",
    # "F(?i)inancial disclosure(|s)(?-i)",
    # "F(?i)inancial declaration(|s)(?-i)",
    "F(?i)inanciamento(?-i)"
  )

  synonyms[["any_title"]] <- c(
    synonyms[["funding_title"]],
    synonyms[["financial_title"]]
  )

  synonyms[["disclosure_title"]] <- c(
    "F(?i)unding disclosure(|s)(?-i)",
    "F(?i)unding disclosure(|s)(?-i)",
    "F(?i)inancial disclosure(|s)(?-i)",
    "F(?i)inancial declaration(|s)(?-i)",
    "F(?i)inancial (&|and) competing interests disclosure(|s)(?-i)",
    "F(?i)inancial (&|and) competing interests declaration(|s)(?-i)",
    "D(?i)isclosure(|s)(?-i)",
    "D(?i)eclaration(|s)(?-i)"
  )

  synonyms[["support"]] <- c(
    "[Ss]upport(|s)",
    "[Aa]id(|s)",
    "[Aa]ssistance",
    "[Ss]ponsorship(|s)"
  )

  synonyms[["support_only"]] <- c(
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

   synonyms[["grant_title"]] <- c(
     "G(?i)rant(|s)(?-i)",
     "G(?i)rant sponsor(|s|ship(|s))(?-i)",
     "G(?i)rant support(|s)(?-i)",
     "G(?i)rant assistance(?-i)",
     "G(?i)rant aid(|s)(?-i)",
     "^[A-Z]\\w+ grant sponsor(|s|ship(|s))(?-i)"
     # removed Sponsorship and sponsors b/c FP without TP
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
     "[Dd]isclosed",
     "[Dd]eclared",
     "[Ss]upplied",
     "[Pp]resented"
   )

   synonyms[["recipient"]] <- c(
     "[Rr]ecipient(|s)",
     "[Aa]wardee(|s)",
     "[Gg]rantee(|s)"
   )


   synonyms[["provide"]] <- c(
     "provid(ed|ing)",
     "g(ave|iving)",
     "award(ed|ing)"
   )

   synonyms[["thank"]] <- c(
     "[Tt]hank(|ful)",
     "[Aa]cknowledge",
     "[Dd]isclose",
     "[Gg]rateful"
   )

   synonyms[["conflict"]] <- c(
     "[Cc]onflict(|ing)",
     "[Cc]ompet(e|ing)",
     "source(|s) of bias"
     # "[Cc]onflits",
     # "[Cc]onflictos",
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
     "D(?i)uality of interest(?-i)",
     "S(?i)ource(|s) of bias(?-i)"
   )

   synonyms[["relationship"]] <- c(
     "relation(|s|ship(|s))",
     "association(|s)",
     "involvement(|s)",
     "affiliation(|s)",
     "tie(|s)"
   )

   synonyms[["info"]] <- c(
     "info(|rmation)\\b",
     "detail(|s)",
     "particulars",
     "data\\b",
     "material\\b"
   )

   synonyms[["acknowledge"]] <- c(
     "acknowledge",
     "recognize",
     "disclose",
     "declare",
     "report",
     "appreciate"
   )

   synonyms[["acknowledged"]] <- c(
     "acknowledged",
     "recognized",
     "disclosed",
     "declared",
     "reported",
     "appreciated"
   )

   synonyms[["foundation"]] <- c(
     "[Ff]oundation(|s)",
     "[Ii]nstitut(e|es|ution)",
     "[Uu]niversit",  # to cover for say German Universitaet
     # "Department(|s)",  # too sensitive
     "[Aa]cadem(y|ies)",
     "[Mm]inistr(y|ies)",
     "[Gg]overnment(|s)",
     "[Cc]ouncil(|s)",
     "[Nn]ational",
     "NIH",
     "NSF",
     "HHMI",
     "[Tt]rust(|s)",
     "[A]ssociation(|s)",
     "Societ(y|ies)",
     "College(|s)",
     "Commission(|s)",
     "Center(|s)",
     "Office(|s)",
     "Program(|s)",
     "Alliance(|s)",
     "[Aa]gency"
   )

   synonyms[["foundation_award"]] <- c(
     synonyms[["foundation"]],
     synonyms[["award"]]
   )

  return(synonyms)
}
