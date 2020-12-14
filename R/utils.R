' Remove fullstops that are unlikely to represent end of sentence
#'
#' Returns the list of paragraphs without potentially misleading fullstops.
#'


#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without misleading fullstops.
.obliterate_fullstop_1 <- function(article) {

  j_p_a_i <- "([A-Z])(\\.)\\s*([A-Z])(\\.)\\s*([A-Z])(\\.)"

  article %>%
    stringr::str_replace_all(j_p_a_i, "\\1 \\3 \\5") %>%
    stringr::str_replace_all("([A-Z])(\\.)\\s*([A-Z])(\\.)", "\\1 \\3") %>%
    stringr::str_replace_all("(\\s[A-Z])(\\.) ([A-Z][a-z]+)", "\\1 \\3") %>%
    stringr::str_replace_all("\\.\\s*([a-z0-9])", " \\1") %>%
    stringr::str_replace_all("\\.([A-Z])", " \\1") %>%
    stringr::str_replace_all("\\.\\s*([A-Z]+[0-9])", " \\1") %>%
    stringr::str_replace_all("\\.([^\\s0-9\\[])", "\\1") %>%
    stringr::str_replace_all("\\.\\s+(\\()", " \\1") %>%
    stringr::str_replace_all("([0-9])\\.([0-9])", "\\1\\2") %>%
    stringr::str_replace_all("\\.(\\s*[[:punct:]])", "\\1")

}


#' Remove semicolons when within parentheses
#'
#' Removes mentions such as: "guidelines for diagnostic studies (trial
#'     registered at www.clinicaltrial.gov; NCT01697930)."
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
.obliterate_semicolon_1 <- function(article) {

  article %>% stringr::str_replace_all("(\\(.*); (.*\\))", "\\1 - \\2")

}


#' Remove commas
#'
#' Removes commas to simplify regular expressions.
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
.obliterate_comma_1 <- function(article) {

  article %>% stringr::str_replace_all(", ", " ")

}


#' Remove apostrophe
#'
#' Removes commas to make ease creation of regular expressions. After
#'     implmenting this function, "ball's" should become balls and l'Alba
#'     should become lAlba and balls' into balls.
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
.obliterate_apostrophe_1 <- function(article) {

  article %>%
    stringr::str_replace_all("([a-zA-Z])'([a-zA-Z])", "\\1\\2") %>%
    stringr::str_replace_all("[a-z]+s'", "s")

}


#' Remove hash
#'
#' Removes hashes to make ease creation of regular expressions.
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
.obliterate_hash_1 <- function(article) {

  article %>% stringr::str_replace_all("#", "")

}


#' Remove uninformative punctuation
#'
#' Removes irrelevant puncutation to ease creation of regular expressions.
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without backlashes.
.obliterate_punct_1 <- function(article) {

  punct <- '[~@#$%^&*{}_+"<>?/=]'  # not :()[] b/c they are informative
  article %>% stringr::str_replace_all(punct, "")
  # do not add a space, otherwise will mess with other functions
  # e.g. get_ct_2, which only allows 2 words between This and study

}


#' Remove break of line tags.
#'
#' Removes missed breaks of line
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs with no remaining line break tags.
.obliterate_line_break_1 <- function(article) {

  article %>% stringr::str_replace_all("\n", " ")

}


#' Remove references
#'
#' Returns the list of paragraphs without references.
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without misleading fullstops.
.obliterate_refs_1 <- function(article) {

  # Built like this to avoid destabilizing the algorithm
  article <- gsub("^.*\\([0-9]{4}\\).*$", "References", article)
  article <- gsub("^.* et al\\..*$", "References", article)

  return(article)
}



#' Remove author contribution statements
#'
#' Returns the text without the author contribution statements.
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
.obliterate_contribs <- function(article) {

  # Not obliterating "Authors' information" b/c some publications use it to
  # mention funds to each author - need a more sophisiticated approach if I
  # want to do that, but not worth it right now.
  # {0,4} accounts for Author(s)
  contribs <- "Author(|s).{0,4}[Cc]ontribution(|s)"
  txt <- "[a-zA-Z0-9\\s,;()\\[\\]/:-]*"

  contribs_title <- paste0("^", contribs, ".*$")
  contribs_intxt <- paste0(contribs, "\\s*(:|-|\\.)", txt, "\\.")

  article %>%
    stringr::str_replace_all(contribs_title, "") %>%
    stringr::str_replace_all(contribs_intxt, "")
    # not very effective without removing full stops

}


# Function copied from the oddpub package included here to avoid using `:::`.
# Need to use here because this package is re-implementing tokenization.
# Author: Nico Riedel.
.correct_tokenization <- function(PDF_text)
{
  PDF_text_corrected <- PDF_text
  sentence_paste_idx <- PDF_text %>%
    stringr::str_sub(-13, -1) %>%
    stringr::str_detect("accession nr.|accession no.|ccession nos.|ccession nrs.") %>%
    which()

  #for all indicies do a pairwise pasting
  if(length(sentence_paste_idx) > 0)
  {
    for(i in 1:length(sentence_paste_idx))
    {
      PDF_text_corrected <- .paste_idx(PDF_text_corrected, sentence_paste_idx[i]-(i-1))
    }
  }

  return(PDF_text_corrected)
}

#helper function for .correct_tokenization
#pastes together sentences where tokenization needs to be corrected by index
.paste_idx <- function(PDF_text, idx)
{
  #create dummy sentences such that the indexing always works correctly,
  #even with only one element in PDF_text
  PDF_text_pasted <- c("x", PDF_text, "x")
  idx <- idx + 1 #shift idx due to dummy sentence

  PDF_text_pasted <- c(PDF_text_pasted[1:(idx-1)],
                       paste(PDF_text_pasted[idx], PDF_text_pasted[idx+1]),
                       PDF_text_pasted[(idx+2):length(PDF_text_pasted)])
  #remove dummy elemets
  PDF_text_pasted <- PDF_text_pasted[c(-1, -length(PDF_text_pasted))]

  return(PDF_text_pasted)
}


#' Find the index of the references
#'
#' Returns the index with the elements of interest. More generic than _1.
#'
#' @param article A List with paragraphs of interest.
#' @return The index of the start and finish of this section.
.where_refs_txt <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("References")
  next_sentence <- "((|:|\\.)|(|:|\\.) [A-Z0-9]+.*)$"

  ref_index <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(paste0, next_sentence) %>%
    # lapply(.title) %>%
    lapply(.encase) %>%
    # lapply(.max_words) %>%
    paste() %>%
    grep(article, perl = T)

  # ref_synonyms <- c(
  #   "R(?i)eferences(?-i)(| [A-Z0-9]+.*)",
  #   "L(?i)terature(?-i)(| [A-Z0-9]+.*)",
  #   "L(?i)iterature Cited(?-i)(| [A-Z0-9]+.*)",
  #   "N(?i)otes and References(?-i)(| [A-Z0-9]+.*)",
  #   "W(?i)orks Cited(?-i)(| [A-Z0-9]+.*)",
  #   "^C(?i)itations(?-i)(| [A-Z0-9]+.*)",
  #   "B(?i)ibliographic references(?-i)(| [A-Z0-9]+.*)",
  #   "R(?i)eferences and recommended reading(?-i)(| [A-Z0-9]+.*)"
  # )

  # no "^" b/c of UTF-8 characters, e.g. "\\fReferences"
  # regex <- paste0("(", paste(ref_synonyms, collapse = "|"), ")$")
  # ref_index <- grep(regex, article, perl = T)

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
.where_acknows_txt <- function(article) {

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



#' Find the Methods section
#'
#' Find the index of the start of the Methods section.
#'
#' @param article The text as a vector of strings.
#' @return Index of element with phrase of interest
.where_methods_txt  <- function(article) {

  method_index <- integer()

  synonyms <- .create_synonyms()
  words <- c("Methods", "Abstract", "Results", "Conclusion")

  method_index <-
    synonyms %>%
    magrittr::extract(words[1]) %>%
    lapply(.title_strict) %>%
    lapply(stringr::str_sub, end = -2) %>%  # remove the $
    # lapply(paste, "($|\\s+[A-Z]") %>%  # TODO: if too sensitive, uncomment
    lapply(.encase) %>%
    paste() %>%
    grep(article, perl = T)

  if (!!length(method_index)) {

    method_index <- method_index[length(method_index)]
    return(method_index)

    # TODO: if too sensitive, uncomment
    # is_abstract <-
    #   synonyms %>%
    #   magrittr::extract(words[2:4]) %>%
    #   grepl(article[(method_index - 3):(method_index + 3)]) %>%
    #   any()
    #
    # if (!is_abstract) {
    #
    #   return(method_index)
    #
    # }
  }

  method_index <-
    synonyms %>%
    magrittr::extract(words[1]) %>%
    lapply(.title_strict, within_text = T) %>%
    lapply(paste, "[A-Z]", sep = "\\s*") %>%
    lapply(.encase) %>%
    paste() %>%
    grep(article, perl = T)

  if (!!length(method_index)) {

    method_index <- method_index[length(method_index)]

  }

  return(method_index)

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
#' @param space_first If TRUE space first (default), else space last.
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



#' Create a regex for titles
#'
#' Returns words designed to identify titles.
#'
#' @param x A vector of strings.
#' @param within_text Boolean defines whether a regex typical to a title found      within text should be created or not.
#' @return A vector of strings with a suffix attached.
.title_strict <- function(x, within_text = F) {

  if (within_text) {

    return(paste0(x, "( [A-Z][a-zA-Z]|:|\\.|\\s*-+)"))
    # stricter: "( [A-Z][a-zA-Z]|:|\\.)", avoided b/c Funding sources none.

  } else {

    return(paste0("^.{0,4}", x, ".{0,4}$"))

  }
}



#' Create a regular expression where the first letter is capital
#'
#' Returns a regular expression that necessitates that the first letter is
#'     capital and the rest can be any case.
#'
#' @param x A vector of strings.
#' @param location Whether to "start", "end" or "both" with a capital letter.
#' @return A string pattern.
.first_capital <- function(x, location = "both") {

  if (location == "both") {

    return(gsub("^([A-Z])(.*)$", "\\1(?i)\\2(?-i)", x))

  }

  if (location == "start") {

    return(gsub("^(.)(.*)$", "\\1(?i)\\2", x))

  }

  if (location == "end") {

    return(gsub("^(.*)$", "\\1(?-i)", x))

  }


}



#' A list of word synonyms
#'
#' Contains the synonyms to words being used throughout the package.
#'
#' @return A list of synonyms to words of interest
.create_synonyms <- function() {

  # whole list synonyms$ vs synonyms[[]] similar performance 760 vs 755 median
  synonyms <- list()

  synonyms[["txt"]] <- "[a-zA-Z0-9\\s,()\\[\\]/:-]*"  # order matters

  synonyms[["This"]] <- c(
    "This",
    "These",
    "The",
    "Our",
    "All"
  )

  synonyms[["This_singular"]] <- c(
    "This",
    "The",
    "Our"
  )

  synonyms[["These"]] <- c(
    "These",
    "Our",
    "Research",
    "All"
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
    "are",
    "was",
    "were",
    "been"
  )

  synonyms[["is_singular"]] <- c(
    "is",
    "was",
    "have",
    "has"
  )

  synonyms[["are"]] <- c(
    "are",
    "were",
    "have",
    "had"
  )

  synonyms[["have"]] <- c(
    "have",
    "has",
    "had"
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
    "[Mm]anuscript(|s)",
    "[Aa]nalys(is|es)",
    "[Ii]nvestigation(|s)",
    "[Pp]rotocol(|s)",
    "[Cc]ohort(|s)",
    "[Cc]ollaboration(|s)"
  )

  synonyms[["research_strict"]] <- c(
    "[Ww]ork(|s)",
    "[Rr]esearch",
    "[Ss]tud(y|ies)",
    "[Pp]roject(|s)",
    "[Tt]rial(|s)",
    "[Pp]rogram(|s)",
    "[Aa]nalys(is|es)",
    "[Ii]nvestigation(|s)",
    "[Pp]rotocol(|s)",
    "[Cc]ohort(|s)",
    "[Cc]ollaboration(|s)"
  )

  synonyms[["research_lower"]] <- c(
    "work(|s)",
    "research",
    "stud(y|ies)",
    "project(|s)",
    "trial(|s)",
    "publication(|s)",
    "report(|s)",
    "program(|s)",
    "paper(|s)",
    "manuscript(|s)",
    "analys(is|es)",
    "investigation(|s)",
    "protocol(|s)",
    "cohort(|s)",
    "collaboration(|s)"
  )

  synonyms[["research_lower_strict"]] <- c(
    "work(|s)",
    "research",
    "stud(y|ies)",
    "project(|s)",
    "trial(|s)",
    "program(|s)",
    "analys(is|es)",
    "investigation(|s)",
    "protocol(|s)",
    "cohort(|s)",
    "collaboration(|s)"
  )

  synonyms[["Research"]] <- c(
    "Work(|s)",
    "Research",
    "Stud(y|ies)",
    "Project(|s)",
    "Trial(|s)",
    "Publication(|s)",
    "Report(|s)",
    "Program(|s)",
    "Paper(|s)",
    "Manuscript(|s)",
    "Analys(is|es)",
    "Investigation(|s)"
  )

  synonyms[["Research_strict"]] <- c(
    "Work(|s)",
    "Research",
    "Stud(y|ies)",
    "Project(|s)",
    "Trial(|s)",
    "Program(|s)",
    "Analys(is|es)",
    "Investigation(|s)"
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
    "[Mm]anuscript",
    "[Aa]nalysis",
    "[Ii]nvestigation"
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
    "[Mm]anuscripts",
    "[Aa]nalyses",
    "[Ii]nvestigations"
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
    "\\b[Aa]id",
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

  # TODO: split this into (financial|funding) (support|source|etc)
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
    "S(?i)ource(|s) of funding(?-i)",
    "F(?i)unding and (|potential )(conflict(|s)|competing) (|of )interest(|s)(?-i)"
  )

  synonyms[["financial"]] <- c(
    "[Ff]inancial support(|s)",
    "[Ff]inancial source(|s)",
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


  # TODO: split this into (financial|funding) (support|source|etc)
  synonyms[["financial_title"]] <- c(
    "F(?i)inancial support(|s)(?-i)",
    "F(?i)inancial source(|s)(?-i)",
    "S(?i)ource(|s) of financial support(|s)(?-i)",
    "F(?i)inancial or other support(|s)(?-i)",
    "F(?i)inancial assistance(?-i)",
    "F(?i)inancial aid(?-i)",
    "F(?i)inancial sponsorship(|s)(?-i)",
    "F(?i)inancial support(|s) and sponsorship(|s)(?-i)",
    "F(?i)inancial source(|s) for the stud(y|ies)(?-i)",
    "F(?i)inancial information(?-i)",
    "F(?i)inancial statement(|s)(?-i)",
    # "F(?i)inancial disclosure(|s)(?-i)",
    # "F(?i)inancial declaration(|s)(?-i)",
    "F(?i)inanciamento(?-i)"
  )

  synonyms[["any_title"]] <- c(
    synonyms[["funding_title"]],
    synonyms[["financial_title"]],
    synonyms[["grant_title"]]
  )

  synonyms[["disclosure_title"]] <- c(
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
    "\\b[Aa]id(|s)",
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
    "(?<![Ss]pecialty) [Ff]ellowship(|s)",
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

  synonyms[["receive"]] <- c(
    "received",
    "own(|s)",
    "hold(|s)",
    "ha(s|ve)",
    "charged",
    "invent(ed|or|ors)",
    "declare"
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

  synonyms[["received_strict"]] <- c(
    "[Rr]eceived",
    "[Aa]ccepted",
    "[Aa]cquired",
    "[Gg]iven",
    "[Oo]ffered",
    "[Dd]isclose(|d)",
    "[Dd]eclare(|d)"
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
    "Ffoundation(|s)",
    "Institut(e|es|ution)",
    "Universit",  # to cover for say German Universitaet
    "Universit(y|ies)",
    # "Department(|s)",  # too sensitive
    "Academ(y|ies)",
    "Ministr(y|ies)",
    "[Gg]overnment(|s)",
    "Council(|s)",
    "National",
    "NIH",
    "NSF",
    "HHMI",
    "Trust(|s)",
    "Association(|s)",
    "Societ(y|ies)",
    "College(|s)",
    "Commission(|s)",
    "Center(|s)",
    "[Oo]ffice(|s)",
    "[Pp]rogram(|s)",
    "[Aa]lliance(|s)",
    "[Aa]gency"
  )

  synonyms[["foundation_award"]] <- c(
    synonyms[["foundation"]],
    synonyms[["award"]]
  )

  synonyms[["References"]] <- c(
    "R(?i)eferences(?-i)",
    "L(?i)terature(?-i)",
    "L(?i)iterature Cited(?-i)",
    "N(?i)otes and References(?-i)",
    "W(?i)orks Cited(?-i)",
    "^C(?i)itations(?-i)",
    "B(?i)ibliograpy(?-i)",
    "B(?i)ibliographic references(?-i)",
    "R(?i)eferences and recommended reading(?-i)"
  )

  synonyms[["Methods"]] <- c(
    ".{0,4}M(?i)ethod(|s)(?-i)",
    ".{0,4}O(?i)nline method(|s)(?-i)",
    ".{0,4}M(?i)aterial(|s) and Method(|s)(?-i)",
    ".{0,4}M(?i)aterial(|s)/Method(|s)(?-i)",
    ".{0,4}M(?i)ethod(|s) and Material(|s)(?-i)",
    ".{0,4}M(?i)ethod(|s)/Material(|s)(?-i)",
    ".{0,4}S(?i)ubjects and method(|s)(?-i)",
    ".{0,4}P(?i)atients and method(|s)(?-i)",
    ".{0,4}P(?i)articipants and method(|s)(?-i)",
    ".{0,4}M(?i)ethods and preliminary analysis(?-i)",
    ".{0,4}E(?i)xperimental section(?-i)",
    ".{0,4}M(?i)ethodology(?-i)",
    ".{0,4}M(?i)etodologia(?-i)",
    ".{0,4}M(?i)etodologie(?-i)"
  )

  synonyms[["Abstract"]] <- c(
    "Abstract",
    "Synopsis",
    "Summary"
  )

  synonyms[["Introduction"]] <- c(
    "Introduction",
    "Background"
  )

  synonyms[["Results"]] <- c(
    "Results",
    "Findings"
  )

  synonyms[["Conclusion"]] <- c(
    "Conclusion",
    "Interpretation"
  )

  synonyms[["sources"]] <- c(
    "source(|s)"
  )

  synonyms[["register"]] <- c(
    "register"
  )

  synonyms[["registered"]] <- c(
    "registered"
  )

  synonyms[["registration"]] <- c(
    "registration"
  )

  synonyms[["registry"]] <- c(
    "[Rr]egistr(y|ies)"
  )

  synonyms[["registered_registration"]] <- c(
    synonyms[["registered"]],
    synonyms[["registration"]]
  )

  # synonyms[["registration_title_1"]] <- c(
  #   "Registration",
  #   "Trial(|s)",
  #   "Clinical trial(|s)"
  #   # "Protocol(|s)"  # only contributed FPs due to protocol ethical approval
  # )

  # synonyms[["registration_title_2"]] <- c(
  #   "info(|rmation)\\b",
  #   "detail(|s)\\b",
  #   "no(|s)(|\\.)",
  #   "number(|s)",
  #   # "[Ii][Dd](|s)", # only contributes to FPs
  #   "identifier(|s)",
  #   "registration"
  # )

  # synonyms[["registration_title_3"]] <- c(
  #   paste(c(
  #     "Work", "Research", "Study", "Project", "Program", "Report"),
  #     "registration"
  #   ),
  #   "Registration"
  # )

  # synonyms[["registration_title"]] <-
  #   synonyms[["registration_title_3"]] %>%
  #   .first_capital()

  # Captures "Trial details" and used with negation - worth the duplication
  # synonyms[["registration_title"]] <- expand.grid(
  #   synonyms[["registration_title_1"]],
  #   synonyms[["registration_title_2"]]
  # ) %>%
  #   purrr::pmap(paste, sep = " ") %>%
  #   unlist() %>%
  #   append(synonyms[["registration_title_3"]]) %>%
  #   .first_capital()

  # Adding the top 4 adds negligible amount of time
  synonyms[["registration_title"]] <- c(
    "T(?i)rial(|s) info(|rmation)\\b(?-i)",
    "C(?i)linical trial(|s) info(|rmation)\\b(?-i)",
    "T(?i)rial(|s) detail(|s)\\b(?-i)",
    "C(?i)linical trial(|s) detail(|s)\\b(?-i)",
    "W(?i)ork registration(?-i)",
    "R(?i)esearch registration(?-i)",
    "S(?i)tudy registration(?-i)",
    "P(?i)roject registration(?-i)",
    "P(?i)rogram registration(?-i)",
    "R(?i)eport registration(?-i)",
    "R(?i)egistration(?-i)"
  )

  synonyms[["protocol"]] <- c(
    "protocol"
  )

  synonyms[["study protocol"]] <- c(
    "research protocol",
    "study protocol",
    "trial protocol",
    "analysis protocol",
    "investigation protocol",
    "research design",
    "study design",
    "trial design"
  )

  synonyms[["published"]] <- c(
    "published",
    "reported",
    "made available",
    "posted",
    "issued"
  )

  synonyms[["previously"]] <- c(
    "previously",
    "before",
    "already"
  )

  # Not strict because they are meant to be used with no regex restrictions
  # and with ignore.case = T. In 3909 XML files searching through titles,
  # this worked perfectly with 100% specificity
  synonyms[["conflict_title"]] <- c(
    "C(?i)onflict(|s) of interest(|s)(?-i)",
    "C(?i)onflict(|s) of interest(|s) declaration(?-i)",
    "C(?i)onflicting interest(|s)(?-i)",
    "C(?i)onflicting interest(|s) declaration(?-i)",
    "C(?i)onflicting financial intere",
    "C(?i)onflicting of interest(|s)(?-i)",
    "C(?i)onflits d'int(?-i)",
    "C(?i)onflictos de Inter(?-i)",
    "C(?i)ompeting interest(|s)(?-i)",
    "C(?i)ompeting interest(|s) declaration(?-i)",
    "C(?i)ompeting of interest(|s)(?-i)",
    "C(?i)ompeting financial interest(|s)(?-i)",
    "D(?i)eclaration of interest(|s)(?-i)",
    "D(?i)eclaration of conflicting interest(|s)(?-i)",
    "D(?i)uality of interest(|s)(?-i)",
    "S(?i)ource(|s) of bias(?-i)",
    "F(?i)unding and (|\\w+ )(conflict(|s)|competing) (|of )interest(|s)(?-i)"
  )

  # Made to work with regex restrictions, e.g. "^...$"
  # \\w+: mostly "Potential|Declaration of" at start and "statement" at end
  synonyms[["conflict_title_strict"]] <- c(
    "(|\\w+ )C(?i)onflict(|s) of interest(|s)(?-i)(| \\(COI\\)| \\w+)",
    "(|\\w+ )C(?i)onflicting interest(|s)(?-i)(| \\(COI\\)| \\w+)",
    "(|\\w+ )C(?i)onflicting financial interest(|s)(?-i)(| \\w+)",
    "(|\\w+ )C(?i)onflicting of interest(|s)(?-i)(| \\(COI\\)| \\w+)",
    "(|\\w+ )C(?i)onflits d'int(?-i)",
    "(|\\w+ )C(?i)onflictos de Inter(?-i)",
    "(|\\w+ )C(?i)ompeting interest(|s)(?-i)(| \\w+)",
    "(|\\w+ )C(?i)ompeting of interest(|s)(?-i)(| \\(COI\\)| \\w+)",
    "(|\\w+ )C(?i)ompeting financial interest(|s)(?-i)(| \\w+)",
    "D(?i)eclaration of interest(|s)(?-i)(| \\w+)",
    "D(?i)eclaration of conflicting interest(|s)(?-i)",
    "D(?i)uality of interest(|s)(?-i)(| \\w+)",
    "(|\\w+ )S(?i)ource(|s) of bias(?-i)(| \\w+)",
    "F(?i)unding and (|\\w+ )(conflict(|s)|competing) (|of )interest(|s)(?-i)"
  )

  synonyms[["conflict"]] <- c(
    "[Cc]onflict(|ing)(|s)",
    "[Cc]ompet(e|ing)",
    "source(|s) of bias",
    "[Cc]onflits",
    "[Cc]onflictos"
  )

  synonyms[["disclose"]] <- c(
    "disclose(|s)",
    "declare(|s)",
    "state(|s)",
    "disclaim(|s)",
    "report(|s)",
    "acknowledge(|s)"
  )

  synonyms[["disclosure"]] <- c(
    "disclosure",
    "declaration",
    "statement",
    "disclaimer",
    "acknowledgement",
    "reporting"
  )

  synonyms[["disclosure_coi_title"]] <- c(
    "F(?i)inancial disclosure(|s)(?-i)",
    "F(?i)inancial declaration(|s)(?-i)",
    "F(?i)inancial (&|and) competing interests disclosure(|s)(?-i)",
    "F(?i)inancial (&|and) competing interests declaration(|s)(?-i)",
    "D(?i)isclosure(|s)(?-i)",
    "D(?i)eclaration(|s)(?-i)"
  )

  synonyms[["no_financial_disclosure"]] <- c(
    "[Nn]othing to (disclose|declare|report|acknowledge)",
    "[Nn]o financial disclosure(|s)",
    "[Nn]o [a-z]+ financial disclosure(|s)",
    "[Nn]o [a-z]+ [a-z]+ financial disclosure(|s)"
  )

  synonyms[["commercial"]] <- c(
    "commercial(|ly)",
    "financial(|y)"
  )

  synonyms[["commercial_strict"]] <- c(
    "commercial(|ly)"
  )

  synonyms[["relationship"]] <- c(
    "relation(|s|ship(|s))",
    "connection(|s)",
    "association(|s)",
    "involvement(|s)",
    "affiliation(|s)",
    "tie(|s)",
    "contract(|s)"
  )

  synonyms[["related"]] <- c(
    "related",
    "connected",
    "associated",
    "involved",
    "affiliated",
    "tied",
    "contracted"
  )

  synonyms[["relationship_strict"]] <- c(
    "relation(|s|ship(|s))",
    "connection(|s)",
    "association(|s)",
    "involvement(|s)",
    "tie(|s)"
  )

  synonyms[["relationship_strict"]] <- c(
    "related",
    "connected",
    "associated",
    "involvemed",
    "tied"
  )

  synonyms[["interests"]] <- c(
    "gain(|s)",
    "benefit(|s)",
    "interest(|s)"
  )

  synonyms[["stock"]] <- c(
    "stock(|s)",
    "shares",
    "bonds"
  )

  synonyms[["fees"]] <- c(
    "fe(e|es)",
    "compensation",
    "payment",
    "honorari(um|a)",
    "sponsorship"
    # "grants"  # too general
  )

  synonyms[["consultant"]] <- c(
    "consultant(|s)",
    "advis(or(|s)|er(|s))",
    "board member(|s)",
    "member(|s) of the board"
    # "advisory board",  # too general
    # "scientific board"  # too general
  )

  synonyms[["consult"]] <- c(
    "consult(|s|ing)",
    "advise(|s)",
    "counsel(|s)"
    # "board(|s)"
    # "advisory board(|s)",  # too general
    # "scientific board(|s)"  # too general
  )

  synonyms[["consult_all"]] <- c(
    "consult(|s|ant(|s))",
    "advis(e(|s)|or(|s)|er(|s))",
    "counsel(|s)"
    # "board(|s)"  #
  )

  synonyms[["speaker"]] <- c(
    "speaker",
    "presenter",
    "lectur(e|es|ing)",
    "employee(|s)"
  )

  synonyms[["proprietary"]] <- c(
    "proprietary",
    "patent(|s)",
    "copyright(|s)",
    "license(|s)",
    "rights",
    "permit(|s)",
    "priviledge(|s)",
    "franchise(|s)"
  )

  synonyms[["founder"]] <- c(
    "founder",
    "co(|-)founder",
    "founding member"
  )

  synonyms[["played"]] <- c(
    "played",
    "had"
  )

  synonyms[["role"]] <- c(
    "role",
    "hand",
    "part",
    "involvement"
  )

  return(synonyms)
}
