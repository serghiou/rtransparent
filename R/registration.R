#' Identify mentions of registration on ClinicalTrials.gov
#'
#' Extract the index of mentions such as: "The study is registered at
#'     www.clinicaltrials.gov (NCT01624883)."
#'
#' @return Index of element with phrase of interest
get_ct_1 <- function(article) {

  # Just using the NCT was too sensitive
  # e.g. picked up references to protocols, mentions of trials underway, etc.
  grep("regist.*NCT[0-9]{8}", article, perl = T)

}


#' Identify mentions of registration on ClinicalTrials.gov
#'
#' Extract the index of mentions such as: "The study (EudraCT 2011‐001925‐26;
#'     ClinicalTrial.gov NCT01489592) was approved by the Ethics Committee of
#'     Rennes University Hospital."
#'
#' @return Index of element with phrase of interest
get_ct_2 <- function(article) {

  grep("[Cc]linical[Tt]rial.* NCT[0-9]{8}", article, perl = T)

}


#' Identify mentions of registration on PROSPERO
#'
#' Extract the index of mentions such as: "We registered the protocol for this
#'     meta-analysis with the PROSPERO database (www.crd.york.ac.uk/prosper
#'     o)—registration no. CRD42014015595."
#'
#' @return Index of element with phrase of interest
get_prospero_1 <- function(article) {

  # Just using the NCT was too sensitive
  # e.g. picked up references to protocols, mentions of trials underway, etc.
  grep("PROSPERO.*[A-Z]{2}\\s*[0-9]{5}", article, perl = T)

}


#' Identify generic mentions of registration
#'
#' Extract the index of mentions such as: "The trial was registered with
#'     controlled-trials.com (ISRCTN 10627379)"
#'
#' @return Index of element with phrase of interest
get_registered_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("research", "register_all")  # Too generic without these

  this_research_registered <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt)

  c(this_research_registered, "([A-Z]{2}\\s*[0-9]{2}|[0-9]{5})") %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify generic mentions of registration
#'
#' Extract the index of mentions such as: "registered with Clinical Trials
#'     (ChiCTR-IOR-14005438)"
#'
#' @return Index of element with phrase of interest
get_registered_2 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("register_all", "research")  # Too generic without these

  c("[Rr]egistered", "([Tt]rial|[Ss]tudy)") %>%
    paste(collapse = synonyms$txt) %>%
    paste("([A-Z]{2}\\s*[0-9]{2}|[0-9]{5})", sep = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify registration titles - sensitive
#'
#' Extract the index of mentions such as: "The trial was registered with
#'     controlled-trials.com (ISRCTN 10627379)"
#'
#' @return Index of element with phrase of interest
get_reg_title_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("registration_title")

  a <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
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

    synonyms %>%
      magrittr::extract(words) %>%
      lapply(.bound) %>%
      lapply(.title, within_text = T) %>%
      lapply(.encase) %>%
      paste() %>%
      grep(article, perl = T)

  }

}


#' Identify registration titles - specific
#'
#' Extract the index of mentions such as: "Retrospective clinical trial
#'     registration:"
#'
#' @return Index of element with phrase of interest
get_reg_title_2 <- function(article) {

  synonyms <- .create_synonyms()

  registration_synonyms <- c(
    "[Rr]egistration",
    "[Cc]linical [Tt]rial",
    "[Tt]rial",
    "[Pp]rotocol"
  )

  a <-
    registration_synonyms %>%
    .encase() %>%
    paste0("^\\s*(|[A-Z]\\w+ )", ., " \\w+(\\.|:)$") %>%
    grep(article, perl = T)

  if (!!length(a)) {

    a <- max(a)

    if (nchar(article[a + 1]) == 0) {
      return(c(a, a + 2))
    } else {
      return(c(a, a + 1))
    }

  } else {

    registration_synonyms %>%
      .encase() %>%
      paste0("^\\s*(|[A-Z]\\w+ )", ., " \\w+(\\.|:)") %>%
      grep(article, perl = T)

  }
}


#' Identify mentions of protocol
#'
#' Extract the index of mentions such as: "Alliance for Clinical Trials in
#'     Oncology (formerly Cancer and Leukemia Group B) Protocol #369901"
#'
#' @return Index of element with phrase of interest
get_protocol_1 <- function(article) {

  grep("[Pp]rotocol .{0,5}(|[A-Z]+)[0-9]{5}", article, perl = T)

}


#' Find the Methods section
#'
#' Find the index of the start of the Methods section.
#'
#' @return Index of element with phrase of interest
find_methods <- function(article) {

  method_index <- integer()

  synonyms <- .create_synonyms()
  words <- c("Methods", "Abstract", "Results", "Conclusion")

  method_index <-
    synonyms %>%
    magrittr::extract(words[1]) %>%
    lapply(.title) %>%
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
    lapply(.title, within_text = T) %>%
    lapply(paste, "[A-Z]", sep = "\\s*") %>%
    lapply(.encase) %>%
    paste() %>%
    grep(article, perl = T)

  if (!!length(method_index)) {

    method_index <- method_index[length(method_index)]

  }

  return(method_index)

}



#' Identify statements of registration
#'
#' Returns all text mentions of phrases related to registration.
#'
#' @param filename A List with paragraphs of interest.
#' @return A dataframe indicating whether a registration statement has been
#'     identified and the registration statement.
#' @export
rt_register <- function(filename) {

  # TODO: Consider removing all :punct: apart from dots (e.g. author(s))

  index <- integer()
  is_relevant <- FALSE

  file_text <- readr::read_file(filename)

  if (any(grepl("[Rr]egist|[Tt]rial", file_text))) {

    is_relevant <- TRUE

    # TODO: MOVE THIS TO THE pdf2text FUNCTION AND ENCODE AS UTF-8
    # Fix PDF to txt bugs
    broken_1 <- "([a-z]+)-\n*([a-z]+)"
    broken_2 <- "([a-z]+)(|,|;)\n*([a-z]+)"
    paragraphs <-
      file_text %>%
      purrr::map(gsub, pattern = broken_1, replacement = "\\1\\2") %>%
      purrr::map(gsub, pattern = broken_2, replacement = "\\1\\3") %>%
      purrr::map(strsplit, "\n| \\*") %>%
      unlist() %>%
      utf8::utf8_encode()


    # TODO: MOVE UP TO obliterate_fullstop_1 TO pdf2text FUNCTION
    # Remove potentially misleading sequences
    utf_1 <- "(\\\\[a-z0-9]{3})"   # remove \\xfc\\xbe etc
    utf_2 <- "(\\\\[a-z])"   # \\f or
    paragraphs_pruned <-
      paragraphs %>%
      purrr::map_chr(gsub, pattern = utf_1, replacement = " ", perl = T) %>%
      purrr::map_chr(gsub, pattern = utf_2, replacement = "",  perl = T) %>%
      obliterate_fullstop_1()


    # Identify sequences of interest
    index_any <- list()
    index_any[["ct_1"]] <- get_ct_1(paragraphs_pruned)
    index_any[["prospero_1"]] <- get_prospero_1(paragraphs_pruned)
    index_any[["registered_1"]] <- get_registered_1(paragraphs_pruned)
    index_any[["registered_2"]] <- get_registered_2(paragraphs_pruned)
    index_any[["reg_title_1"]] <- get_reg_title_1(paragraphs_pruned)
    index_any[["reg_title_2"]] <- get_reg_title_2(paragraphs_pruned)
    index <- unlist(index_any) %>% unique() %>% sort()


    # Apply a more sensitive search in Methods
    if (!length(index)) {

      from <- find_methods(paragraphs_pruned)

      if (!!length(from)) {

        to <- from + 10

        index_method <- list()
        index_method[["ct_2"]] <- get_ct_2(paragraphs_pruned[from:to])
        index <- unlist(index_method) %>% magrittr::add(from - 1)
      }
    }


    is_register_pred <- !!length(index)
    register_text <- paragraphs[index] %>% paste(collapse = " ")

  } else {

    is_register_pred <- FALSE
    register_text <- ""

}

  article <- basename(filename) %>% stringr::word(sep = "\\.")
  pmid <- gsub("^.*PMID([0-9]+).*$", "\\1", filename)
  tibble::tibble(article, pmid, is_register_pred, register_text, is_relevant)

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


#' Create a regular expression where the first letter is capital
#'
#' Returns a regular expression that necessitates that the first letter is
#'     capital and the rest can be any case.
#'
#' @param x A vector of strings.
#' @return A string pattern.
.first_capital <- function(x) {

  gsub("^([A-Z])(.*)$", "\\1(?i)\\2(?-i)", x)

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
    "[Mm]anuscript(|s)",
    "[Aa]nalys(is|es)",
    "[Ii]nvestigation(|s)"
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
    "S(?i)ource(|s) of funding(?-i)"
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

  synonyms[["info_2"]] <- c(
    "info(|rmation)",
    "detail(|s)",
    "particulars",
    "data",
    "material"
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
    ".{0,4}M(?i)aterial(|s) and Method(|s)(?-i)",
    ".{0,4}M(?i)ethod(|s) and Material(|s)(?-i)",
    ".{0,4}S(?i)ubjects and method(|s)(?-i)",
    ".{0,4}P(?i)atients and method(|s)(?-i)",
    ".{0,4}P(?i)articipants and method(|s)(?-i)",
    ".{0,4}M(?i)ethods and preliminary analysis(?-i)",
    ".{0,4}M(?i)ethodology(?-i)",
    ".{0,4}M(?i)etodologia(?-i)",
    ".{0,4}M(?i)etodologie(?-i)"
  )

  synonyms[["Abstract"]] <- c(
    "Abstract",
    "Synopsis",
    "Summary"
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

  synonyms[["register_all"]] <- c(
    synonyms[["register"]],
    synonyms[["registered"]],
    synonyms[["registration"]]
  )

  synonyms[["registration_title_1"]] <- c(
    "Registration",
    "Trial(|s)",
    "Clinical trial(|s)",
    "Protocol(|s)"
  )

  synonyms[["registration_title_2"]] <- c(
    synonyms[["info_2"]],
    "no(|s)(|\\.)",
    "identifier(|s)",
    "registration"
  )

  synonyms[["registration_title_3"]] <- c(
    paste(
      c("Work", "Research", "Study", "Project", "Program", "Report"),
      "registration"
    )
  )

  synonyms[["registration_title"]] <- sapply(
    synonyms[["registration_title_1"]],
    paste,
    synonyms[["registration_title_2"]]
  ) %>%
    unlist() %>%
    append(synonyms[["registration_title_3"]]) %>%
    .first_capital()


  return(synonyms)
}
