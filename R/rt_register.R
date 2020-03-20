#' Identify mentions of registration on ClinicalTrials.gov
#'
#' Extract the index of mentions such as: "The study is registered at
#'     www.clinicaltrials.gov (NCT01624883)."
#'
#' @return Index of element with phrase of interest
get_ct_1 <- function(article) {

  # Just using the NCT was too sensitive
  # e.g. picked up references to protocols, mentions of trials underway, etc.
  grep("\\b(|pre|pre-)regist.{0,20}NCT[0-9]{8}", article, perl = T)

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
#' Extract the index of mentions such as: "This study was approved by the local
#'     Scientific and Ethics Committees of IRCCS \"Saverio de Bellis\",
#'     Castellana Grotte (Ba), Italy, and it was part of a registered research
#'     on https://www.clinicaltrials.gov, reg. number: NCT01244945."
#'
#' @return Index of element with phrase of interest
get_registered_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("this", "research_lower_strict", "and", "registered_registration")
  # Too generic without research & registered, or if including registration

  this_research <-
    synonyms %>%
    magrittr::extract(words[c(1, 2)]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = .max_words(" ", 5, space_first = F))

  this_research_registered <-
    synonyms %>%
    magrittr::extract(words[c(1, 2, 4)]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = .max_words(" ", 5, space_first = F))

  and_registered <-
    synonyms %>%
    magrittr::extract(words[c(3:4)]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = .max_words(" ", 5, space_first = F))

  research_and_registered <-
    synonyms %>%
    magrittr::extract(words[2]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(and_registered, sep = synonyms$txt)

  this_research_and_registered <-
    this_research %>%
    paste(and_registered, sep = synonyms$txt)

  # c(this_research_registered, research_and_registered) %>%
  #   lapply(.encase) %>%
  #   .encase() %>%
  #   paste("([A-Z]{2}\\s*[0-9]{2}|[0-9]{5})", sep = synonyms$txt) %>%
  #   grep(article, perl = T)

  c(this_research_registered, this_research_and_registered) %>%
    lapply(.encase) %>%
    .encase() %>%
    paste("([A-Z]{2}\\s*[0-9]{2}|[0-9]{5})", sep = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify generic mentions of registration
#'
#' Extract the index of mentions such as: " EPGP is registered with
#'     clinicaltrials.gov (NCT00552045)."
#'
#' @return Index of element with phrase of interest
get_registered_2 <- function(article) {

  synonyms <- .create_synonyms()

  c("(^|\\.\\s*)(Ethical|Approval|(|The )[A-Z][A-Z]+)",
    "(registered|registration)",
    "([Tt]rial|[Ss]tudy)"
  ) %>%
    paste(collapse = synonyms$txt) %>%
    paste("([A-Z]{2}\\s*[0-9]{2}|[0-9]{5})", sep = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify generic mentions of registration
#'
#' Extract the index of mentions such as: "registered with Clinical Trials
#'     (ChiCTR-IOR-14005438)"
#'
#' @return Index of element with phrase of interest
get_registered_3 <- function(article) {

  synonyms <- .create_synonyms()

  c("([Tt]rial|[Pp]rotocol) (registered|registration) (with\\b|under\\b|on\\b|at\\b|as\\b)") %>%
    paste("([A-Z]{2}\\s*[0-9]{2}|[0-9]{5})", sep = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify generic mentions of registration
#'
#' Extract the index of mentions such as: "This registered study on
#'     www.clinicaltrials.gov (NCT01375270) was approved by the Ethics
#'     Committee of the Capital Region of Denmark (H-3-2010-127), and all
#'     subjects provided informed written consent to participate."
#'
#' @return Index of element with phrase of interest
get_registered_4 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("this", "registered", "research_lower_strict")
  # Too generic without research & registered, or if including registration

  synonyms$this <- append(synonyms$this, "\\b[Ww]e")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = .max_words(" ", 5, space_first = F)) %>%
    paste("([A-Z]{2}\\s*[0-9]{2}|[0-9]{5})", sep = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify generic mentions of registration
#'
#' Extract the index of mentions such as: "The Régression de l'Albuminurie dans
#'     la Néphropathie Drépanocytaire (RAND) study design was approved by the
#'     local ethics committee (Ref: DGRI CCTIRS MG/CP09.503, 9 July 2009) and
#'     registered at ClinicalTrials.gov (NCT01195818)."
#'
#' @return Index of element with phrase of interest
get_registered_5 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("this", "research_strict", "and", "registered_registration")
  SOME <- "\\([a-zA-Z]+\\)"
  # Too generic without research & registered, or if including registration

  SOME_research <-
    synonyms %>%
    magrittr::extract(words[2]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(SOME, .)

  the_SOME_research <-
    synonyms %>%
    magrittr::extract(words[1]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(SOME_research, sep = synonyms$txt)

  the_SOME_research_registered <-
    synonyms %>%
    magrittr::extract(words[4]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(the_SOME_research, ., sep = .max_words(" ", 5, space_first = F))

  and_registered <-
    synonyms %>%
    magrittr::extract(words[c(3:4)]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = .max_words(" ", 5, space_first = F))

  the_SOME_research_and_registered <-
    the_SOME_research %>%
    paste(and_registered, sep = synonyms$txt)


  c(the_SOME_research_registered, the_SOME_research_and_registered) %>%
    lapply(.encase) %>%
    .encase() %>%
    paste("([A-Z]{2}\\s*[0-9]{2}|[0-9]{5})", sep = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify mentions of lack of registration
#'
#' Extract the index of mentions such as: "This trial and its protocol were not
#'     registered on a publicly accessible registry."
#'
#' @return Index of element with phrase of interest
get_not_registered_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("this", "research_lower_strict")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound, location = "both") %>%
    lapply(.encase) %>%
    paste(collapse = .max_words(" ", n_max = 4, space_first = F)) %>%
    paste(" not", " registered", sep = .max_words("", n_max = 6)) %>%
    grep(article, perl = T)
}


#' Identify generic mentions of registration
#'
#' Extract the index of mentions such as: "Trial registration number is
#'     TCTR20151021001."
#'
#' @return Index of element with phrase of interest
get_registration_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("research_strict", "registration")  # Too generic without these

  research_registration <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = " ") %>%
    paste0("(:|-+)")

  c(research_registration, "([A-Z]{2}\\s*[0-9]{2}|[0-9]{5})") %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify generic mentions of registration
#'
#' Extract the index of mentions such as: "Trial registration number is
#'     TCTR20151021001."
#'
#' @return Index of element with phrase of interest
get_registration_2 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("Research_strict", "registration")

  research_registration <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = " ")

  c(research_registration, "([A-Z]{2}\\s*[0-9]{2}|[0-9]{5})") %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify generic mentions of registration
#'
#' Extract the index of mentions such as: "Public clinical trial registration
#'     (www.clinicaltrials.gov) ID# NCT02720653"
#'
#' @return Index of element with phrase of interest
get_registration_3 <- function(article) {

  grep("(^|\\.).{0,35}\\b(|pre|pre-)registration.{0,35}NCT[0-9]{8}",
       article, perl = T)

}


#' Identify generic mentions of registration
#'
#' Extract the index of mentions such as: "The RAPiD trial's International
#'     Standard Randomised Controlled Trial Number (ISRCTN) registration is
#'     ISRCTN 49204710."
#'
#' @return Index of element with phrase of interest
get_registration_4 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("This", "research_lower_strict", "registration")
  # Too generic without research & registered, or if including registration

  this_research <-
    synonyms %>%
    magrittr::extract(words[c(1, 2)]) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = .max_words(" ", 5, space_first = F))

  c(this_research) %>%
    lapply(.encase) %>%
    .encase() %>%
    paste("registration", "([A-Z]{2}\\s*[0-9]{2}|[0-9]{5})", sep = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify mentions of registry
#'
#' Extract the index of mentions such as: "Here, we describe a collaboration
#'     between an international group of patient organisations advocating for
#'     patients with atypical haemolytic uraemic syndrome (aHUS), the aHUS
#'     Alliance, and an international aHUS patient registry (ClinicalTrials.gov
#'     NCT01522183)."
#'
#' @return Index of element with phrase of interest
get_registry_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("research_lower_strict", "registry")  # Too generic without these

  research_registry <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt)

  a <-
    c(research_registry, "[\\s,;:]+([A-Z]{2,10}\\s*[0-9]{2}|[0-9]{5})") %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

  if (!!length(a)) {

    is_false <- negate_registry_1(article[a])
    a <- a[!is_false]

  }

  return(a)

}




#' Identify registration titles - sensitive with negation
#'
#' Extract the index of mentions such as: "Study registration: ..."
#'
#' @return Index of element with phrase of interest
get_reg_title_1 <- function(article) {

  b <- integer()
  synonyms <- .create_synonyms()
  words <- c("registration_title")

  a <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.title_strict) %>%
    lapply(.encase) %>%
    paste() %>%
    grep(article, perl = T)

  if (!!length(a)) {

    if (nchar(article[a + 1]) == 0) {
      b <- c(a, a + 2)
    } else {
      b <- c(a, a + 1)
    }

    is_true <- any(negate_reg_title_1(article[b]))
    if (is_true) return(b)

  }

  a <-
    synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.title_strict, within_text = T) %>%
    lapply(.encase) %>%
    paste() %>%
    grep(article, perl = T)

  if (!!length(a)) {

    is_true <- negate_reg_title_1(article[a])
    a <- a[is_true]

  }

  return(a)
}


#' Identify registration titles - specific with no negation
#'
#' Extract the index of mentions such as: "Trial registration: ..."
#'
#' @return Index of element with phrase of interest
get_reg_title_2 <- function(article) {

  b <- integer()
  synonyms <- .create_synonyms()

  reg_title_synonyms <- c(
    "R(?i)egistration info(|rmation)(?-i)",
    "R(?i)egistration detail(|s)(?-i)",
    "R(?i)egistration no(|s)(|\\.)(?-i)",
    "R(?i)egistration number(|s)(?-i)",
    "R(?i)egistration identifier(|s)(?-i)",
    "T(?i)rial(|s) identifier(|s)(?-i)",
    "C(?i)linical trial(|s) identifier(|s)(?-i)",
    "T(?i)rial(|s) registration(?-i)",
    "C(?i)linical trial(|s) registration(?-i)",
    "S(?i)tudy registration(?-i)"
  )

  a <-
    reg_title_synonyms %>%
    lapply(.bound) %>%
    lapply(.title_strict) %>%
    .encase %>%
    grep(article, perl = T)

  if (!!length(a)) {

    if (nchar(article[a + 1]) == 0) {
      b <- c(a, a + 2)
    } else {
      b <- c(a, a + 1)
    }

    is_true <- TRUE
    if (is_true) return(b)

  }

  a <-
    reg_title_synonyms %>%
    lapply(.bound) %>%
    lapply(.title_strict, within_text = T) %>%
    .encase %>%
    grep(article, perl = T)

  if (!!length(a)) {

    is_true <- TRUE
    a <- a[is_true]

  }

  return(a)
}


#' Identify registration titles - specific
#'
#' Extract the index of mentions such as: "Retrospective clinical trial
#'     registration:"
#'
#' @return Index of element with phrase of interest
get_reg_title_3 <- function(article) {

  b <- integer()
  d <- integer()
  synonyms <- .create_synonyms()
  punct <-

    # Protocol only contributed to FPs due to protocol ethical approval
    registration_synonyms <- c(
      "Registration",
      "Clinical [Tt]rial",
      "Trial"
    )

  number_synonyms <- c(
    "registration",
    "number",
    "no(|s)(|\\.)",
    "number(|s)",
    "#",
    "ID(|s)",
    "identifier(|s)",
    "registration"
  )

  start <-  "^.{0,1}[A-Z](?i)\\w+"
  registration <- registration_synonyms %>% .bound %>% .encase
  number <- number_synonyms %>% .bound %>% .encase
  finish <- "(| \\w+)(\\.|:|-+)(?-i)"

  a <-
    paste(start, registration, number, finish, sep = "\\s*") %>%
    paste0("$") %>%
    grep(article, perl = T)

  if (!!length(a)) {

    for (i in seq_along(a)) {

      if (nchar(article[a[i] + 1]) == 0) {
        d <- c(a[i], a[i] + 2)
      } else {
        d <- c(a[i], a[i] + 1)
      }

      is_true <- any(negate_reg_title_1(article[d]))
      if (is_true) b <- c(b, d)
    }

    if (!!length(b)) return(unique(b))

  }

  a <-
    paste(start, registration, number, finish, sep = "\\s*") %>%
    grep(article, perl = T)

  if (!!length(a)) {

    is_true <- negate_reg_title_1(article[a])
    a <- a[is_true]

  }

  return(a)
}


#' Identify registration titles - specific
#'
#' Extract the index of mentions such as: "Clinical trial registration details:"
#'
#' @return Index of element with phrase of interest
get_reg_title_4 <- function(article) {

  b <- integer()
  d <- integer()
  synonyms <- .create_synonyms()

  # Protocol only contributed to FPs due to protocol ethical approval
  registration_synonyms <- c(
    "R(?i)egistration",
    "C(?i)linical [Tt]rial",
    "T(?i)rial"
  )

  number_synonyms <- c(
    "registration",
    "number",
    "no(|s)(|\\.)",
    "number(|s)",
    "#",
    "ID(|s)",
    "identifier(|s)",
    "registration"
  )

  start <-  "^.{0,1}"
  registration <- registration_synonyms %>% .bound %>% .encase
  number <- number_synonyms %>% .bound %>% .encase
  finish <- "(| \\w+)(\\.|:|-+)(?-i)"

  a <-
    paste(start, registration, number, finish, sep = "\\s*") %>%
    paste0("$") %>%
    grep(article, perl = T)

  if (!!length(a)) {

    for (i in seq_along(a)) {

      if (nchar(article[a[i] + 1]) == 0) {
        d <- c(a[i], a[i] + 2)
      } else {
        d <- c(a[i], a[i] + 1)
      }

      is_true <- any(negate_reg_title_1(article[d]))
      if (is_true) b <- c(b, d)
    }

    if (!!length(b)) return(unique(b))

  }

  a <-
    paste(start, registration, number, finish, sep = "\\s*") %>%
    grep(article, perl = T)

  if (!!length(a)) {

    is_true <- negate_reg_title_1(article[a])
    a <- a[is_true]

  }

  return(a)
}


#' Identify mentions of protocol
#'
#' Extract the index of mentions such as: "The complete study protocol has been
#'     published previously (Supplement 1)"
#'
#' @return Index of element with phrase of interest
get_protocol_1 <- function(article) {

  synonyms <- .create_synonyms()
  words <- c("study_protocol", "published", "previously")

  synonyms %>%
    magrittr::extract(words) %>%
    lapply(.bound) %>%
    lapply(.encase) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify mentions of protocol
#'
#' Extract the index of mentions such as: "Alliance for Clinical Trials in
#'     Oncology (formerly Cancer and Leukemia Group B) Protocol #369901"
#'
#' @return Index of element with phrase of interest
get_protocol_2 <- function(article) {

  grep("[Pp]rotocol .{0,5}(|[A-Z]+)[0-9]{5}", article, perl = T)

}


#' Identify mentions of funding followed by NCT
#'
#' Extract the index of mentions such as: "Funded by: the National Heart, Lung,
#'     and Blood Institute, the National Institute of Diabetes and Digestive and
#'     Kidney Disease, and others; SPECS ClinicalTrials.gov number, NCT00443599;
#'     Nutrition and Obesity Center at Harvard; NIH 5P30DK040561-17"
#'
#' @return Index of element with phrase of interest
get_funded_ct_1 <- function(article) {

  synonyms <- .create_synonyms()

  # Anything more general contributed more false than true matches
  funded_by_SOME_ct_NCT <- paste0(
    "[Ff]unded by",
    synonyms$txt,
    "[A-Z]{2,7}.{0,20}[Cc]linical[Tt]rial.{0,20}NCT[0-9]{8}"
  )

  grep(funded_by_SOME_ct_NCT, article, perl = T)

}


#' Negate unwanted mentions of registry
#'
#' Negate mentions of registry such as: "Ethics approval and consent to
#'     participate This study was approved by the Institutional Review Board of
#'     Chang Gung Memorial Hospital under registry number 201601023B0."
#'
#' @return Index of element with phrase of interest
negate_registry_1 <- function(article) {

  synonyms <- .create_synonyms()

  research <- synonyms$research %>% .bound %>% .encase
  registry <- synonyms$registry %>% .bound %>% .encase
  code <- "[\\s,;:]+([A-Z]{2,10}\\s*[0-9]{2}|[0-9]{5})"

  paste(research, "approv", registry, code, sep = synonyms$txt) %>%
    grepl(article, perl = T)

}


#' Negate unwanted mentions of title
#'
#' Negate title mentions that refer to unwanted text.
#'
#' @return Index of element with phrase of interest
negate_reg_title_1 <- function(article) {

  grepl("[A-Z]{2}\\s*[0-9]{2}|[0-9]{5}", article, perl = T)

}


#' Remove mentions of previously reported registered studies
#'
#' Removes mentions such as: "An active o <- servational cohort study was
#'     conducted as previously reported (ClinicalTrials.gov identifier
#'     NCT01280162) [16]."
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
obliterate_refs_1 <- function(article) {

  # Good for finding, but not for substituting b/c it's a lookahead
  # words <- c(
  #   # positive lookahead makes these phrases interchangeable
  #   "(?=[a-zA-Z0-9\\s,()-]*(financial|support))",
  #   "(?=[a-zA-Z0-9\\s,()-]*(conflict|competing))"
  # )

  # reported_synonyms <- c(
  #   "reported",
  #   "published"
  # )
  #
  # cohort_synonyms <- c(
  #   "cohort study",
  #   "retrospective\\b",
  #   "propsecitve study",
  #   "nested",
  #   "case( |\\s*-\\s*)control"
  # )
  #
  # gsub("(reported|published).{0,20}([Cc]linical[Tt]rial|NCT", "", article, perl = T)

  gsub("NCT[0-9]{8}.{3}[0-9]+", "", article, perl = T)

  # TODO: This to be inserted only for get_ct_2!

}


#' Remove references
#'
#' Removes mentions such as: "An active o <- servational cohort study was
#'     conducted as previously reported (ClinicalTrials.gov identifier
#'     NCT01280162) [16]."
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
obliterate_references_1 <- function(article) {

  # If within References or under references and starts with 1. or contains et al. then remove.

  ref_from <- find_refs(article)

  if (!!length(ref_from)) {

    ref_to <- length(article)

    article[ref_from] <- ""
    article[ref_from:ref_to] <-
      gsub("^([0-9]{1,3}\\.\\s|.*et al\\.).*$", "",
      article[ref_from:ref_to], perl = T)

  }
  return(article)
}


#' Remove semicolons when within parentheses
#'
#' Removes mentions such as: "guidelines for diagnostic studies (trial
#'     registered at www.clinicaltrial.gov; NCT01697930)."
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
obliterate_semicolon_1 <- function(article) {

  gsub("(\\(.*); (.*\\))", "\\1 - \\2", article)

}


#' Remove commas
#'
#' Removes commas to simplify regular expressions.
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
obliterate_comma_1 <- function(article) {

  gsub(", ", " ", article)

}


#' Remove apostrophe
#'
#' Removes commas to make ease creation of regular expressions. After
#'     implmenting this function, "ball's" should become balls and l'Alba
#'     should become lAlba and balls' into balls.
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
obliterate_apostrophe_1 <- function(article) {

  txt_1 <- "([a-zA-Z])'([a-zA-Z])"
  txt_2 <- "[a-z]+s'"

  article %>%
    purrr::map(gsub, pattern = txt_1, replacement = "\\1\\2") %>%
    purrr::map(gsub, pattern = txt_2, replacement = "s")

}


#' Remove hash
#'
#' Removes hashes to make ease creation of regular expressions.
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
obliterate_hash_1 <- function(article) {

  gsub("#", "", article)

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

  article <- basename(filename) %>% stringr::word(sep = "\\.")
  pmid <- gsub("^.*PMID([0-9]+).*$", "\\1", filename)

  index <- integer()
  is_register_pred <- FALSE
  register_text <- ""
  is_relevant <- NA
  is_NCT <- NA
  is_explicit <- NA
  is_method <- NA


  file_text <- readr::read_file(filename)

  is_relevant <- any(grepl("\\b(|-)([Rr]egist|(|[Cc]linical)[Tt]rial|NCT[0-9]{8})", file_text))
  if (is_relevant) {

    # TODO: MOVE THIS TO THE pdf2text FUNCTION AND ENCODE AS UTF-8
    # Fix PDF to txt bugs
    broken_1 <- "([a-z]+)-\n+([a-z]+)"
    broken_2 <- "([a-z]+)(|,|;)\n+([a-z]+)"
    broken_3 <- "([0-9]+)-\n+([0-9]+)"
    paragraphs <-
      file_text %>%
      purrr::map(gsub, pattern = broken_1, replacement = "\\1\\2") %>%
      purrr::map(gsub, pattern = broken_2, replacement = "\\1\\3") %>%
      purrr::map(gsub, pattern = broken_3, replacement = "\\1\\2") %>%
      purrr::map(strsplit, "\n| \\*") %>%
      unlist() %>%
      utf8::utf8_encode()


    # Stop if this is not an empirical study or a protocol (e.g. a review)
    from <- find_methods(paragraphs)
    is_method <- !!length(from)

    # Previously removed b/c I thought a correspondence article was wrongly
    # classified - I was wrong and that was a TN
    if (!is_method) {

      return(tibble::tibble(
        article,
        pmid,
        is_register_pred,
        register_text,
        is_relevant,
        is_method,
        is_NCT,
        is_explicit
      ))

    }


    # TODO: MOVE UP TO obliterate_fullstop_1 TO pdf2text FUNCTION
    # Remove potentially misleading sequences
    utf_1 <- "(\\\\[a-z0-9]{3})"   # remove \\xfc\\xbe etc
    utf_2 <- "(\\\\[a-z])"   # \\f or
    paragraphs_pruned <-
      paragraphs %>%
      purrr::map_chr(gsub, pattern = utf_1, replacement = " ", perl = T) %>%
      purrr::map_chr(gsub, pattern = utf_2, replacement = "",  perl = T) %>%
      obliterate_references_1() %>%
      obliterate_fullstop_1() %>%
      obliterate_semicolon_1() %>%  # adds minimal overhead
      obliterate_comma_1() %>%   # adds minimal overhead
      obliterate_apostrophe_1() %>%
      obliterate_hash_1()


    is_NCT <- any(grepl("NCT[0-9]{8}", file_text))


    # TODO There are 4 highly overlapping reg_title_ functions, fix!
    # Identify sequences of interest
    index_any <- list()
    # index_any[["ct_1"]] <- get_ct_1(paragraphs_pruned)
    index_any[["prospero_1"]] <- get_prospero_1(paragraphs_pruned)
    index_any[["registered_1"]] <- get_registered_1(paragraphs_pruned)
    index_any[["registered_2"]] <- get_registered_2(paragraphs_pruned)
    index_any[["registered_3"]] <- get_registered_3(paragraphs_pruned)
    index_any[["registered_4"]] <- get_registered_4(paragraphs_pruned)
    index_any[["registered_5"]] <- get_registered_5(paragraphs_pruned)
    index_any[["not_registered_1"]] <- get_not_registered_1(paragraphs_pruned)
    index_any[["registration_1"]] <- get_registration_1(paragraphs_pruned)
    index_any[["registration_2"]] <- get_registration_2(paragraphs_pruned)
    index_any[["registration_3"]] <- get_registration_3(paragraphs_pruned)
    index_any[["registration_4"]] <- get_registration_4(paragraphs_pruned)
    index_any[["registry_1"]] <- get_registry_1(paragraphs_pruned)
    index_any[["reg_title_1"]] <- get_reg_title_1(paragraphs_pruned)
    index_any[["reg_title_2"]] <- get_reg_title_2(paragraphs_pruned)
    index_any[["reg_title_3"]] <- get_reg_title_3(paragraphs_pruned)
    index_any[["reg_title_4"]] <- get_reg_title_4(paragraphs_pruned)
    index_any[["funded_ct_1"]] <- get_funded_ct_1(paragraphs_pruned)
    index <- unlist(index_any) %>% unique() %>% sort()


    # Apply a more sensitive search in Methods
    if (!length(index)) {

      # from <- find_methods(paragraphs_pruned)

      if (!!length(from)) {

        to <- from + 10

        paragraphs_pruned[from:to] %<>% lapply(obliterate_refs_1)

        index_method <- list()
        index_method[["ct_2"]] <- get_ct_2(paragraphs_pruned[from:to])
        index_method[["protocol_1"]] <- get_protocol_1(paragraphs_pruned[from:to])
        index <- unlist(index_method) %>% magrittr::add(from - 1)
      }
    }


    is_register_pred <- !!length(index)
    register_text <- paragraphs[index] %>% paste(collapse = " ")

    if (is_register_pred) {

      is_explicit <- !!length(unlist(index_any))

    }


  } else {

    is_register_pred <- FALSE
    register_text <- ""

  }

  tibble::tibble(
    article,
    pmid,
    is_register_pred,
    register_text,
    is_relevant,
    is_method,
    is_NCT,
    is_explicit
  )
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
.title_strict <- function(x, within_text = F) {

  if (within_text) {

    return(paste0(x, "( [A-Z][a-zA-Z]|:|\\.|\\s*-+)"))
    # stricter: "( [A-Z][a-zA-Z]|:|\\.)", avoided b/c Funding sources none.

  } else {

    return(paste0("^.{0,1}", x, "(|:|\\.|\\s*-+)$"))

  }
}


#' Create a regular expression where the first letter is capital
#'
#' Returns a regular expression that necessitates that the first letter is
#'     capital and the rest can be any case.
#'
#' @param x A vector of strings.
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

  # synonyms$ about x2 faster than synonyms[[]]

  synonyms <- list()

  synonyms$txt <- "[a-zA-Z0-9\\s,()\\[\\]/:-]*"  # order matters

  synonyms$This <- c(
    "This",
    "These",
    "The",
    "Our",
    "All"
  )

  synonyms$This_singular <- c(
    "This",
    "The",
    "Our"
  )

  synonyms$These <- c(
    "These",
    "Our",
    "Research",
    "All"
  )

  synonyms$this <- c(
    "[Tt]his",
    "[Tt]hese",
    "[Tt]he",
    "[Oo]ur"
  )

  synonyms$this_singular <- c(
    "this",
    "the"
  )

  synonyms$these <- c(
    "these"
  )

  synonyms$is <- c(
    "is",
    "been",
    "are",
    "was",
    "were"
  )

  synonyms$is_singular <- c(
    "is",
    "have",
    "has",
    "was"
  )

  synonyms$are <- c(
    "are",
    "have",
    "were"
  )

  synonyms$have <- c(
    "have",
    "has"
  )

  synonyms$is_have <- c(
    synonyms$is,
    synonyms$have
  )

  synonyms$We <- c(
    "We"
  )

  synonyms$by <- c(
    "by",
    "from",
    "within",
    "under"
  )

  synonyms$and <- c(
    "and",
    "&",
    "or"
  )

  synonyms[["for"]] <- c(
    "for"
  )

  synonyms$of <- c(
    "of",
    "about"
  )

  synonyms$for_of <- c(
    synonyms[["for"]],
    synonyms$of
  )

  synonyms$no <- c(
    "[Nn]o",
    "[Nn]il",
    "[Nn]one",
    "[Nn]othing"
  )

  synonyms$No <- c(
    "N(?i)o(?-i)",
    "N(?i)il(?-i)",
    "N(?i)one(?-i)",
    "N(?i)othing(?-i)"
  )

  synonyms$not <- c(
    "not"
  )

  synonyms$author <- c(
    "author(|s|\\(s\\))",
    "researcher(|s|\\(s\\))",
    "investigator(|s|\\(s\\))",
    "scientist(|s|\\(s\\))"
  )

  synonyms$research <- c(
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

  synonyms$research_strict <- c(
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

  synonyms$research_lower <- c(
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

  synonyms$research_lower_strict <- c(
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

  synonyms$Research <- c(
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

  synonyms$Research_strict <- c(
    "Work(|s)",
    "Research",
    "Stud(y|ies)",
    "Project(|s)",
    "Trial(|s)",
    "Program(|s)",
    "Analys(is|es)",
    "Investigation(|s)"
  )

  synonyms$research_singular <- c(
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

  synonyms$researches <- c(
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

  synonyms$funder <- c(
    "[Ff]under",
    "[Ss]ponsor",
    "[Ss]upporter"
  )

  synonyms$funds <- c(
    "[Ff]und(|s)",
    "[Ff]unding",
    "[Ss]elf-funding"
  )

  synonyms$funded <- c(
    "[Ff]unded",
    "[Ss]elf-funded",
    "[Ff]inanced",
    "[Ss]upported",
    "[Ss]ponsored",
    "[Rr]esourced",
    "[Aa]ided"
  )

  synonyms$funding <- c(
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

  synonyms$funded_funding <- c(
    synonyms$funded,
    synonyms$funding
  )

  synonyms$funding_financial <- c(
    synonyms$funding,
    synonyms$financial
  )

  # TODO: split this into (financial|funding) (support|source|etc)
  synonyms$funding_title <- c(
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

  synonyms$financial <- c(
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
  synonyms$financial_title <- c(
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

  synonyms$any_title <- c(
    synonyms$funding_title,
    synonyms$financial_title
  )

  synonyms$disclosure_title <- c(
    "F(?i)unding disclosure(|s)(?-i)",
    "F(?i)unding disclosure(|s)(?-i)",
    "F(?i)inancial disclosure(|s)(?-i)",
    "F(?i)inancial declaration(|s)(?-i)",
    "F(?i)inancial (&|and) competing interests disclosure(|s)(?-i)",
    "F(?i)inancial (&|and) competing interests declaration(|s)(?-i)",
    "D(?i)isclosure(|s)(?-i)",
    "D(?i)eclaration(|s)(?-i)"
  )

  synonyms$support <- c(
    "[Ss]upport(|s)",
    "\\b[Aa]id(|s)",
    "[Aa]ssistance",
    "[Ss]ponsorship(|s)"
  )

  synonyms$support_only <- c(
    "[Ss]upport(|s)"
  )

  synonyms$Supported <- c(
    "Supported"
  )

  synonyms$award <- c(
    "[Gg]rant(|s)",
    "[Ff]ellowship(|s)",
    "[Aa]ward(|s|ing)",
    "[Ss]cholar(|s|ship|ships)",
    "[Ee]ndowment(|s)",
    "[Ss]tipend(|s)",
    "[Bb]ursar(y|ies)"
  )

  synonyms$grant_title <- c(
    "G(?i)rant(|s)(?-i)",
    "G(?i)rant sponsor(|s|ship(|s))(?-i)",
    "G(?i)rant support(|s)(?-i)",
    "G(?i)rant assistance(?-i)",
    "G(?i)rant aid(|s)(?-i)",
    "^[A-Z]\\w+ grant sponsor(|s|ship(|s))(?-i)"
    # removed Sponsorship and sponsors b/c FP without TP
  )

  synonyms$funds_award_financial <- c(
    synonyms$funds,
    synonyms$financial,
    synonyms$award
  )

  synonyms$funding_financial_award <- c(
    synonyms$funding,
    synonyms$financial,
    synonyms$award
  )

  synonyms$received <- c(
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

  synonyms$recipient <- c(
    "[Rr]ecipient(|s)",
    "[Aa]wardee(|s)",
    "[Gg]rantee(|s)"
  )


  synonyms$provide <- c(
    "provid(ed|ing)",
    "g(ave|iving)",
    "award(ed|ing)"
  )

  synonyms$thank <- c(
    "[Tt]hank(|ful)",
    "[Aa]cknowledge",
    "[Dd]isclose",
    "[Gg]rateful"
  )

  synonyms$conflict <- c(
    "[Cc]onflict(|ing)",
    "[Cc]ompet(e|ing)",
    "source(|s) of bias"
    # "[Cc]onflits",
    # "[Cc]onflictos",
  )

  synonyms$conflict_title <- c(
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

  synonyms$relationship <- c(
    "relation(|s|ship(|s))",
    "association(|s)",
    "involvement(|s)",
    "affiliation(|s)",
    "tie(|s)"
  )

  synonyms$info <- c(
    "info(|rmation)\\b",
    "detail(|s)",
    "particulars",
    "data\\b",
    "material\\b"
  )

  synonyms$acknowledge <- c(
    "acknowledge",
    "recognize",
    "disclose",
    "declare",
    "report",
    "appreciate"
  )

  synonyms$acknowledged <- c(
    "acknowledged",
    "recognized",
    "disclosed",
    "declared",
    "reported",
    "appreciated"
  )

  synonyms$foundation <- c(
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

  synonyms$foundation_award <- c(
    synonyms$foundation,
    synonyms$award
  )

  synonyms$References <- c(
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

  synonyms$Methods <- c(
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

  synonyms$Abstract <- c(
    "Abstract",
    "Synopsis",
    "Summary"
  )

  synonyms$Introduction <- c(
    "Introduction",
    "Background"
  )

  synonyms$Results <- c(
    "Results",
    "Findings"
  )

  synonyms$Conclusion <- c(
    "Conclusion",
    "Interpretation"
  )

  synonyms$sources <- c(
    "source(|s)"
  )

  synonyms$register <- c(
    "register"
  )

  synonyms$registered <- c(
    "registered"
  )

  synonyms$registration <- c(
    "registration"
  )

  synonyms$registry <- c(
    "[Rr]egistr(y|ies)"
  )

  synonyms$registered_registration <- c(
    synonyms$registered,
    synonyms$registration
  )

  synonyms$registration_title_1 <- c(
    "Registration",
    "Trial(|s)",
    "Clinical trial(|s)"
    # "Protocol(|s)"  # only contributed FPs due to protocol ethical approval
  )

  synonyms$registration_title_2 <- c(
    "info(|rmation)\\b",
    "detail(|s)\\b",
    "no(|s)(|\\.)",
    "number(|s)",
    # "[Ii][Dd](|s)", # only contributes to FPs
    "identifier(|s)",
    "registration"
  )

  synonyms$registration_title_3 <- c(
    paste(c(
      "Work", "Research", "Study", "Project", "Program", "Report"),
      "registration"
    ),
    "Registration"
  )

  # synonyms[["registration_title"]] <-
  #   synonyms[["registration_title_3"]] %>%
  #   .first_capital()

  # Captures "Trial details" and used with negation - worth the duplication
  synonyms$registration_title <- expand.grid(
    synonyms$registration_title_1,
    synonyms$registration_title_2
  ) %>%
    purrr::pmap(paste, sep = " ") %>%
    unlist() %>%
    append(synonyms$registration_title_3) %>%
    .first_capital()

  synonyms$protocol <- c(
    "protocol"
  )

  synonyms$study_protocol <- c(
    "research protocol",
    "study protocol",
    "trial protocol",
    "analysis protocol",
    "investigation protocol",
    "research design",
    "study design",
    "trial design"
  )

  synonyms$published <- c(
    "published",
    "reported",
    "made available",
    "posted",
    "issued"
  )

  synonyms$previously <- c(
    "previously",
    "before",
    "already"
  )


  return(synonyms)
}
