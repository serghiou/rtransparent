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
get_ct_2 <- function(article, synonyms) {

  the_study <- "(This|The|It|There) ([a-zA-Z0-9]+\\s){0,2}(randomi(z|s)ed|study|trial|was a|is a|were)"
  ct_nct <- "([Cc]linical[Tt]rial.* NCT[0-9]{8}|ISRCTN[0-9]{8}|ChiCTR-IOR-[0-9]{5})"

  c(the_study, ct_nct) %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)

}


#' Identify mentions of registration on ClinicalTrials.gov
#'
#' Extract the index of mentions such as: "Registered on ClinicalTrials.gov (NCT12345678)."
#'
#' @return Index of element with phrase of interest
get_ct_3 <- function(article, synonyms) {

  ct_nct <- c(
    "(^|\\.)[a-zA-Z0-9\\s,]+[Cc]linical[Tt]rials",
    "NCT[0-9]{8}"
  )

  ct_nct %>%
    paste(collapse = synonyms$txt) %>%
    grep(article, perl = T)
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
  grep("\\bP(?i)ROSPERO\\b(?-i).*CRD\\s*[0-9]{5}", article, perl = T)

}


#' Identify generic mentions of registration
#'
#' Extract the index of mentions such as: "This study was approved by the local
#'     Scientific and Ethics Committees of IRCCS \"Saverio de Bellis\",
#'     Castellana Grotte (Ba), Italy, and it was part of a registered research
#'     on https://www.clinicaltrials.gov, reg. number: NCT01244945."
#'
#' @return Index of element with phrase of interest
get_registered_1 <- function(article, synonyms) {

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
get_registered_2 <- function(article, synonyms) {

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
get_registered_3 <- function(article, synonyms) {

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
get_registered_4 <- function(article, synonyms) {

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
get_registered_5 <- function(article, synonyms) {

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
get_not_registered_1 <- function(article, synonyms) {

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
get_registration_1 <- function(article, synonyms) {

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
get_registration_2 <- function(article, synonyms) {

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
get_registration_4 <- function(article, synonyms) {

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
get_registry_1 <- function(article, synonyms) {

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

    is_false <- negate_registry_1(article[a], synonyms)
    a <- a[!is_false]

  }

  return(a)

}




#' Identify registration titles - sensitive with negation
#'
#' Extract the index of mentions such as: "Study registration: ..."
#'
#' @return Index of element with phrase of interest
get_reg_title_1 <- function(article, synonyms) {

  b <- integer()
  d <- integer()

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

    for (i in seq_along(a)) {

      if (is.na(article[a[i] + 1])) {

        b <- c(b, a[i])

      } else {

        if (nchar(article[a[i] + 1]) == 0) {
          b <- c(b, a[i], a[i] + 2)
        } else {
          b <- c(b, a[i], a[i] + 1)
        }
      }

      is_true <- any(negate_reg_title_1(article[b]))
      if (is_true) d <- c(d, b)

    }
    if (!!length(d)) return(d)
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
  d <- integer()

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

    for (i in seq_along(a)) {

      if (is.na(article[a[i] + 1])) {

        b <- c(b, a[i])

      } else {

        if (nchar(article[a[i] + 1]) == 0) {
          b <- c(b, a[i], a[i] + 2)
        } else {
          b <- c(b, a[i], a[i] + 1)
        }
      }

      is_true <- any(negate_reg_title_1(article[b]))
      if (is_true) d <- c(d, b)

    }
    if (!!length(d)) return(d)
  }


  a <-
    reg_title_synonyms %>%
    lapply(.bound) %>%
    lapply(.title_strict, within_text = T) %>%
    .encase %>%
    grep(article, perl = T)

  return(a)
}


#' Identify registration titles - specific
#'
#' Extract the index of mentions such as: "Retrospective clinical trial
#'     registration:"
#'
#' @return Index of element with phrase of interest
get_reg_title_3 <- function(article, synonyms) {

  b <- integer()
  d <- integer()

  # Protocol only contributed to FPs due to protocol ethical approval
  registration_synonyms <- c(
    "Registration",
    "Clinical [Tt]rial",
    "Trial"
  )

  number_synonyms <- c(
    "registration",
    "no(|s)(|\\.)",
    "number(|s)",
    "#",
    "ID(|s)",
    "identifier(|s)",
    "information",
    "details"
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

      if (is.na(article[a[i] + 1])) {

        b <- c(b, a[i])

      } else {

        if (nchar(article[a[i] + 1]) == 0) {
          b <- c(b, a[i], a[i] + 2)
        } else {
          b <- c(b, a[i], a[i] + 1)
        }
      }

      is_true <- any(negate_reg_title_1(article[b]))
      if (is_true) d <- c(d, b)

    }
    if (!!length(d)) return(d)
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
get_reg_title_4 <- function(article, synonyms) {

  b <- integer()
  d <- integer()

  # Protocol only contributed to FPs due to protocol ethical approval
  registration_synonyms <- c(
    "R(?i)egistration",
    "C(?i)linical [Tt]rial",
    "T(?i)rial"
  )

  number_synonyms <- c(
    "registration",
    "no(|s)(|\\.)",
    "number(|s)",
    "#",
    "ID(|s)",
    "identifier(|s)",
    "information",
    "details"
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

      if (is.na(article[a[i] + 1])) {

        b <- c(b, a[i])

      } else {

        if (nchar(article[a[i] + 1]) == 0) {
          b <- c(b, a[i], a[i] + 2)
        } else {
          b <- c(b, a[i], a[i] + 1)
        }
      }

      is_true <- any(negate_reg_title_1(article[b]))
      if (is_true) d <- c(d, b)

    }
    if (!!length(d)) return(d)
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


#' Identify registration titles - specific with no negation
#'
#' Extract the index of mentions such as: "Trial registration: ..."
#'
#' @param article The text as a vector of strings.
#' @return Index of element with phrase of interest
.create_register_pmc_title <- function() {

  title_starts <- c(
    "R(?i)egistration",
    "T(?i)rial",
    "C(?i)linical",
    "(?i)Systematic",
    "M(?i)eta-analysis"
  )

  title_ends <- c(
    "registration",
    "number",
    "\\bid(entifier)",
    "\\bno(|s)",
    "detail(|s)",
    "info(|rmation)"
  )

  list(title_starts, title_ends) %>%
    purrr::map(.encase) %>%
    paste(collapse = "(?i).{0,20}") %>%
    paste(".{0,4}", ., ".{0,1}", sep = "")
}


.get_register_pmc_title <- function(article_xml) {

  b <- ""
  title_regex <- .create_register_pmc_title()

  # If I had not stripped the d1 namespace:
  # "back//fn-group//*[self::d1:title or self::d1:bold or self::d1:italic]"
  back_footnote <-
    article_xml %>%
    xml_find_all("back//fn-group//*[self::title or self::bold or self::italic]")

  a <-
    back_footnote %>%
    xml2::xml_text() %>%
    stringr::str_which(title_regex)

  if (!!length(a)) {

    b <-
      back_footnote %>%
      magrittr::extract(a) %>%
      xml_parent() %>%
      xml_contents() %>%
      xml_text() %>%
      paste(collapse = " ")

    return(b)
  }


  abstract <-
    article_xml %>%
    xml_find_all("front//abstract//*[self::title or self::bold or self::italic]")

  a <-
    abstract %>%
    xml_text() %>%
    stringr::str_which(title_regex)

  if (!!length(a)) {

    b <-
      abstract %>%
      magrittr::extract(a) %>%
      xml_parent() %>%
      xml_contents() %>%
      xml_text() %>%
      paste(collapse = " ")

    return(b)
  }


  front_footnote <-
    article_xml %>%
    xml_find_all("front/article-meta//fn//*[self::title or self::bold or self::italic]")

  a <-
    front_footnote %>%
    xml_text() %>%
    stringr::str_which(title_regex)

  if (!!length(a)) {

    b <-
      front_footnote %>%
      magrittr::extract(a) %>%
      xml_parent() %>%
      xml_contents() %>%
      xml_text() %>%
      paste(collapse = " ")

    return(b)
  }

  return(b)
}

#' Identify mentions of protocol
#'
#' Extract the index of mentions such as: "The complete study protocol has been
#'     published previously (Supplement 1)"
#'
#' @return Index of element with phrase of interest
get_protocol_1 <- function(article, synonyms) {

  words <- c("study protocol", "published", "previously")

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
get_funded_ct_1 <- function(article, synonyms) {


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
negate_registry_1 <- function(article, synonyms) {

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

  article %>% stringr::str_detect("[A-Z]{2}\\s*[0-9]{2}|[0-9]{5}")

}


#' Negate titles that mention that there was no registration
#'
#' Negate mentions such as "Clinical Trial Registration: N/A"
#'
#' @return Index of element with phrase of interest
negate_reg_title_2 <- function(article) {

  article %>% stringr::str_detect("\\bNA\\b|\\bN/A\\b|not registered")

}


#' Remove mentions of previously reported registered studies
#'
#' Removes mentions such as: "An active o <- servational cohort study was
#'     conducted as previously reported (ClinicalTrials.gov identifier
#'     NCT01280162) [16]."
#'
#' @param article A List with paragraphs of interest.
#' @return The list of paragraphs without mentions of financial COIs.
obliterate_refs_2 <- function(article) {

  # Good for finding, but not for substituting b/c it's a lookahead
  # words <- c(
  #   # positive lookahead makes these phrases interchangeable
  #   "(?=[a-zA-Z0-9\\s,()-]*(financial|support))",
  #   "(?=[a-zA-Z0-9\\s,()-]*(conflict|competing))"
  # )

  article %>%
    stringr::str_replace_all("NCT[0-9]{8}.{3}REFFF(|\\)|\\])", "") %>%
    stringr::str_replace_all("NCT[0-9]{8}[a-zA-Z0-9\\s,()\\[\\]/:-]*REFFF(|\\)|\\])", "")

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
rt_register_pmc <- function(filename, remove_ns = F) {

  xpath <- c(
    "front/article-meta/article-id[@pub-id-type = 'pmid']",
    "front/article-meta/article-id[@pub-id-type = 'pmc']",
    "front/article-meta/article-id[@pub-id-type = 'pmc-uid']",
    "front/article-meta/article-id[@pub-id-type = 'doi']"
  )

  var_names <- c(
    "pmid",
    "pmcid_pmc",
    "pmcid_uid",
    "doi"
  )

  # Creating and outputting these lists adds negligible time
  # Way faster than index_any[["reg_title_pmc"]] <- NA
  index_any <- list(
    reg_title_pmc = NA,
    prospero_1 = NA,
    registered_1 = NA,
    registered_2 = NA,
    registered_3 = NA,
    registered_4 = NA,
    registered_5 = NA,
    not_registered_1 = NA,
    registration_1 = NA,
    registration_2 = NA,
    registration_3 = NA,
    registration_4 = NA,
    registry_1  = NA,
    reg_title_1 = NA,
    reg_title_2 = NA,
    reg_title_3 = NA,
    reg_title_4 = NA,
    funded_ct_1 = NA
  )

  index_method <- list(
    ct_2 = NA,
    ct_3 = NA,
    protocol_1 = NA
  )

  out <- list(  # do not change order of pmid:doi - id extraction depends on it
    pmid = NA,
    pmcid_pmc = NA,
    pmcid_uid = NA,
    doi = NA,
    is_research = NA,
    is_review = NA,
    is_slr = FALSE,
    is_relevant = NA,
    is_method = NA,
    is_NCT = NA,
    is_register_pred = FALSE,
    register_text = "",
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
  # out <-
  #   xpath %>%
  #   lapply(.get_text, article_xml = article_xml, find_first = T) %>%
  #   {purrr::list_modify(out, !!!.)}


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

    return(tibble::as_tibble(c(out, index_any, index_method)))

  }

  # Check for SLR (code is marginally faster without this)
  # if (out$is_review) {
  #
  #   txt <- xml_text(article_xml)
  #   out$is_slr <- grepl("systematic review", txt, ignore.case = T)
  #
  #   if (!out$is_slr) {
  #
  #     return(tibble::as_tibble(c(out, index_any, index_method)))
  #
  #   }
  # }


  # Go through titles
  title_txt <- .get_register_pmc_title(article_xml)
  is_title <- nchar(title_txt) > 0

  if (is_title) {

    index_any$reg_title_pmc <- TRUE
    out$is_relevant <- TRUE
    out$is_explicit <- TRUE
    out$is_register_pred <- TRUE
    out$register_text <- title_txt

    return(tibble::as_tibble(c(out, index_any, index_method)))

  }


  # Check for relevance
  # txt <- xml_text(article_xml)
  # rel_regex <- "\\b(|-)([Rr]egist|(|[Cc]linical)[Tt]rial|NCT[0-9]{8}|PROSPERO)"
  # out$is_relevant <- grepl(rel_regex, txt)
  #
  # if (!out$is_relevant) {
  #
  #   return(tibble::as_tibble(c(out, index_any, index_method)))
  #
  # }


  # Check for methods (10x more costly than xml_text(article_xml))
  # methods <- .xml_methods(article_xml)
  # out$is_method <- !!length(methods)
  #
  # if (!out$is_method) {
  #
  #   return(tibble::as_tibble(c(out, index_any, index_method)))
  #
  # }


  # Extract article text into a list
  # article[["ack"]] <- .xml_ack(article_xml)
  # article[["methods"]] <- .xml_methods(article_xml)
  # article[["abstract"]] <- .xml_abstract(article_xml)
  # article[["footnotes"]] <- .xml_footnotes(article_xml)


  # Extract article text into a vector
  ack <- .xml_ack(article_xml)
  methods <- .xml_methods(article_xml)
  abstract <- .xml_abstract(article_xml)
  footnotes <- .xml_footnotes(article_xml)
  article <- c(abstract, methods, footnotes, ack)

  # Can use xpath, but x2 slower (8 vs 4ms) and cannot get refs
  # article_xml %>%
  #     xml_find_all("//text()[contains(translate(., 'REGIST', 'regist'), \
  #                  'regist') or contains(translate(., 'TRIAL', 'trial'),\
  #                  'trial') or contains(., 'NCT') or contains(., 'PROSPERO')\
  #                  ]") %>%
  #     xml_text()

  # Can use article as list (negligible change in performance)
  # article <-
  #   list(abstract, methods, footnotes, ack) %>%
  #   purrr::compact() %>%
  #   purrr::map(~ keep(.x, ~ grepl(relevant_regex, .x))) %>%
  #   purrr::compact()

  # Adding PROPSERO adds negligible overhead
  rel_regex <- "\\b(|-)([Rr]egist|(|[Cc]linical)[Tt]rial|NCT[0-9]{8}|ISRCTN|ChiCTR|PROSPERO)"
  article %<>% purrr::keep(stringr::str_detect, pattern = rel_regex)


  out$is_relevant <- !!length(article)

  # Check for relevance
  if (!out$is_relevant) {

    return(tibble::as_tibble(c(out, index_any, index_method)))

  }

  # Check for methods
  out$is_method <- !!length(methods)

  if (!out$is_method) {

    return(tibble::as_tibble(c(out, index_any, index_method)))

  }

  # Activate if I want to check for title here.
  # if (!!length(index_any$reg_title_pmc)) {
  #
  #   i <- index_any$reg_title_pmc[2]
  #   is_true <- negate_reg_title_1(article[i])
  #
  #   if (is_true) {
  #
  #   index <- unlist(index_any)
  #   out[["is_register_pred"]] <- TRUE
  #   out[["register_text"]] <- article[index] %>% paste(collapse = " ")
  #
  #   return(tibble::as_tibble(c(out, index_any, index_method)))
  #   }
  # }


  # Text pre-processing
  article_processed <-
    article %>%
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT', sub = "") %>%   # keep first
    obliterate_fullstop_1() %>%
    obliterate_semicolon_1() %>%  # adds minimal overhead
    obliterate_comma_1() %>%   # adds minimal overhead
    obliterate_apostrophe_1() %>%
    obliterate_punct_1() %>%
    obliterate_line_break_1() %>%
    obliterate_refs_2()

  # Way faster than: out[["is_NCT"]] <- ...
  out$is_NCT <- purrr::some(article, stringr::str_detect, "NCT[0-9]{8}")

  synonyms <- .create_synonyms()
  index_any$reg_title_pmc <- integer()
  index_any$prospero_1 <- get_prospero_1(article_processed)
  index_any$registered_1 <- get_registered_1(article_processed, synonyms)
  index_any$registered_2 <- get_registered_2(article_processed, synonyms)
  index_any$registered_3 <- get_registered_3(article_processed, synonyms)
  index_any$registered_4 <- get_registered_4(article_processed, synonyms)
  index_any$registered_5 <- get_registered_5(article_processed, synonyms)
  index_any$not_registered_1 <- get_not_registered_1(article_processed, synonyms)
  index_any$registration_1 <- get_registration_1(article_processed, synonyms)
  index_any$registration_2 <- get_registration_2(article_processed, synonyms)
  index_any$registration_3 <- get_registration_3(article_processed)
  index_any$registration_4 <- get_registration_4(article_processed, synonyms)
  index_any$registry_1 <- get_registry_1(article_processed, synonyms)
  index_any$reg_title_1 <- get_reg_title_1(article_processed, synonyms)
  index_any$reg_title_2 <- get_reg_title_2(article_processed, synonyms)
  index_any$reg_title_3 <- get_reg_title_3(article_processed, synonyms)
  index_any$reg_title_4 <- get_reg_title_4(article_processed, synonyms)
  index_any$funded_ct_1 <- get_funded_ct_1(article_processed, synonyms)
  index <- unlist(index_any) %>% unique() %>% sort()

  # Tidier but takes a median 11.0 ms vs current, which takes 10.6 ms
#   index_any <- list(
#     prospero_1 = NA,
#     registered_1 = NA,
#     registered_2 = NA,
#     registered_3 = NA,
#     registered_4 = NA,
#     registered_5 = NA,
#     not_registered_1 = NA,
#     registration_1 = NA,
#     registration_2 = NA,
#     registration_3 = NA,
#     registration_4 = NA,
#     registry_1  = NA,
#     reg_title_1 = NA,
#     reg_title_2 = NA,
#     reg_title_3 = NA,
#     reg_title_4 = NA,
#     funded_ct_1 = NA
#   )
#
#   func <- list(
#     get_prospero_1,
#     get_registered_1,
#     get_registered_2,
#     get_registered_3,
#     get_registered_4,
#     get_registered_5,
#     get_not_registered_1,
#     get_registration_1,
#     get_registration_2,
#     get_registration_3,
#     get_registration_4,
#     get_registry_1,
#     get_reg_title_1,
#     get_reg_title_2,
#     get_reg_title_3,
#     get_reg_title_4,
#     get_funded_ct_1
#   )
#
#   pepa <- article %>% purrr::invoke_map(func, .)
#   index_any %<>% list_modify(!!!pepa)
# }


  if (!!length(index)) {

    out$is_explicit <- !!length(unlist(index_any))
    out$is_register_pred <- !!length(index)
    out$register_text <- article[index] %>% paste(collapse = " ")

    index_any %<>% purrr::map(function(x) !!length(x))

    return(tibble::as_tibble(c(out, index_any, index_method)))
  }


  # Apply a more sensitive search in Methods
  if (out$is_method) {

    # x30 faster than obliterating methods again
    # article_processed %<>% purrr::keep(article %in% methods)
    i <- which(article %in% methods)

    # methods %<>%
    #   obliterate_fullstop_1() %>%
    #   obliterate_semicolon_1() %>%  # adds minimal overhead
    #   obliterate_comma_1() %>%   # adds minimal overhead
    #   obliterate_apostrophe_1() %>%
    #   obliterate_hash_1() %>%
    #   obliterate_backlash_1() %>%
    #   obliterate_line_break_1()
    #
    # article %<>% purrr::keep(magrittr::is_in, methods)

    index_method$ct_2 <- get_ct_2(article_processed[i], synonyms)
    index_method$ct_3 <- get_ct_3(article_processed[i], synonyms)
    index_method$protocol_1 <- get_protocol_1(article_processed[i], synonyms)

    index <- i[unlist(index_method) %>% unique() %>% sort()]
    index_method %<>% purrr::map(function(x) !!length(x))
  }

  out$is_register_pred <- !!length(index)
  out$register_text <- article[index] %>% paste(collapse = " ")

  index_any %<>% purrr::map(function(x) !!length(x))

  if (out$is_register_pred) {

    out$is_explicit <- FALSE

  }

  return(tibble::as_tibble(c(out, index_any, index_method)))
}


.rt_register_pmc <- function(article_ls, pmc_reg_ls, dict) {

  # Creating and outputting these lists adds negligible time
  # Way faster than index_any[["reg_title_pmc"]] <- NA
  index_any <- list(
    prospero_1 = NA,
    registered_1 = NA,
    registered_2 = NA,
    registered_3 = NA,
    registered_4 = NA,
    registered_5 = NA,
    not_registered_1 = NA,
    registration_1 = NA,
    registration_2 = NA,
    registration_3 = NA,
    registration_4 = NA,
    registry_1  = NA,
    reg_title_1 = NA,
    reg_title_2 = NA,
    reg_title_3 = NA,
    reg_title_4 = NA,
    funded_ct_1 = NA
  )

  index_method <- list(
    ct_2 = NA,
    ct_3 = NA,
    protocol_1 = NA
  )

  out <- list(  # do not change order of pmid:doi - id extraction depends on it
    is_relevant_reg = NA,
    is_method = NA,
    is_NCT = NA,
    is_register_pred = FALSE,
    register_text = "",
    is_explicit_reg = NA
  )


  if (!pmc_reg_ls$is_research & !pmc_reg_ls$is_review) {

    return(c(out, index_any, index_method))

  }


  if (pmc_reg_ls$is_register_pred) {

    out$is_relevant_reg <- TRUE
    out$is_explicit_reg <- TRUE
    out$is_register_pred <- TRUE
    out$register_text <- pmc_reg_ls$register_text

    return(c(out, index_any, index_method))
  }



  # TODO Consider adding unique
  article <-
    article_ls[c("ack", "methods", "abstract", "footnotes")] %>%
    unlist()
    # unique()

  methods <- unlist(article_ls$methods)

  reg_regex <- "\\b(|-)([Rr]egist|(|[Cc]linical)[Tt]rial|NCT[0-9]{8}|ISRCTN|ChiCTR|PROSPERO)"
  article %<>% purrr::keep(stringr::str_detect, pattern = reg_regex)


  out$is_relevant_reg <- !!length(article)

  # Check for relevance
  if (!out$is_relevant_reg) {

    return(c(out, index_any, index_method))

  }

  # Check for methods
  out$is_method <- !!length(methods)

  if (!out$is_method) {

    return(c(out, index_any, index_method))

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
    obliterate_refs_2()


  # Way faster than: out[["is_NCT"]] <- ...
  out$is_NCT <- purrr::some(article, stringr::str_detect, "NCT[0-9]{8}")

  index_any$prospero_1 <- get_prospero_1(article_processed)
  index_any$registered_1 <- get_registered_1(article_processed, dict)
  index_any$registered_2 <- get_registered_2(article_processed, dict)
  index_any$registered_3 <- get_registered_3(article_processed, dict)
  index_any$registered_4 <- get_registered_4(article_processed, dict)
  index_any$registered_5 <- get_registered_5(article_processed, dict)
  index_any$not_registered_1 <- get_not_registered_1(article_processed, dict)
  index_any$registration_1 <- get_registration_1(article_processed, dict)
  index_any$registration_2 <- get_registration_2(article_processed, dict)
  index_any$registration_3 <- get_registration_3(article_processed)
  index_any$registration_4 <- get_registration_4(article_processed, dict)
  index_any$registry_1 <- get_registry_1(article_processed, dict)
  index_any$reg_title_1 <- get_reg_title_1(article_processed, dict)
  index_any$reg_title_2 <- get_reg_title_2(article_processed)
  index_any$reg_title_3 <- get_reg_title_3(article_processed, dict)
  index_any$reg_title_4 <- get_reg_title_4(article_processed, dict)
  index_any$funded_ct_1 <- get_funded_ct_1(article_processed, dict)
  index <- unlist(index_any) %>% unique() %>% sort()


  if (!!length(index)) {

    out$is_explicit_reg <- !!length(unlist(index_any))
    out$is_register_pred <- !!length(index)
    out$register_text <- article[index] %>% paste(collapse = " ")

    index_any %<>% purrr::map(function(x) !!length(x))

    return(c(out, index_any, index_method))
  }


  # Apply a more sensitive search in Methods
  if (out$is_method) {

    # x30 faster than obliterating methods again
    # article_processed %<>% purrr::keep(article %in% methods)
    i <- which(article %in% methods)

    # methods %<>%
    #   obliterate_fullstop_1() %>%
    #   obliterate_semicolon_1() %>%  # adds minimal overhead
    #   obliterate_comma_1() %>%   # adds minimal overhead
    #   obliterate_apostrophe_1() %>%
    #   obliterate_hash_1() %>%
    #   obliterate_backlash_1() %>%
    #   obliterate_line_break_1()
    #
    # article %<>% purrr::keep(magrittr::is_in, methods)

    index_method$ct_2 <- get_ct_2(article_processed[i], dict)
    index_method$ct_3 <- get_ct_3(article_processed[i], dict)
    index_method$protocol_1 <- get_protocol_1(article_processed[i], dict)

    index <- i[unlist(index_method) %>% unique() %>% sort()]
    index_method %<>% purrr::map(function(x) !!length(x))
  }

  out$is_register_pred <- !!length(index)
  out$register_text <- article[index] %>% paste(collapse = " ")

  index_any %<>% purrr::map(function(x) !!length(x))

  if (out$is_register_pred) {

    out$is_explicit_reg <- FALSE

  }

  return(c(out, index_any, index_method))
}



rt_register_pmc2 <- function(article_xml) {

  # Creating and outputting these lists adds negligible time
  # Way faster than index_any[["reg_title_pmc"]] <- NA
  index_any <- list(
    reg_title_pmc = NA,
    prospero_1 = NA,
    registered_1 = NA,
    registered_2 = NA,
    registered_3 = NA,
    registered_4 = NA,
    registered_5 = NA,
    not_registered_1 = NA,
    registration_1 = NA,
    registration_2 = NA,
    registration_3 = NA,
    registration_4 = NA,
    registry_1  = NA,
    reg_title_1 = NA,
    reg_title_2 = NA,
    reg_title_3 = NA,
    reg_title_4 = NA,
    funded_ct_1 = NA
  )

  index_method <- list(
    ct_2 = NA,
    ct_3 = NA,
    protocol_1 = NA
  )

  out <- list(  # do not change order of pmid:doi - id extraction depends on it
    pmid = NA,
    pmcid_pmc = NA,
    pmcid_uid = NA,
    doi = NA,
    is_research = NA,
    is_review = NA,
    is_slr = FALSE,
    is_relevant = NA,
    is_method = NA,
    is_NCT = NA,
    is_register_pred = FALSE,
    register_text = "",
    is_explicit = NA
  )

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

    return(tibble::as_tibble(c(out, index_any, index_method)))

  }

  # Check for SLR (code is marginally faster without this)
  # if (out$is_review) {
  #
  #   txt <- xml_text(article_xml)
  #   out$is_slr <- grepl("systematic review", txt, ignore.case = T)
  #
  #   if (!out$is_slr) {
  #
  #     return(tibble::as_tibble(c(out, index_any, index_method)))
  #
  #   }
  # }


  # Go through titles
  title_txt <- .get_register_pmc_title(article_xml)
  is_title <- nchar(title_txt) > 0

  if (is_title) {

    index_any$reg_title_pmc <- TRUE
    out$is_relevant <- TRUE
    out$is_explicit <- TRUE
    out$is_register_pred <- TRUE
    out$register_text <- title_txt

    return(tibble::as_tibble(c(out, index_any, index_method)))

  }


  # Check for relevance
  # txt <- xml_text(article_xml)
  # rel_regex <- "\\b(|-)([Rr]egist|(|[Cc]linical)[Tt]rial|NCT[0-9]{8}|PROSPERO)"
  # out$is_relevant <- grepl(rel_regex, txt)
  #
  # if (!out$is_relevant) {
  #
  #   return(tibble::as_tibble(c(out, index_any, index_method)))
  #
  # }


  # Check for methods (10x more costly than xml_text(article_xml))
  # methods <- .xml_methods(article_xml)
  # out$is_method <- !!length(methods)
  #
  # if (!out$is_method) {
  #
  #   return(tibble::as_tibble(c(out, index_any, index_method)))
  #
  # }


  # Extract article text into a list
  # article[["ack"]] <- .xml_ack(article_xml)
  # article[["methods"]] <- .xml_methods(article_xml)
  # article[["abstract"]] <- .xml_abstract(article_xml)
  # article[["footnotes"]] <- .xml_footnotes(article_xml)


  # Extract article text into a vector
  ack <- .xml_ack(article_xml)
  methods <- .xml_methods(article_xml)
  abstract <- .xml_abstract(article_xml)
  footnotes <- .xml_footnotes(article_xml)
  article <- c(abstract, methods, footnotes, ack)

  # Can use xpath, but x2 slower (8 vs 4ms) and cannot get refs
  # article_xml %>%
  #     xml_find_all("//text()[contains(translate(., 'REGIST', 'regist'), \
  #                  'regist') or contains(translate(., 'TRIAL', 'trial'),\
  #                  'trial') or contains(., 'NCT') or contains(., 'PROSPERO')\
  #                  ]") %>%
  #     xml_text()

  # Can use article as list (negligible change in performance)
  # article <-
  #   list(abstract, methods, footnotes, ack) %>%
  #   purrr::compact() %>%
  #   purrr::map(~ keep(.x, ~ grepl(relevant_regex, .x))) %>%
  #   purrr::compact()

  # Adding PROPSERO adds negligible overhead
  rel_regex <- "\\b(|-)([Rr]egist|(|[Cc]linical)[Tt]rial|NCT[0-9]{8}|ISRCTN|ChiCTR|PROSPERO)"
  article %<>% purrr::keep(stringr::str_detect, pattern = rel_regex)


  out$is_relevant <- !!length(article)

  # Check for relevance
  if (!out$is_relevant) {

    return(tibble::as_tibble(c(out, index_any, index_method)))

  }

  # Check for methods
  out$is_method <- !!length(methods)

  if (!out$is_method) {

    return(tibble::as_tibble(c(out, index_any, index_method)))

  }

  # Activate if I want to check for title here.
  # if (!!length(index_any$reg_title_pmc)) {
  #
  #   i <- index_any$reg_title_pmc[2]
  #   is_true <- negate_reg_title_1(article[i])
  #
  #   if (is_true) {
  #
  #   index <- unlist(index_any)
  #   out[["is_register_pred"]] <- TRUE
  #   out[["register_text"]] <- article[index] %>% paste(collapse = " ")
  #
  #   return(tibble::as_tibble(c(out, index_any, index_method)))
  #   }
  # }


  # Text pre-processing
  article_processed <-
    article %>%
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT', sub = "") %>%   # keep first
    obliterate_fullstop_1() %>%
    obliterate_semicolon_1() %>%  # adds minimal overhead
    obliterate_comma_1() %>%   # adds minimal overhead
    obliterate_apostrophe_1() %>%
    obliterate_punct_1() %>%
    obliterate_line_break_1() %>%
    obliterate_refs_2()

  # Way faster than: out[["is_NCT"]] <- ...
  out$is_NCT <- purrr::some(article, stringr::str_detect, "NCT[0-9]{8}")

  synonyms <- .create_synonyms()
  index_any$reg_title_pmc <- integer()
  index_any$prospero_1 <- get_prospero_1(article_processed)
  index_any$registered_1 <- get_registered_1(article_processed, synonyms)
  index_any$registered_2 <- get_registered_2(article_processed, synonyms)
  index_any$registered_3 <- get_registered_3(article_processed, synonyms)
  index_any$registered_4 <- get_registered_4(article_processed, synonyms)
  index_any$registered_5 <- get_registered_5(article_processed, synonyms)
  index_any$not_registered_1 <- get_not_registered_1(article_processed, synonyms)
  index_any$registration_1 <- get_registration_1(article_processed, synonyms)
  index_any$registration_2 <- get_registration_2(article_processed, synonyms)
  index_any$registration_3 <- get_registration_3(article_processed)
  index_any$registration_4 <- get_registration_4(article_processed, synonyms)
  index_any$registry_1 <- get_registry_1(article_processed, synonyms)
  index_any$reg_title_1 <- get_reg_title_1(article_processed, synonyms)
  index_any$reg_title_2 <- get_reg_title_2(article_processed, synonyms)
  index_any$reg_title_3 <- get_reg_title_3(article_processed, synonyms)
  index_any$reg_title_4 <- get_reg_title_4(article_processed, synonyms)
  index_any$funded_ct_1 <- get_funded_ct_1(article_processed, synonyms)
  index <- unlist(index_any) %>% unique() %>% sort()

  # Tidier but takes a median 11.0 ms vs current, which takes 10.6 ms
  #   index_any <- list(
  #     prospero_1 = NA,
  #     registered_1 = NA,
  #     registered_2 = NA,
  #     registered_3 = NA,
  #     registered_4 = NA,
  #     registered_5 = NA,
  #     not_registered_1 = NA,
  #     registration_1 = NA,
  #     registration_2 = NA,
  #     registration_3 = NA,
  #     registration_4 = NA,
  #     registry_1  = NA,
  #     reg_title_1 = NA,
  #     reg_title_2 = NA,
  #     reg_title_3 = NA,
  #     reg_title_4 = NA,
  #     funded_ct_1 = NA
  #   )
  #
  #   func <- list(
  #     get_prospero_1,
  #     get_registered_1,
  #     get_registered_2,
  #     get_registered_3,
  #     get_registered_4,
  #     get_registered_5,
  #     get_not_registered_1,
  #     get_registration_1,
  #     get_registration_2,
  #     get_registration_3,
  #     get_registration_4,
  #     get_registry_1,
  #     get_reg_title_1,
  #     get_reg_title_2,
  #     get_reg_title_3,
  #     get_reg_title_4,
  #     get_funded_ct_1
  #   )
  #
  #   pepa <- article %>% purrr::invoke_map(func, .)
  #   index_any %<>% list_modify(!!!pepa)
  # }


  if (!!length(index)) {

    out$is_explicit <- !!length(unlist(index_any))
    out$is_register_pred <- !!length(index)
    out$register_text <- article[index] %>% paste(collapse = " ")

    index_any %<>% purrr::map(function(x) !!length(x))

    return(tibble::as_tibble(c(out, index_any, index_method)))
  }


  # Apply a more sensitive search in Methods
  if (out$is_method) {

    # x30 faster than obliterating methods again
    # article_processed %<>% purrr::keep(article %in% methods)
    i <- which(article %in% methods)

    # methods %<>%
    #   obliterate_fullstop_1() %>%
    #   obliterate_semicolon_1() %>%  # adds minimal overhead
    #   obliterate_comma_1() %>%   # adds minimal overhead
    #   obliterate_apostrophe_1() %>%
    #   obliterate_hash_1() %>%
    #   obliterate_backlash_1() %>%
    #   obliterate_line_break_1()
    #
    # article %<>% purrr::keep(magrittr::is_in, methods)

    index_method$ct_2 <- get_ct_2(article_processed[i], synonyms)
    index_method$ct_3 <- get_ct_3(article_processed[i], synonyms)
    index_method$protocol_1 <- get_protocol_1(article_processed[i], synonyms)

    index <- i[unlist(index_method) %>% unique() %>% sort()]
    index_method %<>% purrr::map(function(x) !!length(x))
  }

  out$is_register_pred <- !!length(index)
  out$register_text <- article[index] %>% paste(collapse = " ")

  index_any %<>% purrr::map(function(x) !!length(x))

  if (out$is_register_pred) {

    out$is_explicit <- FALSE

  }

  return(tibble::as_tibble(c(out, index_any, index_method)))
}
