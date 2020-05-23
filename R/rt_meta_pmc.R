rt_meta_pmc <- function(filename, remove_ns = F) {

  # A lot of the PMC XML files are malformed
  article_xml <- tryCatch(.get_xml(filename, remove_ns), error = function(e) e)

  if (inherits(article_xml, "error")) {

    return(tibble::tibble(filename, is_success = F))

  }

  id_ls <- list(filename = filename)
  meta_ls <- .xml_metadata_c(article_xml, as_list = T)

  status_ls <- list(is_success = T)
  tibble::as_tibble(c(id_ls, meta_ls, status_ls))
}
