#' @title Parse Citation into In-text Reference, Year and DOI
#'
#' @decsription This function extracts the in-text reference, year and DOI from a citation.
#'
#' @param citation A character vector containing the citation in APA format.
#'
#' @return A list containing the in-text reference, year and DOI.
#'
#' @export
parse_citation <- function(citation){

  # Check if citation is missing in metadata
  if(is.null(citation)){
    return(list(in_text_ref = NA, year = NA, doi = NA))
  }

  # Extract authors from citation
  authors <- citation %>%
    str_extract("^[^\\(]+") %>%
    str_extract_all("\\b\\w{2,}\\b") %>%
    unlist()

  # Create in-text reference
  in_text_ref <- if(length(authors) > 2){
    paste(authors[1], "et al.", sep = " ")
  } else if (length(authors) == 2){
    paste(authors, sep = " & ")
  } else {
    authors
  }

  # Extract year
  year <- str_match(citation, "\\((\\d{4})\\)")[,2]

  # Extract DOI
  doi <- str_match(citation, "(?i)\\bdoi.*(10\\.\\S+)")[,2]

  return(list(in_text_ref = in_text_ref, year = year, doi = doi))
}
