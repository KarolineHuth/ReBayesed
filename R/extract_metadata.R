#' @title Relevant Network Metadata from `AggStudyResults.RData`
#'
#' @description This function extracts relevant metadata from the `AggStudyResults.RData` file.
#'
#' @param entry A list entry from the `AggStudyResults.RData` file.
#' @return A data frame with the following columns:
#' \itemize{
#'   n_nodes: The number of nodes in the network.
#'   n_edges: The number of edges in the network.
#'   sample_size: The sample size of the study.
#'   model: The model used to generate the network.
#'   topic: The topic of the study.
#'   subtopic: The subtopic of the study.}
#' @export
extract_metadata <- function(entry) {
  # Parse the citation
  parsed_citation <- parse_citation(entry$meta$citation)

  # Extract the metadata
  entry_metadata_df <- data.frame(
    Reference = parsed_citation$in_text_ref,
    Year = as.numeric(parsed_citation$year),
    DOI = parsed_citation$doi,
    Nodes = entry$p,
    Edges = entry$n,
    `Sample size` = entry$meta$sample_size,
    Model = entry$meta$model,
    Topic = entry$meta$topic,
    SampleType = entry$meta$sample_type,
    `Subtopic` = entry$meta$subtopic,
    stringsAsFactors = FALSE
  )

  return(entry_metadata_df)
}
