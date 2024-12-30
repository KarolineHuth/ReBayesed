#' @title Network Edge Inclusion Evidence Plot
#'
#' @description This function creates a network plot of the evidence for edge inclusion from the BGGM results.
#'
#' @param network_results A list containing the results from fitting the network model.
#' @param inclusion A logical value indicating whether to plot the edge inclusion evidence (if FALSE, then evidence for exclusion is plotted).
#'
#' @return A network plot of the parameters from the BGGM results.
#' @export
edge_evidence_plot <- function(network_results, inclusion = TRUE){

  # Create layout for the network plots
  net_layout <- qgraph::averageLayout(network_results$BGGM_parameters)

  # Extract the edge inclusion evidence
  graph <- network_results$BGGM_BF
  graph[] <- 1

  # Change
  if(inclusion){
    graph[network_results$BGGM_inc_probs < 0.5] <- 0
    title <- "Edge Inclusion Evidence Plot"
  } else {
    graph[network_results$BGGM_inc_probs >= 0.5] <- 0
    title <- "Edge Exclusion Evidence Plot"
  }
  # we changed the colors after fitting to yellow instead of red to avoid confusion
  evidence_color <- network_results$BGGM_color
  evidence_color[evidence_color== "#990000"] <- "#eeb004"
  evidence_color[evidence_color== "#d69999"] <- "#f9d183"

  # Create the evidence plot
  edge_evidence_plot <- qgraph::qgraph(graph,
                                       edge.color = evidence_color,
                                       layout = net_layout,
                                       theme = "TeamFortress",
                                       title = title,
                                       vsize = 8,
                                       title.cex = 1.6,
                                       edge.width = 4,
                                       node.width = 1.1,
                                       node.height = 1.1,
                                       label.prop = 0.925,
                                       labels = colnames(network_results$BGGM_parameters)
  )

  return(edge_evidence_plot)
}
