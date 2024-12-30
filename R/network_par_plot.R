#' @title Network Parameter Plot
#'
#' @description This function creates a network plot of the parameters from the BGGM results.
#'
#' @param network_results A list containing the results from fitting the network model.
#'
#' @return A network plot of the parameters from the BGGM results.
#' @export
network_par_plot <- function(network_results){

  # Create layout for the network plots
  net_layout <- qgraph::averageLayout(network_results$BGGM_parameters)

  graph <- network_results$BGGM_parameters
  graph[network_results$BGGM_inc_probs < 0.5] <- 0

  # Create network parameter plot
  par_plot <- qgraph::qgraph(graph,
                             layout = net_layout,
                             theme = "TeamFortress",
                             title = "Parameter Plot",
                             vsize = 8,
                             title.cex = 1.6,
                             edge.width = 1,
                             node.width = 1.1,
                             node.height = 1.1,
                             label.prop = 0.925,
                             legend = FALSE,
                             labels = colnames(network_results$BGGM_parameters)
  )

  return(par_plot)
}
