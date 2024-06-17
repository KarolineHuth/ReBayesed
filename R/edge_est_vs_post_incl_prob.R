#' @title Network Edge Estimates Vs. Posterior Inclusion Probabilities
#'
#' @description This function creates a scatter plot that compares the network edge estimates to their posterior inclusion probabilities.
#'
#' @param agg_point A data frame with the estimates for each edge, for all studies.
#'
#' @return A scatter plot that compares the network edge estimates to their posterior inclusion probabilities.
#' @export

edge_est_vs_post_incl_prob <- function(agg_point){

  plot <- ggplot(agg_point,aes(x=BGGM_estimate, y=BGGM_inc_prob, color = BGGM_category)) +
    geom_vline(aes(xintercept=0),linetype=2) +
    geom_hline(aes(yintercept=0.1),linetype=2, color = "#990000") +
    geom_hline(aes(yintercept=0.25),linetype=2, color = "#d69999") +
    geom_hline(aes(yintercept=0.75),linetype=2, color = "#86a2b9") +
    geom_hline(aes(yintercept=0.9),linetype=2, color = "#36648b") +
    geom_point(color = agg_point$BGGM_color,
               shape = 1, size = 2, alpha = 0.75) +
    scale_x_continuous(limits=c(-0.6,0.6),
                       breaks=c(-0.6, -0.4, -0.2, 0, .2,.4,0.6)) +
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0.1,.25, .75,0.9)) +
    scale_color_manual(values=c("#36648b", "#86a2b9" , "grey", "#d69999", "#990000"),
                       breaks=c('included', 'weak included', 'inconclusive', 'weak excluded', 'excluded'))+
    labs(title = "Edge Estimate vs. Posterior Inclusion Probability (PIP)", x = "Estimate", y = "PIP", color = "Category")+
    # scale_color_discrete(name="Inclusion") +
    gg.theme("clean")
    # theme(legend.position=c(.17,.185),
    #       legend.text=element_text(size=16),
    #       legend.title=element_text(size=18),
    #       plot.margin = unit(c(-2,-1.5,2,2), "lines"),
    #       axis.text.x=element_text(size=20),
    #       axis.text.y=element_text(size=20),
    #       axis.title.x=element_text(size=25, vjust=-1.6),
    #       axis.title.y=element_text(size=26, vjust=2.6),
    #       panel.grid.minor = element_blank())

  return(plot)
}
