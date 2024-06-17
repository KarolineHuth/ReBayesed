#' @title Frequentist vs. Bayesian Parameter Estimate Scatterplot
#'
#' @description This function creates a scatterplot of the frequentist and Bayesian parameter estimates.
#'
#' @param agg_point A data frame with the estimates for each edge, for all studies.
#'
#' @return A scatterplot of the frequentist and Bayesian parameter estimates.
#' @export

freq_vs_bayes_est_plot <- function(agg_point){
  plot <- ggplot(agg_point, aes(x=modSelect_estimate,y=BGGM_estimate)) +
    geom_point(color = agg_point$BGGM_color, shape = 20, size = 2) +
    scale_x_continuous(name="Frequentist Estimate",limits=c(-0.8,0.8),
                       breaks=c(-0.6, -0.4, -0.2, 0, .2,.4,0.6)) +
    scale_y_continuous(name="Bayesian Estimate",limits=c(-0.8,0.8),
                       breaks=c(-0.6, -0.4, -0.2, 0, .2, .4, 0.6)) +
    scale_color_manual(values=c("#36648b", "#86a2b9" , "grey", "#d69999", "#990000"),
                       breaks=c('included', 'weak included', 'inconclusive', 'weak excluded', 'excluded'))+
    ggtitle("") + xlab("") + ylab("") +
    labs(title = "Frequentist vs. Bayesian Edge Parameter Estimates", color = "Category")+
    gg.theme("clean")
    # theme(legend.position= "none",
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
