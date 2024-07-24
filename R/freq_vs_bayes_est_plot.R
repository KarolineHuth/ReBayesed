#' @title Frequentist vs. Bayesian Parameter Estimate Scatterplot
#'
#' @description This function creates a scatterplot of the frequentist and Bayesian parameter estimates.
#'
#' @param agg_point A data frame with the estimates for each edge, for all studies.
#'
#' @return A scatterplot of the frequentist and Bayesian parameter estimates.
#'
#' @export

freq_vs_bayes_est_plot <- function(agg_point){
  plot <- ggplot(agg_point, aes(x = modSelect_estimate, y = BGGM3_estimate, color = BGGM3_color)) +
    geom_vline(xintercept = 0, linewidth = 0.5, linetype = 3, alpha = 0.25)+
    geom_hline(yintercept = 0, linewidth = 0.5, linetype = 3, alpha = 0.25)+
    geom_point(shape = 20, size = 2) +
    # geom_abline(linewidth = 2, linetype = "longdash")+
    scale_x_continuous(name="Frequentist Estimate",limits=c(-0.7, 0.7),
                       breaks=c(-0.6, -0.4, -0.2, 0, .2,.4,0.6)) +
    scale_y_continuous(name="Bayesian Estimate",limits=c(-0.7, 0.7),
                       breaks=c(-0.6, -0.4, -0.2, 0, .2,.4,0.6)) +
    scale_color_identity(breaks = c("#36648b", "#86a2b9" , "grey", "#d69999", "#990000"),
                         labels = c('Included', 'Weak Included', 'Inconclusive', 'Weak Excluded', 'Excluded'),
                         guide = "legend") +
    ggtitle("") + xlab("") + ylab("") +
    labs(title = "", color = "Category") +
    theme_bw(base_size = 16, base_family="Arial") +
    theme(axis.text.x     = element_text(size = 14),
          axis.title.y    = element_text(vjust = +1.5),
          panel.grid.major  = element_blank(),
          panel.grid.minor  = element_blank(),
          legend.position = "none",
          legend.background = element_blank(),
          legend.key = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line  = element_line(colour = "black"))
    # scale_color_discrete(name="Inclusion") +
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
