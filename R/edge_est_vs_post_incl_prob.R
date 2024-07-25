#' @title Network Edge Estimates Vs. Posterior Inclusion Probabilities
#'
#' @description This function creates a scatter plot that compares the network edge estimates to their posterior inclusion probabilities.
#'
#' @param agg_point A data frame with the estimates for each edge, for all studies.
#'
#' @return A scatter plot that compares the network edge estimates to their posterior inclusion probabilities.
#' @export

edge_est_vs_post_incl_prob <- function(agg_point){

  # y_labels <- c("")

  plot <- ggplot(agg_point,aes(x=BGGM3_estimate,y=log(BGGM3_BF), color = BGGM3_color)) +
    geom_vline(aes(xintercept=0),linetype=2) +
    geom_hline(aes(yintercept=log(1/10)),linetype=2, color = "#990000") +
    geom_hline(aes(yintercept=log(1/3)),linetype=2, color = "#d69999") +
    geom_hline(aes(yintercept=log(3)),linetype=2, color = "#86a2b9") +
    geom_hline(aes(yintercept=log(10)),linetype=2, color = "#36648b") +
    geom_point(shape = 20, size = 2) +
    annotate("text", x = 0, y = log(30), label = "^", size = 8, color = "#172543") +  # Indicator for points above y limit
    annotate("text", x = 0.3, y = log(30), label = "^", size = 8, color = "#172543") +  # Indicator for points above y limit
    annotate("text", x = -0.3, y = log(30), label = "^", size = 8, color = "#172543") +  # Indicator for points above y limit
    annotate("text", x = 0.6, y = log(30), label = "^", size = 8, color = "#172543") +  # Indicator for points above y limit
    annotate("text", x = -0.6, y = log(30), label = "^", size = 8, color = "#172543") +  # Indicator for points above y limit
    # annotate("text", x = -0.2, y = 3.12, label = "^", vjust = -1.5, size = 8, color = "black") +
    # annotate("text", x = -0.08, y = 3.12, label = "^", vjust = -1.5, size = 8, color = "black") +
    # annotate("text", x = 0.08, y = 3.12, label = "^", vjust = -1.5, size = 8, color = "black") +
    # annotate("text", x = 0.2, y = 3.12, label = "^", vjust = -1.5, size = 8, color = "black") +
    # annotate("text", x = 0.35, y = 3.12, label = "^", vjust = -1.5, size = 8, color = "black") +
    # annotate("segment", x = 0.35, xend = .35, y = log(30), yend = 3.6,
    #          arrow = arrow(length = unit(0.2, "cm")), color = "black") +
    scale_x_continuous(name="Estimate", limits=c(-0.6,0.6),
                       breaks=c(-0.6, -0.4, -0.2, 0, .2,.4, 0.6)) +
    scale_y_continuous(name="log(Inclusion BF)", limits=c(-3.5,3.45),
                       breaks = c(log(1/10),log(1/3), log(1), log(3), log(10)),
                       labels = c("log(1/10)","log(1/3)", "log(1)", "log(3)", "log(10)")
                       # sec.axis = sec_axis(~ ., name = "Evidence",
                       #                     breaks = c(log(1/20), log(1/6), log(1), log(6), log(20)),
                       #                     labels = c("excl.", "weak excl.", "inconclusive", "weak incl.", "incl."))
    ) +
    scale_color_identity(breaks = c("#36648b", "#86a2b9" , "grey", "#d69999", "#990000"),
                         labels = c('Included', 'Weak Included', 'Inconclusive', 'Weak Excluded', 'Excluded'),
                         guide = "legend") +
    ggtitle("") + xlab("") + ylab("") +
    labs(color = "Category")+
    # scale_color_discrete(name="Inclusion") +
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

  # # might be a way to implement a scale break, but it doesn't work well with the current plot
  # plot + ggbreak::scale_y_break(c(log(30), log(50)))

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
