#' @title Frequentist Vs. Bayesian Edge Inclusion Bar Chart
#'
#' @description This function creates a bar chart that compares the Bayesian and Frequentist edge inclusions,
#'
#' @param agg_level A data frame with the aggregated evidence for inclusion for each edge.
#'
#' @return A bar chart that compares the Bayesian and Frequentist edge inclusions.
#' @export

freq_vs_bayes_incl_bar <- function(agg_level){

  # all_parameterized <- agg_level |>
  #   filter(Estimation %in% c("BGGM", "modSelect")) |>
  #   mutate(Measure = factor(Measure, levels = c('included', 'weak included', 'inconclusive', 'weak excluded', 'excluded')),
  #          Estimation = ifelse(Estimation == "BGGM", "Bayesian", "Frequentist"),
  #          Parameterization = "All",
  #          Parameterization_cat = "All") |>
  #   group_by(Measure, Estimation) |>
  #   summarize(n = n(),
  #             Value = sum(Value)/n)
  #
  # plot <- all_parameterized |>
  #   ggplot(aes(fill=Measure, y=Value, x=Estimation, group = Measure)) +
  #   geom_bar(position="stack", stat="identity") +
  #   # geom_text(aes(label = round(Value, 1)), size = 3, hjust = 1.2, vjust = 0, position = "stack")+
  #   scale_fill_manual(values = c("#990000", "#d69999", "grey", "#86a2b9", "#36648b"),
  #                     breaks=c('excluded', 'weak excluded', 'inconclusive','weak included','included')) +
  #   scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
  #   # facet_grid(cols = vars(networkID)) +
  #   # facet_grid(Estimation ~.) +
  #   gg.theme("clean") +
  #   coord_flip() +
  #   theme(legend.position="top") +
  #   labs(title ="Frequentist vs. Bayesian Evidence for Edge Inclusion", x = "", y = "", fill = "")

  plot <- agg_level |>
    mutate(Category = factor(Category,
                             levels = c('included', 'weak included', 'inconclusive', 'weak excluded', 'excluded'))) |>
    filter(Estimation %in% c("BGGM_2")) |> # changed to BGGM_2
    ggplot(aes(x = Category, y = Value, group = Category,  color = Category))+
    geom_jitter(alpha = 0.3, size = 2.5) +
    coord_flip()+
    geom_boxplot(width = 0.6, alpha = .5, outlier.size = 2.5, outlier.alpha = .9)  +
    scale_color_manual(values=c("#36648b", "#86a2b9" , "grey", "#d69999", "#990000"),
                       breaks=c('included', 'weak included', 'inconclusive', 'weak excluded', 'excluded'))+
    # scale_fill_manual(values=c("#36648b", "#86a2b9" , "grey", "#d69999", "#990000"),
    #                   breaks=c('included', 'weak included', 'inconclusive', 'weak excluded', 'excluded'))+
    xlab("Category") + ylab("Percentage") +
    # gg.theme("clean") +
    theme_bw(base_size = 16, base_family="Arial") +
    theme(legend.position = "none",
          axis.line.y = element_blank(),  # Remove the y-axis line
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x     = element_text(size = 14),
          axis.title.y    = element_text(vjust = +1.5),
          panel.grid.major  = element_blank(),
          panel.grid.minor  = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line  = element_line(colour = "black"))

  return(plot)

}
