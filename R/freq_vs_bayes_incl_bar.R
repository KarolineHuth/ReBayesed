#' @title Percentage of edges in the respective evidence categories
#'
#' @description This function creates a bar chart that shows the percentage of edges falling into respective edge categories.
#'
#' @param agg_point A data frame with the edge specicific information.
#'
#' @return A bar chart that shows the percentage of edges in the respective categories.
#' @export

freq_vs_bayes_incl_bar <- function(agg_point){


  set.seed(1) # for consistent jittering
  plot <- agg_point |>
    mutate(Category = factor(BGGM3_category, levels = c('included', 'weak included', 'inconclusive', 'weak excluded', 'excluded'))) |>
    group_by(Category) |>
    summarize(n = n(),
              Value = (n/nrow(agg_point))*100) |>
    ggplot(aes(fill=Category, y=Value, x = "", group = Category)) +
    geom_bar(position="stack", stat="identity") +
    geom_text(aes(label = round(Value, 1)), size = 7, position = position_stack(vjust = 0.5), family = "serif")+
    scale_fill_manual(values = c("#eeb004", "#f9d183", "grey", "#86a2b9", "#36648b"),
                      breaks=c('excluded', 'weak excluded', 'inconclusive','weak included','included')) +
    coord_flip() +
    theme_bw(base_size = 16, base_family="Arial") +
    ylab("Percentage") +
    xlab("")+
    labs(fill = "Category")+
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


  # agg_level |>
  # mutate(Category = factor(Category,
  #                          levels = c('included', 'weak included', 'inconclusive', 'weak excluded', 'excluded'))) |>
  # filter(Estimation %in% c("BGGM_3")) |>
  # ggplot(aes(x = Category, y = Value, group = Category,  color = Category))+
  # geom_jitter(alpha = 0.3, size = 2.5) +
  # coord_flip()+
  # geom_boxplot(width = 0.6, alpha = .5, outlier.size = 2.5, outlier.alpha = .9)  +
  # scale_color_manual(values=c("#36648b", "#86a2b9" , "grey", "#f9d183", "#eeb004"),
  #                    breaks=c('included', 'weak included', 'inconclusive', 'weak excluded', 'excluded'))+
  # # scale_fill_manual(values=c("#36648b", "#86a2b9" , "grey", "#d69999", "#990000"),
  # #                   breaks=c('included', 'weak included', 'inconclusive', 'weak excluded', 'excluded'))+
  # xlab("Category") + ylab("Percentage") +
  # theme_bw(base_size = 16, base_family="Arial") +
  # theme(legend.position = "none",
  #       axis.line.y = element_blank(),  # Remove the y-axis line
  #       axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank(),
  #       axis.text.x     = element_text(size = 14),
  #       axis.title.y    = element_text(vjust = +1.5),
  #       panel.grid.major  = element_blank(),
  #       panel.grid.minor  = element_blank(),
  #       legend.background = element_blank(),
  #       legend.key = element_blank(),
  #       panel.border = element_blank(),
  #       panel.background = element_blank(),
  #       axis.line  = element_line(colour = "black"))

  return(plot)

}
