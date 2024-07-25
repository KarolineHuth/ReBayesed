#' @title Network Estimate Density
#'
#' @description This function creates a density plot of the edge estimates in all networks.
#'
#' @param agg_point A data frame with the estimates for each edge, for all studies.
#'
#' @return A density plot of the edge estimates in all networks.
#' @export

net_edge_density_plot <- function(agg_point){
  # calculate median and HDI
  hdi <- ggdist::median_hdci(agg_point$BGGM_estimate, na.rm = TRUE, .width = 0.95)

  # format HDI values (APA7)
  hdi_min_text <- if(hdi$ymin == 0){
    "0"
  } else {
    is_neg <- hdi$ymin < 0
    if(is_neg){
      str_sub(round(hdi$ymin, 3), 3) %>%
        paste0("-", .)
    } else {
      str_sub(round(hdi$ymin, 3), 2)
    }
  }

  hdi_max_text <- if(hdi$ymax == 1){
    "1"
  } else {
    str_sub(round(hdi$ymax, 3), 2)
  }

  # create plot
  set.seed(1) # for consistent jitter
  plot <- agg_point %>%
    ggplot(aes(x=BGGM_estimate)) +
    ggdist::stat_halfeye(.width = 0.95, point_interval = median_hdci,
                         color = "black") +
    # geom_vline(xintercept = median_density, linetype="dashed", color="red") +
    # geom_point(aes(x = density, y = -0.05), size = 2, color = "black", shape = 1) +
    geom_jitter(aes(x = BGGM_estimate, y = -0.05),
                width = 0, height = 0.025, color = "black", shape = 1, alpha = 0.1, size = 0.025) +
    scale_x_continuous(limits=c(-0.7, 0.7),
                       breaks=c(-0.6, -0.4, -0.2, 0, .2,.4,0.6)) +
    # gg.theme("clean") +
    # annotate("text", x = hdi$y, y = 0.12,
    #          label = paste0("Median: ", str_sub(round(hdi$y, 3), 2)), color = "black", vjust = -0.5) +
    # annotate("text", x = hdi$ymin, y = 0.02,
    #          label = hdi_min_text, color = "black", vjust = -0.5) +
    # annotate("text", x = hdi$ymax, y = 0.02,
    #          label = hdi_max_text, color = "black", vjust = -0.5) +
    labs(x = "Edge Density") +
    theme_bw(base_size = 16, base_family="Arial") +
    theme(axis.text.x     = element_text(size = 14),
          axis.title.y    = element_blank(),
          # title           = element_blank(),
          panel.grid.major  = element_blank(),
          panel.grid.minor  = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line  = element_line(colour = "black"))

  # add panel to plot in top right corner with median value
  plot <- plot +
    annotation_custom(
      grob = ggpubr::text_grob(
        label = paste0("Median: ", str_sub(round(hdi$y, 3), 2)),
        x = 0.9, y = 0.9,
        hjust = 0.95, vjust = 0.95
    )
  ) +
    annotation_custom(
      grob = ggpubr::text_grob(
        label = paste0("HDI: [", hdi_min_text, ", ", hdi_max_text, "]"),
        x = 0.9, y = 0.85,
        hjust = 0.95, vjust = 0.95
      )
    )

  return(plot)
}
