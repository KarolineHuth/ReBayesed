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
  plot <- agg_point %>%
    ggplot(aes(x=BGGM_estimate)) +
    ggdist::stat_halfeye(.width = 0.95,
                         color = "black",
                         point_interval = median_hdi) +
    # geom_vline(xintercept = median_density, linetype="dashed", color="red") +
    # geom_point(aes(x = density, y = -0.05), size = 2, color = "black", shape = 1) +
    geom_jitter(aes(x = BGGM_estimate,
                    y = -0.05),
                width = 0,
                height = 0.025,
                color = "black",
                shape = 1,
                alpha = 0.1,
                size = 0.025) +
    gg.theme("clean") +
    annotate("text",
             x = hdi$y,
             y = 0.12,
             label = paste0("Median: ",
                            str_sub(round(hdi$y, 3), 2)),
             color = "black",
             vjust = -0.5) +
    annotate("text",
             x = hdi$ymin,
             y = 0.02,
             label = hdi_min_text,
             color = "black",
             vjust = -0.5) +
    annotate("text",
             x = hdi$ymax,
             y = 0.02,
             label = hdi_max_text,
             color = "black",
             vjust = -0.5) +
    labs(title = "Density Plot with 95% HDI and Median ",
         x = "Edge Density",
         y = "")

  return(plot)
}
