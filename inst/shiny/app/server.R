library(shiny)
library(DT)
library(tidyverse)
library(qgraph)
library(ggdist)
library(NRP.web)
library(readxl)

# Define server logic
server <- function(input, output, session) {

  # Load data
  agg_data_list <- readRDS(system.file("extdata/AggStudyResults.RData",
                                       package = "NRP.web"))
  agg_data_level <- readRDS(system.file("extdata/StudyLevelofInclusion.RData",
                                        package = "NRP.web"))
  agg_data_point <- readRDS(system.file("extdata/EdgeSpecificEstimates.RData",
                                        package = "NRP.web"))
  metadata <- read_excel(system.file("extdata/metadata.xlsx",
                                     package = "NRP.web"))

  # Apply our metadata extraction function to each entry in agg_data_list,
  # filter by selected topics,
  # and combine in a reactive df
  table_data <- reactive({
    lapply(agg_data_list, extract_metadata) %>%
      do.call(rbind, .) %>%
      dplyr::filter(Topic %in% input$topicCheckbox) %>%
      dplyr::filter(SampleType %in% input$clinicalCheckbox, TRUE) %>%
      rownames_to_column() %>%
      rename("Network_ID" = "rowname")
  })

  ### INDIVIDUAL STUDIES START ###
  # Render indStudiesTable
  output$indStudiesTable <- renderDT({
    datatable(table_data(), filter = 'top', options = list(pageLength = 25), selection = 'single')
  }, server = TRUE)

  # Render par_plot and edge_ plots
  observeEvent(input$indStudiesTable_rows_selected, {
    # Get the selected row
    selected_row <- input$indStudiesTable_rows_selected

    if (length(selected_row) == 1) {
      selected_network <- table_data()[selected_row, "Network_ID"]
      network_results <- agg_data_list[[selected_network]]

      # Render parameter estimate plot
      output$par_plot <- renderPlot({
        network_par_plot(network_results)
      })

      # Render edge evidence inclusion plot
      output$edge_inc_plot <- renderPlot({
        edge_evidence_plot(network_results, inclusion = TRUE)
      })

      # Render edge evidence exclusion plot
      output$edge_exc_plot <- renderPlot({
        edge_evidence_plot(network_results, inclusion = FALSE)
      })

      # Show plots in pop-up window
      showModal(
        modalDialog(
          title = "test",
          fluidRow(
            column(4, plotOutput("par_plot")),
            column(4, plotOutput("edge_inc_plot")),
            column(4, plotOutput("edge_exc_plot"))
          ),
          downloadButton("downloadNetworkData", "Download Network Data"),
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          tags$style(HTML("
            .modal-dialog{
            width: 80%;
            max-width: 1200px;
            }"
                          )
                     )
          )
        )
    }
  })

  # Download Handler for Table Data
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("network-data-", Sys.Date(), ".Rdata", sep="")
    },
    content = function(file) {
      saveRDS(table_data(), file)
    }
  )

  # Download Handler for Network Data
  output$downloadNetworkData <- downloadHandler(
    filename = function() {
      row_name <- rownames(table_data()[input$indStudiesTable_rows_selected, ])
      name = paste(row_name, ".Rdata", sep="")
      return(name)
    },
    content = function(file) {
      selected_row <- input$indStudiesTable_rows_selected
      network_results <- agg_data_list[[selected_row]]
      saveRDS(network_results, file)
    }
  )

  ### INDIVIDUAL STUDIES END ###

  ### METADATA START ###
  metadata_r <- reactive({
    warning(input$rank_list_1)
    warning(input$rank_list_2)

    return("AAA")
  })

  output$bucket_check <- renderPrint({
    metadata_r()
  })

  # output$emergencyPlot <- renderPlot({
  #   metadata_r |>
  #     ggplot(aes(x = .)) +
  #     geom_bar(fill = "#ffa500") +
  #     gg.theme("clean") +
  #     ylab("Count") +
  #     theme(text=element_text(size=21))
  # })

  ### METADATA END ###

  ### ESTIMATES START ###

  # Filter data based on user input
  estimates_data <- reactive({
    lapply(agg_data_list, extract_metadata) %>%
    do.call(rbind, .) %>%
    dplyr::filter(Topic %in% input$topicCheckboxEstimates) %>%
    dplyr::filter(SampleType %in% input$clinicalCheckboxEstimates) %>%
    dplyr::filter(Year >= input$yearSliderEstimates[1] & Year <= input$yearSliderEstimates[2]) %>%
    dplyr::filter(Nodes >= input$nNodesSliderEstimates[1] & Nodes <= input$nNodesSliderEstimates[2]) %>%
    dplyr::filter(Edges >= input$nEdgesSliderEstimates[1] & Edges <= input$nEdgesSliderEstimates[2]) %>%
    dplyr::filter(Sample.size >= input$sampleSizeEstimates[1] & Sample.size <= input$sampleSizeEstimates[2]) %>%
    rownames_to_column() %>%
    rename("Network_ID" = "rowname") %>%
    dplyr::select(Network_ID) %>%
    pull()
  })

  # Render freqVsBayesInclBar
  output$freqVsBayesInclBar <- renderPlot({
    warning(estimates_data())
    freq_vs_bayes_incl_bar(agg_data_level[agg_data_level$networkID %in% estimates_data(),])
  })

  # Render edgeEstVsPostInclProb
  output$edgeEstVsPostInclProb <- renderPlot({
    edge_est_vs_post_incl_prob(agg_data_point[agg_data_point$networkID %in% estimates_data(),])
  })

  # Render freqVsBayesEstPlot
  output$freqEstVsBayesEst <- renderPlot({
    freq_vs_bayes_est_plot(agg_data_point[agg_data_point$networkID %in% estimates_data(),])
  })

  output$netDensity <- renderPlot({
    net_density_plot(agg_data_point[agg_data_point$networkID %in% estimates_data(),])
  })

  ### ESTIMATES END ###
}
