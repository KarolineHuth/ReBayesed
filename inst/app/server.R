
library(shiny)
library(DT)
library(tidyverse)
library(qgraph)
library(ggdist)
library(readxl)
library(magrittr)
library(grid)
library(gridExtra)
library(ggplotify)
library(gtools)


# Define server logic
server <- function(input, output, session) {

  #### LOAD Data
  # agg_data_point <- data_point
  # agg_data_level <- data_level
  # agg_data_list <- data_list
  # metadata <- mdata
  # Load data
  agg_data_list <- readRDS("AggStudyResults.rds")
  agg_data_level <- readRDS("IndividualStudyData.rds")
  agg_data_point <- readRDS("EdgeSpecificEstimates.rds")
  suppressMessages({
    metadata <- read_excel("metadata.xlsx")
  })

  #
  # agg_data_level <- readRDS(system.file("extdata", "IndividualStudyData.rds",
  #                                       package = "ReBayesed"))
  # agg_data_point <- readRDS(system.file("extdata", "EdgeSpecificEstimates.rds",
  #                                       package = "ReBayesed"))
  # #
  # # # Load metadata
  # suppressMessages({
  #   metadata <- read_excel(system.file("extdata", "metadata.xlsx",
  #                                      package = "ReBayesed"))
  # })

  ##### ABOUT PAGE ######
  # Create contact link
  observeEvent(input$contactBtn, {
    showModal(modalDialog(
      title = "Contact Us",
      HTML("You can reach us at: <a href='mailto:networkreanalysis-fmg@uva.nl'>networkreanalysis-fmg@uva.nl</a>"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  # Custom JavaScript for copying text to clipboard
  observeEvent(input$copyCitation, {
    citationText <- c("Huth, K., Haslbeck, J. M. B., Keetelaar, S., van Holst, R.J., & Marsman, M.(2024). Statistical Evidence in Psychological Networks: A Bayesian Analysis of 294 Networks from 126 Studies. Retrieved from osf.io/n8r9g.")
    js$copyToClipboard(citationText)
    showNotification("Citation copied to clipboard!")
  })

  shinyjs::extendShinyjs(
    text = "
    shinyjs.copyToClipboard = function(params) {
      // Ensure params is a string
      var textToCopy = params || '';

      // Create a temporary textarea element
      var copyText = document.createElement('textarea');
      copyText.value = textToCopy; // Set the value to the text to copy
      document.body.appendChild(copyText); // Append it to the body
      copyText.select(); // Select the text

      // Try copying to clipboard
      try {
        var successful = document.execCommand('copy');
        if (!successful) {
          console.error('Failed to copy text to clipboard');
        }
      } catch (err) {
        console.error('Error copying text: ', err);
      }

      // Remove the textarea element from the DOM
      document.body.removeChild(copyText);
    }
  ",
    functions = c("copyToClipboard")
  )



  ## Fix NaN/Inf in inclusion prob / BF issue:
  # Get the number of columns to check/fix
  n_to_fix <- grepl("BF", names(agg_data_point)) %>% sum()
  if(n_to_fix > 0){ # If there are columns to fix:
    # Get the columns to check and fix
    check_cols <- agg_data_point[grepl("BF", names(agg_data_point))]
    fix_cols <- agg_data_point[grepl("inc_prob", names(agg_data_point))]

    for(i in 1:n_to_fix){ # For each column to check/fix:
      # Check for rows with infinite evidence for inclusion
      fix_rows <- is.infinite(check_cols[,i])

      if(sum(fix_rows) > 0) # If there are rows to fix:
        # Set the inclusion probability to 1 (instead of default NaN) for rows with infinite evidence for inclusion
        fix_cols[fix_rows, i] <- 1
    }
    # Replace the fixed columns in the original data
    agg_data_point[grepl("inc_prob", names(agg_data_point))] <- fix_cols
  }


  # first row is the column names
  colnames(metadata) <- as.character(metadata[1,])
  metadata <- metadata[-1,]

  # deal with unnamed columns
  colnames(metadata)[is.na(colnames(metadata))] <-
    paste0("Unnamed_", seq_along(colnames(metadata))[is.na(colnames(metadata))])

  # Apply our metadata extraction function to each entry in agg_data_list,
  # filter by selected topics,
  # and combine in a reactive df
  table_data <- reactive({
    agg_data_df <- lapply(agg_data_list, extract_metadata) %>%
      do.call(rbind, .) %>%
      dplyr::filter(Topic %in% input$topicCheckbox) %>%
      dplyr::filter(SampleType %in% input$clinicalCheckbox, TRUE) %>%
      rownames_to_column() %>%
      rename("NetworkID" = "rowname")

    # Get relevant metadata for each network
    metadata_df <- metadata %>%
      dplyr::filter(NetworkID %in% agg_data_df$NetworkID) %>%
      rename(Questionnaires = "Questionnaires used",
             DataLink = "Repository Link") %>%
      select(NetworkID, Questionnaires, DataLink)

    # Merge the two dataframes
    merged_df <- merge(agg_data_df, metadata_df, by = "NetworkID", all.x = TRUE) %>%
      # mutate(Questionnaires = as.factor(Questionnaires)) %>%
      select(Reference, Year, DOI, Topic, Subtopic, Questionnaires, SampleType,
             Sample.size, Nodes, Edges, Model, DataLink, NetworkID)

    # Sort by NetworkID
    merged_df <- merged_df[merged_df$NetworkID %>% mixedorder(),]

    # Redo rownames
    rownames(merged_df) <- 1:nrow(merged_df)

    return(merged_df)
  })


  ### INDIVIDUAL STUDIES START ###
  shinyInput <- function(FUN, len, id, label = NULL, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), label, ...))
    }
    inputs
  }


  js_combined <- c(
    "$(document).on('click', '[id^=checkb]', function(){",
    "  var id = this.getAttribute('id');",
    "  var i = parseInt(id.replace('checkb', ''));",
    "  var value = $(this).prop('checked');",
    "  Shiny.setInputValue('checkbox_state', {row: i, value: value}, {priority: 'event'});",
    "});",
    "$(document).on('click', '[id^=plotBtn]', function(){",
    "  var id = this.getAttribute('id');",
    "  var i = parseInt(id.replace('plotBtn', ''));",
    "  Shiny.setInputValue('plot_button_click', i, {priority: 'event'});",
    "  setTimeout(function() {",
    "    Shiny.setInputValue('plot_button_click', null);",
    "  }, 1);",
    "});",
    "$(document).on('shiny:inputchanged', function(event) {",
    "  if (event.name === 'select_all') {",
    "    var selectAll = event.value;",
    "    if (selectAll) {",
    "      $('[id^=checkb]').prop('checked', true).change();",
    "    } else {",
    "      $('[id^=checkb]').prop('checked', false).change();",
    "    }",
    "    // Trigger an input change to update the R server",
    "    var checkedBoxes = $('[id^=checkb]').map(function(){ return this.checked; }).get();",
    "    Shiny.setInputValue('all_checkboxes', checkedBoxes, {priority: 'event'});",
    "  }",
    "});"
  )

  # Render indStudiesTable
  output$indStudiesTable <- renderDT({
    datatable(cbind(action = shinyInput(actionButton, nrow(table_data()), "plotBtn", "Plots"),
                    checkb = shinyInput(checkboxInput, nrow(table_data()), "checkb"),
                    table_data()),
              colnames = c("", "Select", "Authors", "Year", "DOI", "Topic", "Subtopic",
                           "Questionnaires", "Sample Type", "Sample Size", "Nodes",
                           "Edges", "Model", "Data Link", "Network ID"),
              # rownames = FALSE, # removing rownames like this doesn't work, need to use custom CSS
              selection = 'none',
              callback = JS(js_combined),
              escape = c(-2, -3, -5, -14), # Allows HTML in cells
              filter = 'top',
              options = list(
                autoWidth = FALSE, # THIS FUCKS UP THE BUTTON WIDTH -> MAKE FALSE, THEN MANUALLY SET WIDTHS FOR COLS
                scrollX = TRUE,
                pageLength = 100,
                columnDefs = list(
                  list(targets = c(2,4), width = '40px'),
                  list(targets = c(1,10,11,12,13), width = '50px'),
                  list(targets = c(6,9,15), width = '70px'),
                  list(targets = c(3), width = '90px'),
                  list(targets = c(7,8), width = '150px'),
                  list(targets = 5, width = '200px',
                       render = JS( # Render DOI as a clickable hyperlink
                         "function(data, type, row, meta) {",
                         " if (data === null) {",
                         "  return '';",
                         " } else {",
                         "  return '<a href=\"https://doi.org/' + data + '\" target=\"_blank\">' + data + '</a>';",
                         " }",
                         "}"
                      )
                    ),
                  list(targets = 14, width = '30px',
                    # Render Data Link as a clickable hyperlink, exclude if NA
                    render = JS(
                      "function(data, type, row, meta) {",
                      " if (data === null) {",
                      "  return '';",
                      " } else {",
                      "  return '<a href=\"' + data + '\" target=\"_blank\">Link</a>';",
                      " }",
                      "}"
                      )
                    )
                  # list(targets = 15, width = '2%'),
                  # list(targets = 16, width = '2%')
                  )
                )
              )
    })

  # Render par_plot and edge_ plots
  observeEvent(input$plot_button_click, {
    # Get the clicked row
    clicked_row <- input$plot_button_click

    if (!is.null(clicked_row)) {
      # Get the ID + data of the clicked network
      selected_network <- table_data()[clicked_row, "NetworkID"]
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
          title = "Network Plots",
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
          ))
        )
      )
    }
  })


  # Reactive variable to store checked network IDs
  checked_network_ids <- reactiveVal(character())

  # Observe network checkbox changes
  observeEvent(input$checkbox_state, {
    checkbox_state <- input$checkbox_state # get which checkbox was clicked
    row <- checkbox_state$row # get the row of the clicked checkbox
    value <- checkbox_state$value # get what the new value of the clicked checkbox is

    current_ids <- checked_network_ids() # get the current list of checked network IDs

    clicked_id <- table_data()[row, "NetworkID"] # get the Network ID of the clicked checkbox
    if (value) {
      # add Network ID to list if checked
      checked_network_ids(unique(c(current_ids, clicked_id)))
    } else {
      # remove Network ID from list if unchecked
      checked_network_ids(setdiff(current_ids, clicked_id))
    }
  })

  # observe all_checkboxes input to handle select_all button
  observeEvent(input$all_checkboxes, {
    all_checked <- input$all_checkboxes
    if (is.null(all_checked))
      return()

    # get the Network IDs of all currently displayed rows
    network_ids <- table_data()[input$indStudiesTable_rows_all, "NetworkID"]

    if (all(all_checked)) {
      # All checkboxes are selected
      checked_network_ids(network_ids)
    } else {
      # No checkboxes are selected
      checked_network_ids(character())
    }
  })

  # observe({
  #   print(checked_network_ids())
  # })


  # Download Handler for Table Data
  output$downloadIndStudiesTable <- downloadHandler(
    filename = function() {
      paste("filtered-network-data-", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      saveRDS(agg_data_list[names(agg_data_list) %in% checked_network_ids()],
              file)
    }
  )

  # Download Handler for Network Data
  output$downloadNetworkData <- downloadHandler(
    filename = function() {
      name = paste(table_data()[input$plot_button_click, "NetworkID"], ".rds", sep="")
      return(name)
    },
    content = function(file) {
      selected_network <- table_data()[input$plot_button_click, "NetworkID"]
      network_results <- agg_data_list[[selected_network]]
      saveRDS(network_results, file)
    }
  )

  ### INDIVIDUAL STUDIES END ###

  # ### METADATA START ###
  # metadata_r <- reactive({
  #   # warning(input$rank_list_1)
  #   # warning(input$rank_list_2)
  #
  #   return("AAA")
  # })
  #
  # output$bucket_check <- renderPrint({
  #   metadata_r()
  # })
  #
  # # output$emergencyPlot <- renderPlot({
  # #   metadata_r |>
  # #     ggplot(aes(x = .)) +
  # #     geom_bar(fill = "#ffa500") +
  # #     gg.theme("clean") +
  # #     ylab("Count") +
  # #     theme(text=element_text(size=21))
  # # })
  #
  # ### METADATA END ###

  ### ESTIMATES START ###

  # If no networks are selected in 'Individual Studies', disable 'Use selected networks' checkbox
  observeEvent(checked_network_ids(), {
    shinyjs::toggleState('useSelectedNetworks',
                         condition = length(checked_network_ids()) != 0)

    # Reset the 'Use selected networks' checkbox if no networks are selected
    #  (relevant if user:
    #  1. selects networks
    #  2. then checks 'Use selected networks'
    #  3. then unselects all networks in 'Individual Studies' pane)
    if(checked_network_ids() %>% length() == 0)
      updateCheckboxInput(session, "useSelectedNetworks", value = FALSE)
  })

  # If 'Use selected networks' is checked, disable all other inputs
  observeEvent(input$useSelectedNetworks, {
    shinyjs::toggleState('topicCheckboxEstimates',
                         condition = !input$useSelectedNetworks)
    shinyjs::toggleState('clinicalCheckboxEstimates',
                         condition = !input$useSelectedNetworks)
    shinyjs::toggleState('yearSliderEstimates',
                         condition = !input$useSelectedNetworks)
    shinyjs::toggleState('nNodesSliderEstimates',
                         condition = !input$useSelectedNetworks)
    shinyjs::toggleState('sampleSizeEstimates',
                         condition = !input$useSelectedNetworks)
  }, ignoreNULL = TRUE)

  # If 'sampleSizeEstimates' max value is NOT set to 5000, disable 'sampleSizeOutliersEstimates' checkbox
  observeEvent(input$sampleSizeEstimates, {
    shinyjs::toggleState('sampleSizeOutliersEstimates',
                         condition = input$sampleSizeEstimates[2] == 5000)

    # Reset the 'sampleSizeOutliersEstimates' checkbox if max value is not 5000
    if(input$sampleSizeEstimates[2] != 5000)
      updateCheckboxInput(session, "sampleSizeOutliersEstimates", value = FALSE)
  }, ignoreNULL = TRUE)


  # Determine what data to use for 'Estimates' plots
  estimates_data_real <- reactive({
    # Get the aggregated data
    estimates_df <- agg_data_list %>%
      lapply(., extract_metadata) %>%
      do.call(rbind, .) %>%
      rownames_to_column() %>%
      rename("NetworkID" = "rowname")

    # Check if user has selected any studies in the 'Individual Studies' pane AND
    #       if user wants to use selected networks
    if(length(checked_network_ids()) > 0 & input$useSelectedNetworks){
      # Use selected networks
      estimates_df %<>%
        dplyr::filter(NetworkID %in% checked_network_ids())

    } else {
      # Use all networks and filter based on user input in sidebar
      estimates_df %<>%
        dplyr::filter(Topic %in% input$topicCheckboxEstimates) %>%
        dplyr::filter(SampleType %in% input$clinicalCheckboxEstimates) %>%
        dplyr::filter(Year >= input$yearSliderEstimates[1] & Year <= input$yearSliderEstimates[2]) %>%
        dplyr::filter(Nodes >= input$nNodesSliderEstimates[1] & Nodes <= input$nNodesSliderEstimates[2]) %>%
        dplyr::filter(Sample.size >= input$sampleSizeEstimates[1]) # Filter based on min sample size first

      # If 'sampleSizeOutliersEstimates' is unchecked (most cases), filter based on max sample size too
      if(!input$sampleSizeOutliersEstimates){
        estimates_df %<>%
          dplyr::filter(Sample.size <= input$sampleSizeEstimates[2])
      }
    }
    # Select the network IDs
    estimates_df %<>%
      dplyr::select(NetworkID) %>%
      pull()

    return(estimates_df)
  })

  # Render netDensity
  output$netDensityDensity <- renderPlot({
    net_density_density_plot(agg_data_point[agg_data_point$NetworkID %in% estimates_data_real(),])
  })

  # Render netEdgeDensity
  output$netEdgeDensity <- renderPlot({
    net_edge_density_plot(agg_data_point[agg_data_point$NetworkID %in% estimates_data_real(),])
  })

  ## Render optional plots based on plot checkbox selection
  # Generate plots based on checkbox selection and estimates_data_real()
  plot_reactive <- reactive({
    fvb_incl_plot <- if ("fvb_incl" %in% input$estimatesPlotsCheckbox) {
      bayes_perc_cat(agg_data_point[agg_data_point$NetworkID %in% estimates_data_real(),])
    } else {
      NULL
    }

    edge_est_post_incl_plot <- if ("edge_est_post_incl" %in% input$estimatesPlotsCheckbox) {
      edge_est_vs_post_incl_prob(agg_data_point[agg_data_point$NetworkID %in% estimates_data_real(),])
    } else {
      NULL
    }

    fvb_est_plot <- if ("fvb_est" %in% input$estimatesPlotsCheckbox) {
      freq_vs_bayes_est_plot(agg_data_point[agg_data_point$NetworkID %in% estimates_data_real(),])
    } else {
      NULL
    }

    plots <- list(edge_est_post_incl_plot = edge_est_post_incl_plot,
                  fvb_est_plot = fvb_est_plot,
                  fvb_incl_plot = fvb_incl_plot)

    # Create shared legend if at least one plot is present
    if(all(sapply(plots, is.null))){
      # No plots present
      legend_shared <- NULL
    } else {
      # At least one plot present, take the first non-NULL plot and add legend
      plot_w_legend <- plots[[which(!sapply(plots, is.null))[1]]] +
        labs(color = "Evidence Category") +
        guides(color = guide_legend(override.aes = list(size = 10))) +
        theme(legend.position = "right",
              legend.title = element_text(size = 25),
              legend.text = element_text(size = 25))

      # Extract the legend
      g <- ggplotGrob(plot_w_legend)$grobs
      legend_shared <- g[[which(sapply(g, function(x) x$name) == "guide-box")]] %>%
        ggplotify::as.ggplot()
    }

    plots <- list( fvb_est_plot = fvb_est_plot,
                  fvb_incl_plot = fvb_incl_plot,
                  edge_est_post_incl_plot = edge_est_post_incl_plot,
                  legend_shared = legend_shared)

    return(plots)
  })

  # Observe the reactive expression and render plots
  observe({
    plots <- plot_reactive()

    output$freqVsBayesInclBar <- renderPlot({plots$fvb_incl_plot})
    output$edgeEstVsPostInclProb <- renderPlot({plots$edge_est_post_incl_plot})
    output$freqEstVsBayesEst <- renderPlot({plots$fvb_est_plot})
    output$plotsLegend <- renderPlot({plots$legend_shared})
  })


  # # Render edgeEstVsPostInclProb
  # output$edgeEstVsPostInclProb <- renderPlot({
  #   edge_est_vs_post_incl_prob(agg_data_point[agg_data_point$networkID %in% estimates_data(),])
  # })
  #
  # # Render freqVsBayesEstPlot
  # output$freqEstVsBayesEst <- renderPlot({
  #   freq_vs_bayes_est_plot(agg_data_point[agg_data_point$networkID %in% estimates_data(),])
  # })


  ### ESTIMATES END ###
  ### DOCUMENTATION START ###

    ### DOCUMENTATION END ###
}
