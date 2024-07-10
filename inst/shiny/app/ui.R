library(shiny)
library(shinyjs)
library(DT)
library(bslib)
library(sortable)
library(shinyjs)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    table.dataTable tbody td:nth-child(2) {
      width: 50px;
    }
  "))),
  # Your other UI components
)

# Define UI
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "yeti"),
  "RobustNetPsych",
  tabPanel(
    "About",
    fluidPage(
      tags$head(
        tags$style(HTML("table {table-layout: fixed;")),
        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
      ),
      useShinyjs(),  # Initialize shinyjs
      titlePanel("RobustNetPsych: Bayesian Insights into Network Stability and Evidence"),
      br(),  # Add a line break to create space
      h2("What is this Shiny App?"),
      p("Via this web-app, we aim to share all the obtained resources from a large-scale Bayesian re-analysis project. In particular, we share the re-analysed network fits which allows user to gain an insight into the particular network of interest. It has two main tabs."),
      h3("Invidual Studies"),
      p("This tab includes an overview of all obtained networks from the various papers. Several networks might stem from one paper. The table displays various meta-data information about the network such as citation, topic, questionnaires, sample size and number of nodes. Users can select the networks to be shown in the table with the panel on the left. By clicking on the panel, a pop-up window opens which shows three network plots. The plot on the left shows the estimated edge strength network, where the thickness and saturation of an edge represents the partial association strength. The two right plots show the edge evidence categorization. The middle plot shows all edges that have some evidence for inclusion with grey edges depicting slight but less than weak, light blue edges weak, and dark blue edges strong evidence for inclusion. The far right plot shows all edges with some evidence for edge exclusion. Grey edges indicate slight but less than weak, light red weak, and dark red strong evidence for exclusion."),
      p("With the download button, users can obtain a list of network-specific results. Each network-specific list, contains meta-data, parameter estimates, parameter inclusion evidence, and general data information such as covariance estimates. For a detailed overview of the network-specific information see the",
        a("background information", href = "https://osf.io/x9q84", target = "_blank"),
        "."),
      h3("Estimates"),
      p("This tab shows the distribution of the parameter estimates and the density of the network (i.e., percentage of present edges). Optionally, users can choose to show the (1) Bayesian parameter estimates against the edge inclusion evidence, (2) frequentist against Bayesian parameter estimates, and (3) the percentage of (un)certainty in the networks. The subset of networks on which the plots are generated can be selected with the sidebar."),
      br(),  # Add a line break to create space
      h2("What is the Project?"),
      p("Network analysis has become extremely popular in all psychological disciplines, especially in clinical psychology. Along with the enthusiasm for networks, there has been growing concern about the stability of their results. In interpreting results and accumulating our understanding across studies, we need to be aware of the potential uncertainty underlying the estimated networks."),
      p("In this project, our goal was to assess the robustness of published psychological networks. We systematically searched and re-analyzed previously published networks to address questions such as: Is there evidence for conditional independence when an edge is missing from an estimated network, or is the edge simply too unstable to be included? Also, how much evidence do we have for the inclusion of edges? To adequately assess the robustness of the networks, we used a Bayesian approach. This approach allows quantification of the uncertainty of the networks (Huth et al., 2023); in particular, to determine the uncertainty of the estimated network structure, to obtain the statistical evidence for the inclusion or exclusion of edges, and to quantify the precision of the network parameters."),
      p("The project was led by Karoline Huth, Sara Keetelaar, Jonas Haslbeck, and Maarten Marsman. For more information on this topic and the group, visit the ",
        a("Bayesian Graphical Modeling website", href = "https://bayesiangraphicalmodeling.com/re-analysis/", target = "_blank"),
        "."
      ),
      br(),  # Add a line break to create space
      h2("Citation"),
      p("Please cite the web app and the materials obtained from it accordingly. Furthermore, in case specific datasets are being used, please cite the original article. The citation can be found in the respective network information."),
      div(style = "border: 1px solid grey; background-color: #f0f0f0; padding: 10px; margin-top: 10px;",
          "Huth, K. B. S., Haslbeck, J. M. B., Keetelaar, S., van Holst, R., & Marsman, M. (2024). Statistical Evidence in Published Psychometric Networks: A Bayesian Re-Analysis of 144 Papers. Retrieved from osf.io/n8r9g",
          br(),
          actionButton("copyCitation", "Copy Citation", icon = icon("copy"))
      ),
      br(),
      h2("Questions?"),
      p("Please contact us via email."),
      actionButton("contactBtn", "Contact", icon = icon("envelope"))
    )

  ),
  tabPanel("Individual Studies",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               checkboxGroupInput("topicCheckbox",
                                  "Topics",
                                  choices = c("Clinical",
                                              "Social",
                                              "Personality",
                                              "Work- and Organizational",
                                              "Diagnostics",
                                              "Public Health",
                                              "Education",
                                              "Other"),
                                  selected = c("Clinical",
                                               "Social",
                                               "Personality",
                                               "Work- and Organizational",
                                               "Diagnostics",
                                               "Public Health",
                                               "Education",
                                               "Other")),
               conditionalPanel(
                 # condition = "input.topicCheckbox.indexOf('Clinical') !== -1 |
                 #              input.topicCheckbox.indexOf('Diagnostics') !== -1",
                 condition = "TRUE",
                 checkboxGroupInput("clinicalCheckbox",
                                    "Sample Type for Clinical/Diagnostics Studies",
                                    choices = c("General Population" = "Population",
                                                "Clinical",
                                                "Mixed"),
                                    selected = "Population"
                                    )
               ),
               checkboxInput("select_all",
                             "Select/Deselect All"),
               br(),
               downloadButton("downloadIndStudiesTable",
                              "Download Filtered Network Results as RDS")
             ),
             mainPanel(
               width = 9,
               DTOutput("indStudiesTable"),
               br()
             )
           )
  ),
  tabPanel("Meta Data",
           sidebarLayout(
             sidebarPanel(
               width = 4,
               checkboxGroupInput("topicCheckboxMetadata",
                                  "Topics",
                                  choices = c("Clinical",
                                              "Social",
                                              "Personality",
                                              "Work- and Organizational",
                                              "Diagnostics",
                                              "Public Health",
                                              "Education",
                                              "Other"),
                                  selected = c("Clinical",
                                               "Social",
                                               "Personality",
                                               "Work- and Organizational",
                                               "Diagnostics",
                                               "Public Health",
                                               "Education",
                                               "Other")
               ),
               conditionalPanel(
                 # condition = "input.topicCheckbox.indexOf('Clinical') !== -1 |
                 #              input.topicCheckbox.indexOf('Diagnostics') !== -1",
                 condition = "TRUE",
                 checkboxGroupInput("clinicalCheckboxMetadata",
                                    "Sample Type for Clinical/Diagnostics Studies",
                                    choices = c("General Population" = "Population",
                                                "Clinical",
                                                "Mixed"),
                                    selected = "Population"
                 )
               ),
               sliderInput("yearSliderMetadata",
                           "Year of Publication",
                           min = 2015,
                           max = 2024,
                           value = c(2015, 2024),
                           step = 1
               ),
               sliderInput("nNodesSliderMetadata",
                           "Number of Nodes",
                           min = 3,
                           max = 97,
                           value = c(3, 97),
                           step = 1
               ),
               sliderInput("sampleSizeMetadata",
                           "Sample Size",
                           min = 23,
                           max = 388286,
                           value = c(23, 388286),
                           step = 1
               ),
               downloadButton("downloadTableMetadata",
                              "Download Table as RDS"
               )
             ),
             mainPanel(
               width = 8,
               fluidRow(
                 column(
                   width = 12,
                   bucket_list(
                     header = "Metadata variables",
                     group_name = "bucket_list_group",
                     orientation = "horizontal",
                     add_rank_list(
                       text = "Available variables",
                       labels = list(
                         "Year",
                         "Nodes",
                         "Edges",
                         "Sample.size",
                         "Topic"
                       ),
                       input_id = "rank_list_1"
                     ),
                     add_rank_list(
                       text = "To plot",
                       labels = NULL,
                       input_id = "rank_list_2"
                     )
                   )
                 )
               ),
               fluidRow(
                 column(6, verbatimTextOutput("bucket_check")),
               ),
               br()
             )
           )
  ),
  tabPanel("Estimates",
           sidebarLayout(
             sidebarPanel(
               width = 4,
               shinyjs::useShinyjs(),
               checkboxInput("useSelectedNetworks",
                             "Use Networks Selected in 'Individual Studies' Tab"),
               checkboxGroupInput(
                 "estimatesPlotsCheckbox",
                 "Plots",
                 choices = c("Frequentist vs. Bayesian Inclusion Probability" = "fvb_incl",
                             "Edge Estimate vs. Posterior Inclusion Probability" = "edge_est_post_incl",
                             "Frequentist Estimate vs. Bayesian Estimate" = "fvb_est"),
                 selected = c("fvb_incl",
                              "edge_est_post_incl",
                              "fvb_est")
               ),
               checkboxGroupInput("topicCheckboxEstimates",
                                  "Topics",
                                  choices = c("Clinical",
                                              "Social",
                                              "Personality",
                                              "Work- and Organizational",
                                              "Diagnostics",
                                              "Public Health",
                                              "Education",
                                              "Other"),
                                  selected = c("Clinical",
                                               "Social",
                                               "Personality",
                                               "Work- and Organizational",
                                               "Diagnostics",
                                               "Public Health",
                                               "Education",
                                               "Other")
               ),
               conditionalPanel(
                 # condition = "input.topicCheckbox.indexOf('Clinical') !== -1 |
                 #              input.topicCheckbox.indexOf('Diagnostics') !== -1",
                 condition = "TRUE",
                 checkboxGroupInput("clinicalCheckboxEstimates",
                                    "Sample Type for Clinical/Diagnostics Studies",
                                    choices = c("General Population" = "Population",
                                                "Clinical",
                                                "Mixed"),
                                    selected = "Population"
                 )
               ),
               sliderInput("yearSliderEstimates",
                           "Year of Publication",
                           min = 2015,
                           max = 2024,
                           value = c(2015, 2024),
                           step = 1
               ),
               sliderInput("nNodesSliderEstimates",
                           "Number of Nodes",
                           min = 3,
                           max = 97,
                           value = c(3, 97),
                           step = 1
               ),
               sliderInput("sampleSizeEstimates",
                           "Sample Size",
                           min = 23,
                           max = 10000,
                           value = c(23, 10000),
                           step = 1
               ),
               checkboxInput("sampleSizeOutliersEstimates",
                             "Include Sample Size Outliers (n > 10k)"),
               downloadButton("downloadTableEstimates",
                              "Download Table as RDS"
               )
             ),
             mainPanel(
               width = 8,
               fluidRow(
                 column(6, plotOutput("netDensityDensity")),
                 column(6, plotOutput("netEdgeDensity")),
               ),
               fluidRow(
                 column(6, plotOutput("freqVsBayesInclBar")),
                 column(6, plotOutput("edgeEstVsPostInclProb"))
               ),
               fluidRow(
                 column(6, plotOutput("freqEstVsBayesEst")),
               )
             )
           )
  )
)
