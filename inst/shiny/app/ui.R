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
  theme = bslib::bs_theme(bootswatch = "minty"),
  "ReBayesed",
  tabPanel(
    "About",
    fluidPage(
      tags$head(
        tags$style(HTML("table {table-layout: fixed;")), # I don't remember how this works, but keeps DTs nice, no delete
        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # Remove minor ticks from sliders
        tags$style(HTML("
        table.dataTable th:nth-child(1),
        table.dataTable td:nth-child(1) {
          display: none;
        }
        ")) # Remove rownames from DTs
      ),
      useShinyjs(),  # Initialize shinyjs
      titlePanel("ReBayesed: Bayesian Analysis of cross-sectional psychological networks"),
      br(),  # Add a line break to create space
      br(),  # Add a line break to create space
      h2("What is ReBayesed?"),
      p("This app encompassess a large resource of psychological networks analysed with the Bayesian approach.
        At this moment, it consists of 294 networks from 126 studies.
        Networks are form various subdisciplines from the social and behavioral sciences and consist of nodes
        representing a psychometric measurement variable and edges being estimated
        on data and representing partial associations.
        From each network, we share the re-analysed network fits."),
        p("There are three tabs: Individual Studies, Estimates, and Documentation. Individual Studies contains
        a table that entails the meta-data of all networks from this resource,
          where researchers can take a look at the particular network and its statistical support for the edges
          and download the aggregated network information.
          Estimates contains several figures providing an overview of the aggregated information across all studies.
          Documentation contains more detailed information about the content of the information in this app."),
      br(),  # Add a line break to create space
      h2("Why do we need ReBayesed?"),
      p("Network analysis has become popular in the social sciences,
      especially in clinical psychology.
        Along with the enthusiasm for networks, there has been growing concern about the
        stability of their results.
        In interpreting results and accumulating our understanding across studies,
        we need to know whether the estimated networks have sufficient statistical support from the data.
        If there is little statistical support for the network, one should not report its results,
        such as edge presence or absence, with confidence but rather acknowledge its uncertainty."),
      p("The common methods used to analyse networks do not allow researchers to properly determine the uncertainty
      underlying the results. To adequately assess the statistical evidence underlying networks, one should make use
      of the Bayesian approach. This approach allows quantification of the uncertainty of the networks (Huth et al., 2023)
      and answer questions whether edges are present, absent or whether there is unsifficient evidence to conclude an edges presence or absence.
      Despite the benefits, the Bayesian approach has found limited application in the network field until now and as such the
      (un)certainty in previous networks findings is unknown.
      Therefore, in this project, we assessed the statistical evidence supporting several published psychological networks by analysing
      the original networks also using the Bayesian approach.
    "),
      p("The resource was compiled by Karoline Huth, Sara Keetelaar, Jonas Haslbeck, and Maarten Marsman. For more
        information on this topic and the group, visit the ",
        a("Bayesian Graphical Modeling website.", href = "https://bayesiangraphicalmodeling.com/re-analysis/", target = "_blank")
      ),
      br(),  # Add a line break to create space
      h2("Want to contribute a network of yours?"),
      p("ReBayesed is aimed at being a living resource with networks being added on a regular basis.
        To contribute your own network, please do not hesitate to reach out to us using the contact email below."),
      br(),  # Add a line break to create space
      h2("Citation"),
      p("Please cite the application and the materials obtained from it accordingly. Furthermore, in case specific datasets are being used,
        please also cite the original article. The original article citation can be found in the network-specific rds file."),
      div(style = "border: 1px solid grey; background-color: #f0f0f0; padding: 10px; margin-top: 10px;",
          "Huth, K. B. S., Haslbeck, J. M. B., Keetelaar, S., van Holst, R. J., & Marsman, M. (2024). Statistical Evidence in Psychological Networks:
          A Bayesian Analysis of 294 Networks from 126 Studies. Retrieved from osf.io/n8r9g",
          br(),
          actionButton("copyCitation", "Copy Citation", icon = icon("copy"))
      ),
      br(),
      h2("Questions or Comments?"),
      p("Please contact us via email."),
      actionButton("contactBtn", "Contact", icon = icon("envelope")),
      br(),  # Add a line break to create space
      h2(" ")
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
                             "Select/Deselect Filtered"), # make "All" if all checkboxes above TRUE
               br(),
               downloadButton("downloadIndStudiesTable",
                              "Download Selected Network Results as RDS")
             ),
             mainPanel(
               width = 9,
               DTOutput("indStudiesTable"),
               br()
             )
           )
  ),
  # tabPanel("Meta Data",
  #          sidebarLayout(
  #            sidebarPanel(
  #              width = 4,
  #              checkboxGroupInput("topicCheckboxMetadata",
  #                                 "Topics",
  #                                 choices = c("Clinical",
  #                                             "Social",
  #                                             "Personality",
  #                                             "Work- and Organizational",
  #                                             "Diagnostics",
  #                                             "Public Health",
  #                                             "Education",
  #                                             "Other"),
  #                                 selected = c("Clinical",
  #                                              "Social",
  #                                              "Personality",
  #                                              "Work- and Organizational",
  #                                              "Diagnostics",
  #                                              "Public Health",
  #                                              "Education",
  #                                              "Other")
  #              ),
  #              conditionalPanel(
  #                # condition = "input.topicCheckbox.indexOf('Clinical') !== -1 |
  #                #              input.topicCheckbox.indexOf('Diagnostics') !== -1",
  #                condition = "TRUE",
  #                checkboxGroupInput("clinicalCheckboxMetadata",
  #                                   "Sample Type for Clinical/Diagnostics Studies",
  #                                   choices = c("General Population" = "Population",
  #                                               "Clinical",
  #                                               "Mixed"),
  #                                   selected = "Population"
  #                )
  #              ),
  #              sliderInput("yearSliderMetadata",
  #                          "Year of Publication",
  #                          min = 2015,
  #                          max = 2024,
  #                          value = c(2015, 2024),
  #                          step = 1
  #              ),
  #              sliderInput("nNodesSliderMetadata",
  #                          "Number of Nodes",
  #                          min = 3,
  #                          max = 97,
  #                          value = c(3, 97),
  #                          step = 1
  #              ),
  #              sliderInput("sampleSizeMetadata",
  #                          "Sample Size",
  #                          min = 23,
  #                          max = 388286,
  #                          value = c(23, 388286),
  #                          step = 1
  #              ),
  #              downloadButton("downloadTableMetadata",
  #                             "Download Table as RDS"
  #              )
  #            ),
  #            mainPanel(
  #              width = 8,
  #              fluidRow(
  #                column(
  #                  width = 12,
  #                  bucket_list(
  #                    header = "Metadata variables",
  #                    group_name = "bucket_list_group",
  #                    orientation = "horizontal",
  #                    add_rank_list(
  #                      text = "Available variables",
  #                      labels = list(
  #                        "Year",
  #                        "Nodes",
  #                        "Edges",
  #                        "Sample.size",
  #                        "Topic"
  #                      ),
  #                      input_id = "rank_list_1"
  #                    ),
  #                    add_rank_list(
  #                      text = "To plot",
  #                      labels = NULL,
  #                      input_id = "rank_list_2"
  #                    )
  #                  )
  #                )
  #              ),
  #              fluidRow(
  #                column(6, verbatimTextOutput("bucket_check")),
  #              ),
  #              br()
  #            )
  #          )
  # ),
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
                 choices = c("Edge Categorization Percentage" = "fvb_incl",
                             "Edge Estimate and Inclusion BF" = "edge_est_post_incl",
                             "Frequentist and Bayesian Estimate" = "fvb_est"),
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
                           step = 1,
                           sep = ""
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
                           max = 5000,
                           value = c(23, 5000),
                           step = 1
               ),
               checkboxInput("sampleSizeOutliersEstimates",
                             "Include Sample Size Outliers (n > 5k)"),
               # downloadButton("downloadTableEstimates",
               #                "Download Table as RDS"
               # )
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
                 column(6, plotOutput("plotsLegend"))
               )
             )
           )
  ),
  tabPanel(
    "Documentation",
    fluidPage(
      tags$head(
        tags$style(HTML("table {table-layout: fixed;")), # I don't remember how this works, but keeps DTs nice, no delete
        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # Remove minor ticks from sliders
        tags$style(HTML("
        table.dataTable th:nth-child(1),
        table.dataTable td:nth-child(1) {
          display: none;
        }
        ")) # Remove rownames from DTs
      ),
      useShinyjs(),  # Initialize shinyjs
      titlePanel("Documentation"),
      br(),  # Add a line break to create space
      br(),  # Add a line break to create space
      h2("Individual Studies"),
      p("This tab includes an overview of all obtained networks from the various papers.
      Several networks might stem from one paper.
        The table displays various meta-data information about the networks such as citation,
        subfield, topic of the estimated network, questionnaires, sample size and number of nodes.
        Users can select the networks to be shown in the table with the panel on the left.
        By clicking on the panel, a window
        opens which shows three network plots. The plot on the left shows the estimated edge
        strength network, where the thickness and
        saturation of an edge represents the partial association strength. The two right plots
        show the edge evidence categorization.
        The middle plot shows all edges that have some evidence for inclusion with grey edges
        depicting slight (Bayes factor smaller than 3), light blue edges weak (Bayes factor larger than 3),
        and dark blue edges strong evidence for inclusion (Bayes factor larger than 10). The far right plot shows all edges
        with some evidence for edge exclusion. Grey edges indicate
        slight but less than weak (Bayes factor smaller than 1 but larger than 1/3), light red edges weak (Bayes factor smaller than 1/3), and
        dark red edges strong evidence for exclusion (Bayes factor smaller than 1/10)."),
      p("With the download button, users can obtain a list of network-specific results. Each
      network-specific list, contains meta-data,
        parameter estimates, parameter inclusion evidence, and general data information such
        as covariance estimates. For a detailed overview
        of the network-specific information see the",
        a("background information", href = "https://osf.io/x9q84", target = "_blank"),
        "on OSF."),
      p("Users can filter the networks using the left sidepanel or the column-specific search function. "),
      br(),  # Add a line break to create space
      h2("Estimates"),
      p("This tab shows the distribution of the parameter estimates and the density of the
      network (i.e., percentage of present edges).
        Optionally, users can choose to show the (1) Bayesian parameter estimates against the
        edge inclusion evidence, (2) frequentist against
        Bayesian parameter estimates, and (3) the percentage of (un)certainty in the networks.
        The subset of networks on which the plots are
        generated can be selected with the sidebar. Users can additionally select a subset of networks from the Individual Studies tab. "),
      br(),  # Add a line break to create space
      br(),
      h2("Questions?"),
      p("Please contact us via email."),
      actionButton("contactBtn", "Contact", icon = icon("envelope")),
      br(),  # Add a line break to create space
      h2(" ")
    )

  )
)
