library(shiny)
library(shinyjs)
library(DT)
library(bslib)
library(sortable)


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
        Networks are form various subdisciplines from the social and behavioral sciences. Network nodes
        represent a psychometric measurement variable and edges were estimated
        on data and represent partial associations.
        From each network, we share the network graphs and all aggregated statistics."),
      p("There are three tabs: Individual Studies, Estimates, and Documentation. Individual Studies contains
        a table with the meta-data and graphs of all networks.
          Estimates contains several figures providing an overview of the aggregated information across all studies.
          Documentation contains more detailed information about the content of this app."),
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
      of the Bayesian approach. This approach allows quantification of the uncertainty of the networks
      and answer questions whether edges are present, absent or whether there is insufficient evidence to conclude an edges presence or absence.
      Despite the benefits, the Bayesian approach has found limited application in the network field until now and as such the
      (un)certainty of previous networks findings is unknown.
      Therefore, in this project, we assessed the statistical evidence supporting several published psychological networks by analysing
      the original networks also using the Bayesian approach.
    "),
      p("The resource was compiled by Karoline Huth, Sara Keetelaar, Jonas Haslbeck, and Maarten Marsman. For more
        information on this topic and the group, visit the ",
        a("Bayesian Graphical Modeling website.", href = "https://bayesiangraphicalmodeling.com/re-analysis/", target = "_blank")
      ),
      br(),  # Add a line break to create space
      h2("Want to contribute (a network of yours)?"),
      p("We appreciate any input that you might have, which could either be a feature request for this app or a mistake or issue that you encountered. To contribute your own network, please do not hesitate to reach out to us using the contact email below."),
      br(),  # Add a line break to create space
      h2("Citation"),
      p("Please cite the application and the materials obtained from it accordingly. Furthermore, in case specific datasets are being used,
        please also cite the original article. The original article citation can be found in the network-specific .rds file."),
      div(style = "border: 1px solid grey; background-color: #f0f0f0; padding: 10px; margin-top: 10px;",
          "Huth, K. B. S., Haslbeck, J. M. B., Keetelaar, S., van Holst, R. J., & Marsman, M. (2024). Statistical Evidence in Psychological Networks:
          A Bayesian Analysis of 294 Networks from 126 Studies. Submitted Manuscript. Retrieved from osf.io/n8r9g.",
          br()#,
          # actionButton("copyCitation", "Copy Citation", icon = icon("copy"))
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
                                    selected = c("Population",
                                                 "Clinical",
                                                 "Mixed"),
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
                                    selected = c("Population", "Clinical",
                                                 "Mixed")
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
      # Add custom CSS for styling
      tags$head(
        tags$style(HTML("
      .list-element {
        background-color: #e6efef; /* Light blue background */
        padding: 2px 4px; /* Padding around the word */
        border-radius: 4px; /* Rounded corners */
        color: #6da1a4; /* Highlight color for list elements */
        font-weight: bold; /* Bold the list elements */
      }
      .exclplot {
        background-color: #f9d183; /* Light yellow background */
        padding: 2px 4px; /* Padding around the word */
        border-radius: 4px; /* Rounded corners */
        color: #eeb004; /* Highlight color for list elements */
        font-weight: bold; /* Bold the list elements */
      }
      .inclplot {
        background-color: #86a2b9; /* Light blue background */
        padding: 2px 4px; /* Padding around the word */
        border-radius: 4px; /* Rounded corners */
        color: #36648b; /* Highlight color for list elements */
        font-weight: bold; /* Bold the list elements */
      }
      .description {
        margin-left: 10px; /* Add spacing for better readability */
        font-size: 15px; /* Slightly smaller font for descriptions */
        color: #333333; /* Dark grey for contrast */
      }
      .main-panel {
        padding: 20px; /* Add padding around the main panel */
        background-color: #f9f9f9; /* Light background for better contrast */
        border-radius: 10px; /* Rounded corners */
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); /* Subtle shadow for depth */
      }
    "), tags$script(src = "path_to_local_mathjax.js")
        )
      ),
      useShinyjs(),  # Initialize shinyjs
      # titlePanel("Documentation"),
      br(),  # Add a line break to create space
      br(),  # Add a line break to create space
      h2("Individual Studies"),
      p("This tab shows the meta data from all analysed networks and allows users to visualize the network graphs. "),
      p("The main panel is a", span("table", class = "list-element"), "displaying various meta-data information about the networks. Each row in the table represents one unique network, where several unique networks might stem from the same paper.
      The table includes information such as: citation,
        subfield, topic of the estimated network, questionnaires, sample size and number of nodes."),
      p("Users have three different ways to ", span("select a subset", class = "list-element"), " of all networks to be shown in the table. First, users can use the side panel on the left and filter networks based on the subdimensions.
      For example, users could only select the checkbox next to `Public Health` to select networks classified to that topic.
        Second, users can filter each column in the table. For example, to filter depression networks, users can type `depression` in the top row of the `Subtopic` column or
        type `PHQ-9`in the column `Questionnaire` to find networks having used that exact scale. Lastly, users can also select individual networks from the table by clicking the checkbox in the second column of the table."),

      p("All network graphs can be depicted by clicking the row-specific ", span("plot", class = "list-element"), "icon."),
      tags$ul(
        tags$li("The plot on the left shows the estimated", span("parameter network,", style = "font-weight: bold;"), "where the thickness and saturation of an edge represents the partial association strength."
        ),
        tags$li("The middle plot shows the", span("inclusion edge evidence network,", style = "font-weight: bold;"), "showing edges that have some evidence for inclusion.", span("Grey", style = "color: grey;background-color: #efefef;"), "edges
        depicting slight but inconclusive (Bayes factor smaller than 3),", span("light blue", style = "color: #86a2b9; background-color: #ebeff3;"), " edges weak (Bayes factor larger than 3),
        and", span("dark blue", style = "color: #36648b;background-color: #ebeff3;"), "edges strong evidence for inclusion (Bayes factor larger than 10). "
        ),
        tags$li("The right plot shows the", span("exclusion edge evidence network,", style = "font-weight: bold;"), "showing edges that have some evidence for exclusion.", span("Grey", style = "color: grey;background-color: #efefef;"), "edges indicate
        slight but inconclusive (Bayes factor smaller than 1 but larger than 1/3),", span("light yellow", style = "color: #f9d183;background-color: #fff1da;"), "edges weak (Bayes factor smaller than 1/3), and
       ", span("dark yellow", style = "color: #eeb004;background-color: #fff1da;"), " edges strong evidence for exclusion (Bayes factor smaller than 1/10)."
        )
      ),
      # p("More detailed information about the interpretation of the graphs can be found in the manuscript."),
      p("With the ", span("download", class = "list-element"), " button, users can obtain an .rds file with network-specific results. Each
      network-specific file, contains meta-data,
        parameter estimates, parameter inclusion evidence, and general data information such
        as covariance estimates."),
      p("For a detailed information on the plot interpretation or
        of the network-specific rds files see the",
        a("manuscript and information sheets", href = "https://osf.io/x9q84", target = "_blank"),
        "on OSF."),
      br(),  # Add a line break to create space
      h2("Estimates"),
      p("This tab shows various plots on the aggregated measures of the analysed networks. "),
      p("By default, the tab always shows the", span("distribution of the density ", class = "list-element"), "of the
      networks (i.e., percentage of present edges) and the ", span("distribution of the parameter estimates", class = "list-element"),
        ". Optionally, users can choose to show the"),
      tags$ul(
        tags$li(span("Edge Categorization Percentage", style = "font-weight: bold;"), " plot, which depicts the percentage of the edges from the (sub)
                selected networks that fall into the respective edge evidence categories.
                We grouped edges into one of the evidence categories in each network according to the Bayes factor:
(1) Dark yellow indicates strong evidence for absence; BF < 0.1.
(2) Light yellow indicates weak evidence for absence; BF < 0.33.
(3) Grey indicates inconclusive evidence for either presence or absence; 0.33 < BF < 3.
(4) Light blue indicates weak evidence for presence; BF > 3.
(5) Dark blue indicates strong evidence for presence; BF > 10."
        ),
        tags$li(span("Edge Estimate and Inclusion BF", style = "font-weight: bold;"), " plot, which depicts the edge estimate (i.e., the partial association) and the respective
                Bayes factor specifying the evidence for edge inclusion.
                The x-axis represents the edge weights, the partial correlations. The y-axis represents the log of the Bayes Factor.
                Each dot represents one edge in the selected networks, which are colored according to the evidence category (see above). "),
        tags$li(span("Frequentist and Bayesian Estimate", style = "font-weight: bold;"), " scatterplot, which depicts the frequentist and Bayesian edge weights.
                The x-axis indicates the frequentist edge weights and the y-axis the corresponding Bayesian estimate.
                Each dot represents one edge in the selected networks, which are colored according to the evidence categorization.")
      ),
      p("Users have two different ways to ", span("select a subset", class = "list-element"), "of all networks to be included in generating the plots.
      First, users can use the side panel on the left and filter networks based on the intended subdimensions.
      For example, users could only select the networks published between 2020 to 2022 by adapting the slider.
        Second, users can wish to only show the networks filtered in the `Individual Studies` tab.
        To do so, users can select networks in the individual studies table with the above mentioned methods.
        Then, users tick the `Select or Deselect Filtered` checkbox in the individual studies tab and then tick the
        `Use Networks Selected in Individual Studies Tab` checkbox in the `Estimates` tab."),
      br(),  # Add a line break to create space
      # div(class = "main-panel",
      h2("Network-specific .rds files"),
      p("For each network or each collection of networks, users can download the aggregated data. In case of one network,
      users will download one .rds file with the following information, and for a set of networks users will obtain an .rds
      file which contains a list with all respective network specific elements.
        For each network, users will have the following data: the name of list element is highlighted in", span(" blue,", class = "list-element"),
        "the R-object type is indicated in parentheses, and the respective meaning of the element is added after the colon."),
      tags$ul(
        tags$li(span("p", class = "list-element"), " (integer) = Number of nodes in the network"),
        tags$li(span("n_edges", class = "list-element"), " (integer) = number of possible edges in the network"),
        tags$li(
          span("meta", class = "list-element"), " (list) = list that contains several meta-data information about the network",
          tags$ul(
            tags$li(span("networkID", class = "list-element"), " (character) = project-assigned ID for the particular network. Can be used to link to other project data files."),
            tags$li(span("paperID", class = "list-element"), " (character) = project-assigned ID for the particular paper. Can be used to link to other project data files."),
            tags$li(span("citation", class = "list-element"), " (character) = Citation for the paper that the network was published in"),
            tags$li(span("data_link", class = "list-element"), " (character) = if available, link to the public repository holding the raw data"),
            tags$li(span("model", class = "list-element"), " (character) = type of model that was fit to the data, either GGM for continuous data or Ising for binary data"),
            tags$li(span("network_subtype", class = "list-element"), " (character) = if there are several networks in one paper, it encodes how the respective network is different from the original network (first network shown in the publication); e.g., could be a `different population` to indicate a network that differs from the `original` network by being fit to a different sample or `different variables` to indicate a network that differs based on the nodes that are included in the network"),
            tags$li(span("topic", class = "list-element"), " (character) = overarching research topic of the publication. Can be one of the following: Clinical, Social, Personality, Work- and Organizational, Diagnostics, Public Health, Education, Other"),
            tags$li(span("subtopic", class = "list-element"), " (character) = More specific research topic of the article which was assigned after screening the articles. It usually is the phenomenon mentioned in the title or research question of the article. At least one subtopic but can also be more than one subtopic separated with a semi-colon. e.g., Depression, Narcissism, Rumination"),
            tags$li(span("questionnaires", class = "list-element"), " (character) = the name(s) or abbreviation(s) of the questionnaire, if an established questionnaire was used. If several established questionnaires were used, they were separated with a semi-colon"),
            tags$li(span("variable_names", class = "list-element"), " (character) = Names or abbreviations of the nodes as provided in the original publication separated with a semi-colon"),
            tags$li(span("type_of_variables", class = "list-element"), " (character) = The nature of the variables that were included in the network. Can be one of the following: continuous, binary, mixed. If the type of variables were “mixed”, they needed to have no categorical variables"),
            tags$li(span("sample_type", class = "list-element"), " (character) = The type of sample on which the network was fitted, which can be one of the following: population, clinical, and mixed. Population indicates general population samples, clinical indicates a sample with a diagnosed disorder, and mixed indicates a mix of general population and clinical samples"),
            tags$li(span("sample_size", class = "list-element"), " (integer) = the number of individuals included in the sample to which the network was fit"),
            tags$li(span("number_nodes", class = "list-element"), " (integer) = number of nodes in the network"),
            tags$li(span("variable_means", class = "list-element"), " (vector) = mean of the variables"),
            tags$li(span("variable_standard_deviations", class = "list-element"), " (vector) = standard deviation of the variables"),
            tags$li(span("covariance_matrix", class = "list-element"), " (matrix) = covariance matrix of the dataset, obtained with the R function cov(na.omit(data))"),
            tags$li(span("correlation_matrix", class = "list-element"), " (matrix) = correlation matrix of the dataset, obtained with the R function cor(na.omit(data))"),
            tags$li(span("partialcorrelation_matrix", class = "list-element"), " (matrix) = partial correlation matrix of the dataset, obtained with the R function ppcor::pcor(na.omit(data))")
          )),
        tags$li(span("modSelect_parameters", class = "list-element"), " (matrix) = parameter estimates of the network edges obtained with the frequentist modeling “ggmModSelect”. Fit obtained with bootnet::estimateNetwork(data, default = “ggmModSelect”). Bootnet version: 1.5.3"),
        tags$li(span("modSelect_n_inc", class = "list-element"), " (integer) = number of edges determined to be included when fitting the network with “ggmModSelect”"),
        tags$li(span("modSelect_n_exc", class = "list-element"), " (integer) = number of edges determined to be excluded when fitting the network with “ggmModSelect”"),
        tags$li(span("EBIC_parameters", class = "list-element"), " (matrix) = parameter estimates of the network edges obtained with the frequentist modeling “EBICglasso”. Fit obtained with bootnet::estimateNetwork(data, default = “EBICglasso”). Bootnet version: 1.5.3"),
        tags$li(span("EBIC_n_inc", class = "list-element"), " (integer) = number of edges determined to be included when fitting the network with “EBICglasso”"),
        tags$li(span("EBIC_n_exc", class = "list-element"), " (integer) = number of edges determined to be excluded when fitting the network with “EBICglasso”")
      ),
      tags$ul(
        tags$li(
          span("BGGM_parameters", class = "list-element"),
          " (matrix) = parameter estimates of the network edges obtained with the Bayesian modeling as implemented in “BGGM”.
          Fit obtained with easybgm::easybgm(data = data, type = \"continuous\",
          save = F, package = \"BGGM\",", span("prior_sd = sqrt(1/8)", style = "font-weight: bold;"), ", iter = 10000). easybgm version: 0.1.2"
        ),
        tags$li(
          span("BGGM_inc_probs", class = "list-element"),
          " (matrix) = edge-specific posterior inclusion probability obtained with “BGGM”."
        ),
        tags$li(
          span("BGGM_BF", class = "list-element"),
          " (matrix) = edge-specific inclusion Bayes factor obtained with “BGGM”."
        ),
        tags$li(
          span("BGGM_category", class = "list-element"),
          " (matrix) = edge-specific categorization of the Bayes factor obtained with “BGGM” following the criteria by Jeffreys (1961):
          excluded, weak excluded, inconclusive, weak included, and included."
        ),
        tags$li(
          span("BGGM_color", class = "list-element"),
          " (matrix) = self-assigned coloring to the edge-specific inclusion categorization used in the network edge evidence plots:
          dark yellow, light yellow, grey, light blue, dark blue."
        ),
        tags$li(
          span("BGGM2_parameters", class = "list-element"),
          "(matrix) = parameter estimates of the network edges obtained with the Bayesian modeling as implemented in “BGGM”.
          Fit obtained with easybgm::easybgm(data = data, type = \"continuous\", save = F,
          package = \"BGGM\", ", span("prior_sd = sqrt(1/24)", style = "font-weight: bold;"), ", iter = 10000). easybgm version: 0.1.2"
        ),
        tags$li(
          span("BGGM2_inc_probs", class = "list-element"),
          " (matrix) = edge-specific posterior inclusion probability obtained with “BGGM”."
        ),
        tags$li(
          span("BGGM2_BF", class = "list-element"),
          " (matrix) = edge-specific inclusion Bayes factor obtained with “BGGM”."
        ),
        tags$li(
          span("BGGM2_category", class = "list-element"),
          " (matrix) = edge-specific categorization of the Bayes factor obtained with “BGGM” following the criteria by Jeffreys (1961):
          excluded, weak excluded, inconclusive, weak included, and included."
        ),
        tags$li(
          span("BGGM2_color", class = "list-element"),
          " (matrix) = self-assigned coloring to the edge-specific inclusion categorization used in the network edge evidence plots:
          dark yellow, light yellow, grey, light blue, dark blue."
        ),
        tags$li(
          span("BGGM3_parameters", class = "list-element"),
          " (matrix) = parameter estimates of the network edges obtained with the Bayesian modeling as implemented in “BGGM”.
          Fit obtained with easybgm::easybgm(data = data, type = \"continuous\", save = F,
          package = \"BGGM\"," , span("prior_sd = sqrt(1/16)", style = "font-weight: bold;"), ", iter = 10000). easybgm version: 0.1.2"
        ),
        tags$li(
          span("BGGM3_inc_probs", class = "list-element"),
          " (matrix) = edge-specific posterior inclusion probability obtained with “BGGM”."
        ),
        tags$li(
          span("BGGM3_BF", class = "list-element"),
          " (matrix) = edge-specific inclusion Bayes factor obtained with “BGGM”."
        ),
        tags$li(
          span("BGGM3_category", class = "list-element"),
          " (matrix) = edge-specific categorization of the Bayes factor obtained with “BGGM” following the criteria by Jeffreys (1961):
          excluded, weak excluded, inconclusive, weak included, and included."
        ),
        tags$li(
          span("BGGM3_color", class = "list-element"),
          " (matrix) = self-assigned coloring to the edge-specific inclusion categorization used in the network edge evidence plots:
          dark yellow, light yellow, grey, light blue, dark blue."
        ),
      ),
      # ) # Add other elements similarly
      br(),
      h2("Questions?"),
      p("Please contact us via email."),
      actionButton("contactBtn", "Contact", icon = icon("envelope")),
      br(),  # Add a line break to create space
      h2(" ")
    )

  )
)
