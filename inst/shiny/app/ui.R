library(shiny)
library(DT)
library(bslib)
library(sortable)

labels <- list(
  "one",
  "two",
  "three",
  htmltools::tags$div(
    htmltools::em("Complex"), " html tag without a name"
  ),
  "five" = htmltools::tags$div(
    htmltools::em("Complex"), " html tag with name: 'five'"
  )
)

rank_list_swap <- rank_list(
  text = "Notice that dragging causes items to swap",
  labels = labels,
  input_id = "rank_list_swap",
  options = sortable_options(swap = TRUE)
)

# Define UI
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "yeti"),
  "App Title",
  tabPanel("About",
           fluidPage(
             # tags$head(
             #   tags$script(HTML(
             #     "Shiny.addCustomMessageHandler('openInBrowser', function(message) {
             #      window.open(message.url, '_blank');
             #    });"
             #   ))
             # ),
             h3("About"),
             p("Cool logo"),
             p("Short explainer text"),
             p("Citation"),
             p("Disclaimer about app still being in development"),
             p("Contact button")
           )
  ),
  tabPanel("Individual Studies",
           sidebarLayout(
             sidebarPanel(
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
               downloadButton("downloadTable",
                              "Download Table as RDS")
             ),
             mainPanel(
               DTOutput("indStudiesTable"),
               br()
             )
           )
  ),
  tabPanel("Meta Data",
           sidebarLayout(
             sidebarPanel(
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
                           max = 388286,
                           value = c(23, 388286),
                           step = 1
               ),
               downloadButton("downloadTableEstimates",
                              "Download Table as RDS"
               )
             ),
             mainPanel(
               fluidRow(
                 column(6, plotOutput("freqVsBayesInclBar")),
                 column(6, plotOutput("edgeEstVsPostInclProb"))
               ),
               fluidRow(
                 column(6, plotOutput("freqEstVsBayesEst")),
                 column(6, plotOutput("netDensity"))
               )
             )
           )
  )
)
