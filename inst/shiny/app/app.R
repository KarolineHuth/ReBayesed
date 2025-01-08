# Load the required packages
library(shiny)
library(DT)
library(tidyverse)
library(qgraph)
library(bslib)

##### ABOUT PAGE END ######



# Source the UI and server components
source(system.file("shiny/app/ui.R", package = "ReBayesed"))
source(system.file("shiny/app/server.R", package = "ReBayesed"))

# Run the application
shinyApp(ui = ui, server = server)
