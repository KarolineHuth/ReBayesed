# Load the required packages
library(shiny)
library(DT)
library(tidyverse)
library(qgraph)
library(bslib)

# Source the UI and server components
source(system.file("shiny/app/ui.R", package = "NRP.web"))
source(system.file("shiny/app/server.R", package = "NRP.web"))

# Run the application
shinyApp(ui = ui, server = server)
