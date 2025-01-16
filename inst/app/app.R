# Load the required packages
# https://stackoverflow.com/questions/37830819/developing-shiny-app-as-a-package-and-deploying-it-to-shiny-server
library(shiny)
library(DT)
library(tidyverse)
library(qgraph)
library(bslib)

##### ABOUT PAGE END ######



# Source the UI and server components
source(system.file("app/ui.R", package = "ReBayesed"))
source(system.file("app/server.R", package = "ReBayesed"))


# Run the application
ReBayesed <- function(...) {
  shinyApp(ui = ui, server = server, ...)
}

# pkgload::load_all(".")
# ReBayesed()
launch_app()
