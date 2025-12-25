# Main application file for the Shiny app

# Load required libraries
library(shiny)
library(shinyjs)
library(DT)
library(tidyverse)
library(stringr)
library(ggplot2)

# Source global variables
source("R/global.R", local = FALSE)

# Source all UI components
source("R/ui/ui_main.R", local = FALSE)

# Source all server components
source("R/server/server_main.R", local = FALSE)

# Define UI
ui <- create_main_ui(free_text_questions)

# Define server
server <- create_main_server(free_text_questions)

# Create the Shiny app
shinyApp(ui = ui, server = server, options = list(port = 7008))
