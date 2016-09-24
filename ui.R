library(shiny)
library(magrittr)
library(readr)
library(shinyjs)

options(shiny.trace=TRUE)

# --------------------------------------------------------------------


shinyUI(fluidPage(

  #use shiny js 
  shinyjs::useShinyjs(),
  
  theme = "flatly.css",
  
  tags$head(
    tags$style(HTML("
      
      h1 {
        color: #e6b800;
      }

    "))
  ),

  # Application title
  titlePanel(tags$h1("Cost of Treatment Calculator"), windowTitle = "Cost of Treatment Calculator"),
  
  tags$hr(),
  
  tabsetPanel(tabPanel(title = "Use the calculator.", source("use-the-calculator.R")),
              tabPanel(title = "Look up prices.", source("look-up-prices.R")))
  )
  )
