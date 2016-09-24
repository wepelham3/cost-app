library(shiny)
library(magrittr)
library(readr)
library(shinyjs)

options(shiny.trace=TRUE)

# --------------------------------------------------------------------


shinyUI(
  navbarPage("Cost of Treatment Calculator",
             
             windowTitle = "Cost of Treatment Calculator",
             selected = "About the app.",
             
             # fix the navbar to always show and provide padding so it doesn't overlay the panels
             position = "fixed-top",
             tags$style(type="text/css", "body {padding-top: 70px;}"),
             
             shinyjs::useShinyjs(),
             
             theme = "flatly.css",
             tags$head(tags$style('nav .container:first-child {margin-left:10px; width: 100%;}')),
             tags$head(tags$style(HTML("h1 {color: #e6b800;}"))),
             
             tabPanel(title = "About the app.", "spiel about the app"),
             tabPanel(title = "Watch a 5-minute video to get started.", "youtube embed"),
             tabPanel(title = "Use the calculator.", source("use-the-calculator.R")),
             tabPanel(title = "Look up prices and wages.", source("look-up-prices.R")),
             tabPanel(title = "Contact us.", "github page, email, etc.")
  
             )
  )
