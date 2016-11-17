library(shiny)
library(magrittr)
library(readr)
library(shinyjs)

options(shiny.trace=TRUE)

# --------------------------------------------------------------------


shinyUI(fluidPage(
  
  tags$style(HTML(" body {
                    font-family: 'Lato', sans-serif;
                          }")),
  
  list(tags$head(HTML('<link rel="icon", href="ccf.png", 
                                   type="image/png" />'))),

      titlePanel(
        title="", windowTitle = "Cost of Treatment Calculator"
      ),

  
  navbarPage(id="main.nav.bar"
                 , title = div(img(src="ccf.png", style = "height: 50%; width: 50%; padding: 0px 30px 0px 5px;", align = "top")
                 , "Cost of Treatment Calculator")
                 , selected = "About the app"
                 , fluid = TRUE,
             
             # fix the navbar to always show and provide padding so it doesn't overlay the panels
             position = "fixed-top",
             tags$style(type="text/css", "body {padding-top: 150px;}", ".navbar {background-color: #b38600;}"),
             
             shinyjs::useShinyjs(),
             
             theme = "flatlynew.css",
             tags$head(tags$style('nav .container:first-child {margin-left:10px; width: 100%; }')),
             tags$head(tags$style(HTML("h1 {color: #e6b800;}"))),
             
             tabPanel(title = "About the app",
                      source("ui-navbar-tabs/about-the-app.R")$value),
             
             tabPanel(title = "Watch a 5-minute video to get started",
                      source("ui-navbar-tabs/watch-a-video.R")$value),
             
             tabPanel(title = "Use the calculator",
                      source("ui-navbar-tabs/use-the-calculator.R")$value),
             
             tabPanel(title = "Compare protocols",
                      source("ui-navbar-tabs/compare-protocols.R")$value),
             
             tabPanel(title = "Look up prices and wages",
                      source("ui-navbar-tabs/look-up-prices.R")$value),
             
             tabPanel(title = "Contact us",
                      source("ui-navbar-tabs/contact-us.R")$value)
  
             )
  )
)
