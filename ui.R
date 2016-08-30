library(shiny)
library(magrittr)
library(readr)

df.meds = read_csv("data-meds.csv", col_names = TRUE)
df.comps = read_csv("data-comps.csv", col_names = TRUE)

# --------------------------------------------------------------------


shinyUI(fluidPage(
theme = "flatly.css", #Add the css theme
  # Application title
  titlePanel("Cost of Treatment Calculator"),
  tags$hr(), #Horizontal line to separate the page header
  
  sidebarLayout(sidebarPanel(
    # textInput(inputId = "name", label = "Name of component:", placeholder = "e.g., parent training"),
    # selectInput(inputId = "person", label = "Person:",
    #             choices = c("Psychiatrist" = 1, "Mental Health Counselor" = 2, "Teacher" = 3, "Parent" = 4)),
    # selectInput(inputId = "format", label = "Format:",
    #             choices = c("Individual", "Group")),
    # numericInput(inputId = "duration", label = "Duration (in minutes):", value = 0, min = 0, step = 5),
    # numericInput(inputId = "frequency", label = "Frequency (per year):", value = 0, min = 0),
    selectInput(inputId = "med", label = "Medication:", choices = df.meds$name),
    selectInput(inputId = "sched.times", label = "Pills taken per day:",
                choices = c(1, 2, 3)),
    selectInput(inputId = "sched.weekly", label = "Weekly schedule:",
                choices = c("Everyday", "Weekdays only", "Weekends only")),
    selectInput(inputId = "sched.yearly", label = "Yearly schedule:",
                choices = c("Year-round", "School year only", "Summer only")),
    actionButton(inputId = "add", label = "Add protocol component.")
  ),
mainPanel(
  tags$body(
    tags$style("body {background-color: #badede; }"), #Set background color for the main panel
  tags$h4(textOutput("list.of.components")),
  # textOutput("total.cost.explicit"),
  # textOutput("total.cost.implicit"),
  # textOutput("total.cost.combined"),
  textOutput("test"),
  tags$h5(textOutput("cost.meds"))
))

)))
