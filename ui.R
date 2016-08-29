library(shiny)
library(magrittr)
library(readr)

df.meds = read_csv("data-meds.csv", col_names = TRUE)
df.comps = read_csv("data-comps.csv", col_names = TRUE)

# --------------------------------------------------------------------


shinyUI(fluidPage(
  
  theme = "flatly.css",

  # Application title
  titlePanel(tags$h1("Cost of Treatment Calculator"), windowTitle = "Cost of Treatment Calculator"),
  tags$hr(),
  
  sidebarLayout(sidebarPanel(
    # textInput(inputId = "name", label = "Name of component:", placeholder = "e.g., parent training"),
    # selectInput(inputId = "person", label = "Person:",
    #             choices = c("Psychiatrist" = 1, "Mental Health Counselor" = 2, "Teacher" = 3, "Parent" = 4)),
    # selectInput(inputId = "format", label = "Format:",
    #             choices = c("Individual", "Group")),
    # numericInput(inputId = "duration", label = "Duration (in minutes):", value = 0, min = 0, step = 5),
    # numericInput(inputId = "frequency", label = "Frequency (per year):", value = 0, min = 0),
    
    tabsetPanel(
      
      tabPanel(
        title = "Individual Treatment",
        selectInput(inputId = "professional", label = "Professional:", choices = c("Psychiatrist", "Registered Nurse")),
        sliderInput(inputId = "frequency_in", label = "Frequency per year:", min = 1, max = 10, value = 1, ticks = TRUE, width = 500),
        textInput(inputId = "duration_indiv", label = "Duration (in minutes):", value = "0"),
        actionButton(inputId = "add_ind", tags$a(href= NULL, class = "btn btn-primary", "Add protocol component"))
      
      ),
      tabPanel(
        title = "Group Treatment",
        selectInput(inputId = "group_leader", label = "Group leader:", choices = c("Psychiatrist", "Mental Health Counselor")),
        sliderInput(inputId = "frequency_gr", label = "Frequency per year:", min = 1, max = 10, value = 1, ticks = TRUE, width = 500),
        textInput(inputId = "duration_group", label = "Duration (in minutes):", value = "0"),
        actionButton(inputId = "add_gr", tags$a(href= NULL, class = "btn btn-primary", "Add protocol component"))
      
      ),
      tabPanel(
        title = "Medication",
    selectInput(inputId = "med", label = "Medication:", choices = df.meds$name),
    selectInput(inputId = "sched.times", label = "Pills taken per day:",
                choices = c(1, 2, 3)),
    selectInput(inputId = "sched.weekly", label = "Weekly schedule:",
                choices = c("Everyday", "Weekdays only", "Weekends only")),
    selectInput(inputId = "sched.yearly", label = "Yearly schedule:",
                choices = c("Year-round", "School year only", "Summer only")),
    actionButton(inputId = "add_med", tags$a(href= NULL, class = "btn btn-primary", "Add protocol component"))
      )
    )

  ),
mainPanel(
  tags$body(
    tags$style("body {background-color: #badede; }"),
  tags$h4(textOutput("list.of.components")),
  # textOutput("total.cost.explicit"),
  # textOutput("total.cost.implicit"),
  # textOutput("total.cost.combined"),
  tags$h5(textOutput("test")),
  tags$h5(textOutput("cost.meds")),
  tags$h5(textOutput("indiv_treatment")),
  tags$h5(textOutput("group_treatment"))
))

)))
