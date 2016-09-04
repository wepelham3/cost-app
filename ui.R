library(shiny)
library(magrittr)
library(readr)
library(shinyjs)

df.meds = read_csv("data-meds.csv", col_names = TRUE)
df.comps = read_csv("data-comps.csv", col_names = TRUE)
options(shiny.trace=TRUE)

# --------------------------------------------------------------------


shinyUI(fluidPage(
  
  #use shiny js to disable the ID fields
  shinyjs::useShinyjs(),
  
  theme = "flatly.css",

  # Application title
  titlePanel(tags$h1("Cost of Treatment Calculator"), windowTitle = "Cost of Treatment Calculator"),
  tags$hr(),
  
  sidebarLayout(sidebarPanel(
    
    h3(em("Click one of the tabs below to add a component to the treatment protocol...")),
  
    tabsetPanel( type ="pills", 
                 
    
    tabPanel(
        title = "Add Individual Treatment",
        shinyjs::disabled(textInput("id.ind", "Id", "0")),
        textInput(inputId = "label.ind", label = "Label:"),
        sliderInput(inputId = "frequency.ind", label = "Frequency per year:", min = 1, max = 10, value = 1, ticks = TRUE, width = 500),
        textInput(inputId = "duration.ind", label = "Duration per session (in minutes):", value = "0"),
        selectInput(inputId ="num.persons.ind", "Number of Persons", choices = seq(1,5,1)),
        uiOutput("persons.ind"),
        uiOutput("persons.comm.ind"),
        actionButton(inputId = "add.ind", tags$a(href= NULL, class = "btn btn-primary", "Add Component")),
        actionButton(inputId = "new.ind", tags$a(href= NULL, class = "btn btn-primary", "New")),
        actionButton(inputId = "del.ind", tags$a(href= NULL, class = "btn btn-primary", "Delete"))
      
      ),
      tabPanel(
        title = "Add Group Treatment",
        textInput(inputId = "label.gr", label = "Label:"),
        sliderInput(inputId = "frequency.gr", label = "Frequency per year:", min = 1, max = 10, value = 1, ticks = TRUE, width = 500),
        textInput(inputId = "duration.gr", label = "Duration (in minutes):", value = "0"),
        selectInput(inputId = "person1.gr", label = "Person 1:", choices = df.comps$person),
        radioButtons(
          "p1.gr.yn",
          label = tags$h5("Is this person commuting to the session?"), choices = c("Yes", "No"), selected = NULL
        ),
        selectInput(inputId = "person2.gr", label = "Person 2:", choices = df.comps$person),
        radioButtons(
          "p2.gr.yn",
          label = tags$h5("Is this person commuting to the session?"), choices = c("Yes", "No"), selected = NULL
        ),
        actionButton(inputId = "add.gr", tags$a(href= NULL, class = "btn btn-primary", "Add Component")),
        actionButton(inputId = "new.gr", tags$a(href= NULL, class = "btn btn-primary", "New")),
        actionButton(inputId = "del.gr", tags$a(href= NULL, class = "btn btn-primary", "Delete"))
      
      ),
      tabPanel(
        title = "Add Medication",
        selectInput(inputId = "med", label = "Medication:", choices = df.meds$name),
        selectInput(inputId = "sched.times", label = "Pills taken per day:",
                    choices = c(1, 2, 3)),
        selectInput(inputId = "sched.weekly", label = "Weekly schedule:",
                   choices = c("Everyday", "Weekdays only", "Weekends only")),
        selectInput(inputId = "sched.yearly", label = "Yearly schedule:",
                   choices = c("Year-round", "School year only", "Summer only")),
        actionButton(inputId = "add.med", tags$a(href= NULL, class = "btn btn-primary", "Add Medication")),
        actionButton(inputId = "new.med", tags$a(href= NULL, class = "btn btn-primary", "New")),
        actionButton(inputId = "del.med", tags$a(href= NULL, class = "btn btn-primary", "Delete"))
        
        
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
  tags$h5(textOutput("indiv.treatment")),
  tags$h5(htmlOutput("people.ind")),
  
  #data table
  
  # simple format
  tableOutput("table.persons.ind"),
  
  DT::dataTableOutput("responsesIndivTreatment", width = 400), 
  
  tags$h5(textOutput("cost.indiv.treatments")),
  
  tags$h5(textOutput("group.treatment")),
  tags$h5(htmlOutput("people.gr")),
  tags$h5(textOutput("cost.group.treatments")),
  
  
  tags$h5(textOutput("med")),
  tags$h5(textOutput("cost.meds"))
  
  # tags$h5(textOutput("people_ind")),
))

)))
