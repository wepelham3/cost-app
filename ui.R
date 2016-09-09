library(shiny)
library(magrittr)
library(readr)
library(shinyjs)

df.meds = read_csv("data-meds.csv", col_names = TRUE)
df.comps = read_csv("data-comps.csv", col_names = TRUE)
options(shiny.trace=TRUE)

# --------------------------------------------------------------------


shinyUI(fluidPage(
  
  #use shiny js 
  shinyjs::useShinyjs(),
  
  theme = "flatly.css",

  # Application title
  titlePanel(tags$h1("Cost of Treatment Calculator"), windowTitle = "Cost of Treatment Calculator"),
  tags$hr(),
  
  sidebarLayout(sidebarPanel(
  
    tabsetPanel( type ="pills", 
                 
    
    tabPanel(
        title = "Individual Treatment",
        shinyjs::hidden(textInput("id.ind", "Id", "0")),
        #shinyjs::disabled(textInput("id.ind", "Id", "0")),
        textInput(inputId = "label.ind", label = "Label:"),
        sliderInput(inputId = "frequency.ind", label = "Frequency per year:", min = 1, max = 10, value = 1, ticks = TRUE, width = 500),
        textInput(inputId = "duration.ind", label = "Duration per session (in minutes):", value = "0"),
      # it was before  
      #  selectInput(inputId ="num.persons.ind", "Number of Persons", choices = seq(1,5,1)),
      #  uiOutput("persons.ind"),
      #  uiOutput("persons.comm.ind"),
      
         div(id = "p1.ind",
           selectInput(inputId = "person1.ind", label = "Person 1", choices = df.comps$person),
           textInput(inputId = "p1.comm.ind", label = "Person 1 Commute Time (one-way in min)", value = "0"),
           a(id = "add.person2.ind", "Show/Hide Person 2"),
           tags$br(),
           tags$br()
          ),
        
       # a(id = "add.person1.ind", "Add Person"),
        
        #actionButton(inputId = "add.person.ind", tags$a(href= NULL, class = "btn btn-primary", "Add Person")),
        
        
        shinyjs::hidden(
         div(id = "p2.ind",
          selectInput(inputId = paste0("person", 2, ".ind"), label = paste0("Person ", 2), choices = df.comps$person),
          textInput(inputId = paste0("p", 2, ".comm.ind"), label = paste0("Person ", 2, " Commute, one-way (min)" ), value = "0"),
          a(id = "add.person3.ind", "Show/Hide Person 3"),
          tags$br(),
          tags$br()
         )
        ),
        
        shinyjs::hidden(
          div(id = "p3.ind",
              selectInput(inputId = paste0("person", 3, ".ind"), label = paste0("Person ", 3), choices = df.comps$person),
              textInput(inputId = paste0("p", 3, ".comm.ind"), label = paste0("Person ", 3, " Commute, one-way (min)" ), value = "0"),
              a(id = "add.person4.ind", "Show/Hide Person 4"),
              tags$br(),
              tags$br()
          )
        ),
        
        shinyjs::hidden(
          div(id = "p4.ind",
              selectInput(inputId = paste0("person", 4, ".ind"), label = paste0("Person ", 4), choices = df.comps$person),
              textInput(inputId = paste0("p", 4, ".comm.ind"), label = paste0("Person ", 4, " Commute, one-way (min)" ), value = "0"),
              a(id = "add.person5.ind", "Show/Hide Person 5"),
              tags$br(),
              tags$br()
          )
        ),
        
        shinyjs::hidden(
          div(id = "p5.ind",
              selectInput(inputId = paste0("person", 5, ".ind"), label = paste0("Person ", 5), choices = df.comps$person),
              textInput(inputId = paste0("p", 5, ".comm.ind"), label = paste0("Person ", 5, " Commute, one-way (min)" ), value = "0"),
            #  a(id = "add.person4.ind", "Add Person"),
            tags$br(),
            tags$br()
          )
        ),
        
        actionButton(inputId = "submit.ind", tags$a(href= NULL, class = "btn btn-primary", "Submit")),
        actionButton(inputId = "reset.ind", tags$a(href= NULL, class = "btn btn-primary", "Reset")),
        actionButton(inputId = "delete.ind", tags$a(href= NULL, class = "btn btn-primary", "Delete"))
      
      ),
      tabPanel(
        title = "Group Treatment",
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
        title = "Medication",
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
  # tableOutput("table.persons.ind"),
  # dataTableOutput("mytable1")),
 
  # Uncomment for debugging
  # tableOutput("inputvals"),
  # tableOutput("mytable1"),
  #  tableOutput("mytable2"),
  
  DT::dataTableOutput("data.table.ind.treatment", width = 500)
 
# tags$h5(textOutput("cost.indiv.treatments")),
 #
 
 # tags$h5(textOutput("group.treatment")),
 # tags$h5(htmlOutput("people.gr")),
 # tags$h5(textOutput("cost.group.treatments")),
  
 # tags$h5(textOutput("med")),
 # tags$h5(textOutput("cost.meds")),
  
 #  tags$h5(textOutput("people_ind"))
 
 ) # close tags$body
 ) # close mainPanel


)))
