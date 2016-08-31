library(shiny)
library(magrittr)
library(readr)
library(DT)

df.meds = read_csv("data-meds.csv", col_names = TRUE)
df.comps = read_csv("data-comps.csv", col_names = TRUE)

# --------------------------------------------------------------------

# CRUD Functions for Individual Treatments

# Get table metadata
GetTableMetadataIndivTreatment <- function() {
  fields <- c(id.ind = "Id", 
              label.ind = "Label", 
              frequency.ind = "Frequency", 
              duration.ind = "Duration",
              num.persons.ind = "Num Persons")
  result <- list(fields = fields)
  return (result)
}

# Find the next ID of a new record
# (in mysql, this could be done by an incremental index)
GetNextIdIndivTreatment <- function() {
  if (exists("responsesIndivTreatment") && nrow(responsesIndivTreatment) > 0) {
    max(as.integer(rownames(responsesIndivTreatment))) + 1
  } else {
    return (1)
  }
}

#C - CREATE
CreateDataIndivTreatment <- function(data) {
  
  data <- CastDataIndivTreatment(data)
  rownames(data) <- GetNextIdIndivTreatment()
  if (exists("responsesIndivTreatment")) {
    responsesIndivTreatment <<- rbind(responsesIndivTreatment, data)
  } else {
    responsesIndivTreatment <<- data
  }
}

#R - READ
ReadDataIndivTreatment <- function() {
  if (exists("responsesIndivTreatment")) {
    responsesIndivTreatment
  }
}

#U - UPDATE
UpdateDataIndivTreatment <- function(data) {
  data <- CastDataIndivTreatment(data)
  responsesIndivTreatment[row.names(responsesIndivTreatment) == row.names(data), ] <<- data
}

#D - DELETE
DeleteData <- function(data) {
  responsesIndivTreatment <<- responses[row.names(responsesIndivTreatment) != unname(data["id_ind"]), ]
}

# Return an empty, new record
CreateDefaultIndivTreatment <- function() {
  default.indiv.treatment <- CastDataIndivTreatment(list(id.ind = "0", label.ind = "", frequency.ind = 0, duration.ind = 0, num.persons.ind = 1))
  #mydefault <- CastData(list(id = "0", name = "", used_shiny = FALSE, r_num_years = 2))
  return (default.indiv.treatment)
}

# Cast from Inputs to a one-row data.frame
CastDataIndivTreatment <- function(data) {
  datar <- data.frame(label.ind = data["label.ind"], 
                      frequency.ind = as.integer(data["frequency.ind"]),
                      duration.ind = as.integer(data["duration.ind"]),
                      num.persons.ind = as.integer(data["num.persons.ind"]),
                      stringsAsFactors = FALSE)
  rownames(datar) <- data["id.ind"]
  return (datar)
}

# Fill the input fields with the values of the selected record in the table
UpdateInputsIndivTreatment <- function(data, session) {
  updateTextInput(session, "id.ind", value = unname(rownames(data)))
  updateTextInput(session, "label.ind", value = unname(data["label.ind"]))
  updateSliderInput(session, "frequency.ind", value = as.integer(data["frequency.ind"]))
  updateTextInput(session, "duration.ind", value = as.integer(data["duration.ind"]))
  updateTextInput(session, "num.persons.ind", value = as.integer(data["num.persons.ind"]))
}  
  

shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  #values$df.indiv.treatment <- data.frame(Label = character(0), Frequency = character(0), Duration = numeric(0), stringsAsFactors=FALSE)
  values$df.persons.ind <- data.frame(Person = character(0), Price = numeric(0), Commuting = character(0), stringsAsFactors=FALSE)
  
  # Individual Treatments CRUD
  
  # Indiv. Treatment input fields are treated as a group

  formDataIndivTreatment <- reactive({
    sapply(names(GetTableMetadataIndivTreatment()$fields), function(x) input[[x]])
  })
  
  # Dynamically Rendering the input controls for the Individual Treatments's persons
  
  output$persons.ind <- renderUI({
    num.persons.ind <- as.integer(input$"num.persons.ind");
    
    lapply(1:num.persons.ind, function(i) {
      
      selectInput(inputId = paste0("person", i, ".ind"), label = paste0("Person ", i), choices = df.comps$person);
      
    })
  })
  
  output$persons.comm.ind <- renderUI({
    num.persons.ind <- as.integer(input$"num.persons.ind");
    
    lapply(1:num.persons.ind, function(i) {
      
      radioButtons(
        paste0("p", i, ".ind.yn"),
        label = tags$h5(paste0("Is the person ", i, " commuting to the session?")), choices = c("Yes", "No"), selected = NULL
      )
    })
  })
  
  # Processing Events for IndivTreatment
  # Click "Add Component" button -> save data
  observeEvent(input$add.ind, {
    if (input$add.ind == "0") {
      return ()
    } else {
      CreateDataIndivTreatment(formDataIndivTreatment())
      UpdateInputsIndivTreatment(CreateDefaultIndivTreatment(), session)
    }
  }, priority = 1)
  
  # Click "Add Component" button -> save data
  # observeEvent(input$add.ind, {
  #   if (input$add.ind != "0") {
  #     UpdateDataIndivTreatment(formDataIndivTreatment())
  #   } else {
  #     CreateDataIndivTreatment(formDataIndivTreatment())
  #     UpdateInputsIndivTreatment(CreateDefaultIndivTreatment(), session)
  #   }
  # }, priority = 1)
  
  # Press "New" button -> display empty record for IndivTreatment
  observeEvent(input$new.ind, {
    UpdateInputsIndivTreatment(CreateDefaultIndivTreatment(), session)
  })
  
  observe({
    if (input$add.ind == 0)
      return()
    isolate({
     # Your logic here
        output$indiv.treatment = renderText({
        paste("Individual treatment: ", input$label.ind, " | frequency = ", input$frequency.ind, " times per year | duration = ",  input$duration.ind, " min")
        })
        num.persons.ind <- as.integer(input$"num.persons.ind");
        #newLine <- isolate(c(as.character(input$person1.ind), as.numeric(df.comps[which(df.comps$person == input$person1.ind), 2]), input$p1.ind.yn))
        #isolate(values$df.persons.ind[nrow(values$df.persons.ind) + 1,]
        #              <- c(input$person1.ind, as.numeric(df.comps[which(df.comps$person == input$person1.ind), 2]), input$p1.ind.yn))
        newLine <- c(as.character(input$person1.ind), as.numeric(df.comps[which(df.comps$person == input$person1.ind), 2]), input$p1.ind.yn)
        isolate(values$df.persons.ind[nrow(values$df.persons.ind) + 1,]
                <- c(input$person1.ind, as.numeric(df.comps[which(df.comps$person == input$person1.ind), 2]), input$p1.ind.yn))
        if(num.persons.ind > 1){

             newLine <- c(as.character(input$person2.ind), as.numeric(df.comps[which(df.comps$person == input$person2.ind), 2]), input$p2.ind.yn)
             isolate(values$df.persons.ind[nrow(values$df.persons.ind) + 1,]
                     <- c(input$person2.ind, as.numeric(df.comps[which(df.comps$person == input$person2.ind), 2]), input$p2.ind.yn))

         }

          if(num.persons.ind > 2){

            newLine <- c(as.character(input$person3.ind), as.numeric(df.comps[which(df.comps$person == input$person3.ind), 2]), input$p3.ind.yn)
            isolate(values$df.persons.ind[nrow(values$df.persons.ind) + 1,]
                    <- c(input$person3.ind, as.numeric(df.comps[which(df.comps$person == input$person3.ind), 2]), input$p3.ind.yn))

          }

          if(num.persons.ind > 3){

            newLine <- isolatec(as.character(input$person4.ind), as.numeric(df.comps[which(df.comps$person == input$person4.ind), 2]), input$p4.ind.yn)
            isolate(values$df.persons.ind[nrow(values$df.persons.ind) + 1,]
                    <- c(input$person4.ind, as.numeric(df.comps[which(df.comps$person == input$person4.ind), 2]), input$p4.ind.yn))


          }

          if(num.persons.ind > 4){

            newLine <- isolatec(as.character(input$person5.ind), as.numeric(df.comps[which(df.comps$person == input$person5.ind), 2]), input$p5.ind.yn)
            isolate(values$df.persons.ind[nrow(values$df.persons.ind) + 1,]
                    <- c(input$person5.ind, as.numeric(df.comps[which(df.comps$person == input$person5.ind), 2]), input$p5.ind.yn))


          }
       })
  })
 
  #observeEvent(input$add.ind, {
  #   output$indiv.treatment = renderText({
  #    paste("Individual treatment: ", input$label.ind, " | frequency = ", input$frequency.ind, " times per year | duration = ",  input$duration.ind, " min")
  #})
  #}) # close observeEvent(input$add.ind)
  
  
 output$table.persons.ind <- renderTable({values$df.persons.ind})
 
 # display table
 output$responsesIndivTreatment <- DT::renderDataTable({
   #update after submit is clicked
  input$add.ind
   #update after delete is clicked
  #input$delete
   ReadDataIndivTreatment()
 }, server = FALSE, selection = "single", 
 colnames = unname(GetTableMetadataIndivTreatment()$fields)[-1]
 , options = list(sDom  = '<"top">rt<"bottom">ip') # to suppress search box
 # <"top">flrt<"bottom">ip - The syntax is a bit quirky, but basically the above says that f, l, r and t options
 # are to be placed in the top div with the i and p options in the bottom div. 
 #Please refer to the docs at http://legacy.datatables.net/usage/options for a more thorough explanation.
 # "f" is the "Filtering input" option (i.e. the search option) and by moving "f" to another div 
 # or omitting it we may move around or disable the search bar.
 # and omitting "l" will disable the show entries
 
 )     
 
 # Sending Outputs to the interface
 output$list.of.components = renderText({
   print("Protocol Components:")
 })

 # output$people.ind = renderUI ({
 #      str1 <- paste("Person 1: ", input$person1.ind, " | Price: ", as.numeric(df.comps[which(df.comps$person == input$person1.ind), 2]) , " | Commuting: ", input$p1.ind.yn)
 #      str2 <- paste("Person 2: ", input$person2.ind, " | Price: ", as.numeric(df.comps[which(df.comps$person == input$person2.ind), 2]) , " | Commuting: ", input$p2.ind.yn)
 #      HTML(paste(str1, str2, sep ='<br>'))
 #    })
  
  # output$people_ind = renderText ({
  #    paste(sep='\n',   paste("Person 1: ", input$person1_ind, " | Price: ", as.numeric(df.comps[which(df.comps$person == input$person1_ind), 2]) , " | Commuting: ", input$p1_ind_yn)
  #                    , paste("Person 2: ", input$person2_ind, " | Price: ", as.numeric(df.comps[which(df.comps$person == input$person2_ind), 2]) , " | Commuting: ", input$p2_ind_yn))
  #  })
  #  })
  #})
  
  output$cost.indiv.treatments = renderText({
  
  })
  
  observeEvent(input$add.gr, {
    
    output$group.treatment = renderText({
      paste("Group treatment: ", input$label.gr, " | frequency = ", input$frequency.gr, " times per year | duration = ",  input$duration.gr, " min")
    })
  
    output$people.gr = renderUI ({
      str1 <- paste("Person 1: ", input$person1.gr, " | Price: ", as.numeric(df.comps[which(df.comps$person == input$person1.gr), 2]) , " | Commuting: ", input$p1.gr.yn)
      str2 <- paste("Person 2: ", input$person2.gr, " | Price: ", as.numeric(df.comps[which(df.comps$person == input$person2.gr), 2]) , " | Commuting: ", input$p2.gr.yn)
      HTML(paste(str1, str2, sep ='<br>'))
    })
  })
  
  output$cost.group.treatments = renderText({
    
  })
  

  observeEvent(input$add.med, {
    save.string = paste0("--- ",
                         input$med, ", ",
                         input$sched.times, "x daily, ",
                         input$sched.weekly, ", ",
                         input$sched.yearly,
                          " | Price: ", as.numeric(df.meds[which(df.meds$name == input$med), 2]))
    output$med = renderText({
      print(save.string)
    })
  })
  
  output$cost.meds = renderText({
  
  
  })
  
  # output$total.cost.explicit = renderText({
  #   (input$frequency * input$duration * 10) %>%
  #     paste0("Total explicit cost is... $", ., " in 2015 USD")
  # })
  # 
  # output$total.cost.implicit = renderText({
  #   (input$frequency * input$duration * 10) %>%
  #     paste0("Total implicit cost is... $", ., " in 2015 USD")
  # })
  # 
  # output$total.cost.combined = renderText({
  #   (input$frequency * input$duration * 10) %>%
  #     paste0("Total combined cost is... $", ., " in 2015 USD")
  # })

})
