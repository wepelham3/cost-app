library(shiny)
library(magrittr)
library(readr)
library(DT)
library(shinyjs)

#df.meds = read_csv("data-meds.csv", col_names = TRUE)
#df.comps = read_csv("data-comps.csv", col_names = TRUE)


shinyServer(function(input, output, session) {
  
  #use shiny js 
  shinyjs::useShinyjs()
  
 # values <- reactiveValues()
 # values$df.indiv.treatment <- data.frame(Label = character(0), Frequency = character(0), Duration = numeric(0), stringsAsFactors=FALSE)
 # values$df.persons.ind <- data.frame(Person = character(0), Price = numeric(0), Commuting = character(0), stringsAsFactors=FALSE)
 # values$num.persons.ind <- as.integer(input$"num.persons.ind")  
  
 # uncomment for debugging: send all Input Values to the interface
  
 #  output$inputvals<-renderTable({
 #    as.data.frame(reactiveValuesToList(input))
 #  })



  # Individual Treatments CRUD
  
  # Indiv. Treatment input fields are treated as a group
  
  formDataIndivTreatment <- reactive({
    sapply(names(GetTableMetadataIndivTreatment()$fields), function(x) input[[x]])
  })
  
  
 # uncommment for debugging
 #  output$mytable1<-renderTable({
 #    as.data.frame(formDataIndivTreatment())
 #})
 
 #  output$mytable2<-renderTable({ 
 #   as.data.frame(ReadDataIndivTreatment()[input$data.table.ind.treatment_rows_selected, ])
 #  })
 ####################
  
  
  # The "Label" field is mandatory and thus the "Submit" button should not be enabled if there is no name
  # Define here rulese for mandatory fields 
  # more examples at http://deanattali.com/2015/04/23/shinyjs-r-package/
  observe({
    if (is.null(input$label.ind) || input$label.ind == "") {
      shinyjs::disable("submit.ind")
    } else {
      shinyjs::enable("submit.ind")
    }
    
    if (input$id.ind == 0) {
      shinyjs::disable("delete.ind")
    } else {
      shinyjs::enable("delete.ind")
    }
    
  })
  
  # Show/Hide Persons
  shinyjs::onclick("add.person2.ind",
                   shinyjs::toggle(id = "p2.ind", anim = TRUE))
  
  # Add the Person 3
  shinyjs::onclick("add.person3.ind",
                   shinyjs::toggle(id = "p3.ind", anim = TRUE))
  
  # Add the Person 4
  shinyjs::onclick("add.person4.ind",
                   shinyjs::toggle(id = "p4.ind", anim = TRUE))
  
  # Add the Person 5
  shinyjs::onclick("add.person5.ind",
                   shinyjs::toggle(id = "p5.ind", anim = TRUE))
  
  
  # Click "Submit" button to save the data
   observeEvent(input$submit.ind, {
     if (input$id.ind != "0") {
       UpdateDataIndivTreatment(formDataIndivTreatment(), session)
     } else { #new record
       CreateDataIndivTreatment(formDataIndivTreatment())
       UpdateInputsIndivTreatment(CreateDefaultIndivTreatment(), session)
     }
   }, priority = 1)
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$data.table.ind.treatment_rows_selected, {
    if (length(input$data.table.ind.treatment_rows_selected) > 0) {
      data <- ReadDataIndivTreatment()[input$data.table.ind.treatment_rows_selected, ]
      UpdateInputsIndivTreatment(data, session)
    
      if (data["person2.ind"] != "None" )
        shinyjs::show(id = "p2.ind", anim = TRUE)
       else
        shinyjs::hide(id = "p2.ind", anim = TRUE) 
      
      if (data["person3.ind"] != "None" )
        shinyjs::show(id = "p3.ind", anim = TRUE)
      else
        shinyjs::hide(id = "p3.ind", anim = TRUE) 
      
      if (data["person4.ind"] != "None" )
        shinyjs::show(id = "p4.ind", anim = TRUE)
      else
        shinyjs::hide(id = "p4.ind", anim = TRUE) 
      
      if (data["person5.ind"] != "None" )
        shinyjs::show(id = "p5.ind", anim = TRUE)
      else
        shinyjs::hide(id = "p5.ind", anim = TRUE) 
      
    }
  })
    
  # Press "Delete" button -> delete from data
  observeEvent(input$delete.ind, {
    DeleteDataIndivTreatment(formDataIndivTreatment())
    UpdateInputsIndivTreatment(CreateDefaultIndivTreatment(), session)
  }, priority = 1)
   
  # Press "Reset" button -> display empty record for IndivTreatment
  observeEvent(input$reset.ind, {
    UpdateInputsIndivTreatment(CreateDefaultIndivTreatment(), session)
    shinyjs::hide(id = "p2.ind", anim = TRUE)
    shinyjs::hide(id = "p3.ind", anim = TRUE)
    shinyjs::hide(id = "p4.ind", anim = TRUE)
    shinyjs::hide(id = "p5.ind", anim = TRUE)
  })
  
  # Display the Data Table
  output$data.table.ind.treatment <- DT::renderDataTable({
   #update after submit is clicked
  input$submit.ind
   #update after delete is clicked
  input$delete.ind
   ReadDataIndivTreatment()
 }, server = FALSE, selection = "single", 
 colnames = unname(GetTableMetadataIndivTreatment()$fields)[-1]
 , options = list(sDom  = '<"top">flrt<"bottom">ip)') # to suppress search box
 # <"top">flrt<"bottom">ip - The syntax is a bit quirky, but basically the above says that f, l, r and t options
 # are to be placed in the top div with the i and p options in the bottom div. 
 #Please refer to the docs at http://legacy.datatables.net/usage/options for a more thorough explanation.
 # "f" is the "Filtering input" option (i.e. the search option) and by moving "f" to another div 
 # or omitting it we may move around or disable the search bar.
 # and omitting "l" will disable the show entries
 
 )     
 
  
  # It was before: Dynamically Rendering the input controls for the Individual Treatments's persons
  
  #  output$persons.ind <- renderUI({
  
  #    num.persons.ind <- as.integer(input$"num.persons.ind");
  
  #    lapply(1:num.persons.ind, function(i) {
  
  #      selectInput(inputId = paste0("person", i, ".ind"), label = paste0("Person ", i), choices = df.comps$person);
  
  #    })
  #  })
  
  #  output$persons.comm.ind <- renderUI({
  #    num.persons.ind <- as.integer(input$"num.persons.ind");
  
  #    lapply(1:num.persons.ind, function(i) {
  
  #      textInput(inputId = paste0("p", i, ".comm.ind"), label = paste0("Person ", i, " Commute, one-way (min)" ), value = "20");
  
  # radioButtons(
  #  paste0("p", i, ".ind.yn"),
  #   label = tags$h5(paste0("Is the person ", i, " commuting to the session?")), choices = c("Yes", "No"), selected = NULL
  
  #    })
  #  })
  
  
  
  
 # Sending Outputs to the interface
# output$list.of.components = renderText({
#   print("Protocol Components:")
# })

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
  
 # output$cost.indiv.treatments = renderText({
 #  
 #  })
  
  # --------------------------------------------------------------------
  #group dynamic code
  # --------------------------------------------------------------------
  
#  observeEvent(input$add.gr, {
#    
#    output$group.treatment = renderText({
#      paste("Group treatment: ", input$label.gr, " | frequency = ", input$frequency.gr, " times per year | duration = ",  input$duration.gr, " min")
#    })
#  
#    output$people.gr = renderUI ({
#      str1 <- paste("Person 1: ", input$person1.gr, " | Price: ", as.numeric(df.comps[which(df.comps$person == input$person1.gr), 2]) , " | Commuting: ", input$p1.gr.yn)
#      str2 <- paste("Person 2: ", input$person2.gr, " | Price: ", as.numeric(df.comps[which(df.comps$person == input$person2.gr), 2]) , " | Commuting: ", input$p2.gr.yn)
#      HTML(paste(str1, str2, sep ='<br>'))
#    })
#  })
  
#  output$cost.group.treatments = renderText({
    

#  })
  
  # --------------------------------------------------------------------
  #meds dynamic code
  # --------------------------------------------------------------------

#  observeEvent(input$add.med, {
#    save.string = paste0("--- ",
#                         input$med, ", ",
#                         input$sched.times, "x daily, ",
#                         input$sched.weekly, ", ",
#                         input$sched.yearly,
#                          " | Price: ", as.numeric(df.meds[which(df.meds$name == input$med), 2]))
#    output$med = renderText({
#      print(save.string)
#    })
#  })
  
#  output$cost.meds = renderText({
  
  
#  })
  
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
