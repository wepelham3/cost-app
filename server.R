library(shiny)
library(magrittr)
library(dplyr)
library(readr)
library(DT)
library(shinyjs)


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

 # Synchronize the Input and Output tabsetPanels
  
  observeEvent(input$in.components, {
    updateTabsetPanel(session, "out.components",
                      selected = input$in.components
    )
  })
  
  observeEvent(input$out.components, {
    updateTabsetPanel(session, "in.components",
                      selected = input$out.components
    )
  })
  
 
  # Individual Treatments CRUD
  
  # Indiv. Treatment input fields are treated as a group
  
  formDataIndivTreatment <- reactive({
    sapply(names(GetTableMetadataIndivTreatment()$fields), function(x) input[[x]])
  })
  
  
  #$values.component.cost.ind <-
  

 # uncommment for debugging ########################################################################
 #  output$mytable1<-renderTable({
 #      as.data.frame(formDataIndivTreatment())
 #})
 
 #  output$mytable2<-renderTable({ 
 #     as.data.frame(ReadDataIndivTreatment()[input$data.table.ind.treatment_rows_selected, ])
 #  })
 ####################################################################################################
  
  
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
    #  input$cost.ind <-ReadDataIndivTreatment()[input$data.table.ind.treatment_rows_selected, ]["cost.ind"]  
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
  
 
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  
  #CRUD Group treatment
  
  
  formDataGroupTreatment <- reactive({
    sapply(names(GetTableMetadataGroupTreatment()$fields), function(x) input[[x]])
  })
  
  
  
  observe({
    if (is.null(input$label.gr) || input$label.gr == "" || input$num.families.gr == 0 ) {
      shinyjs::disable("submit.gr")
    } else {
      shinyjs::enable("submit.gr")
    }
    
    if (input$id.gr == 0) {
      shinyjs::disable("delete.gr")
    } else {
      shinyjs::enable("delete.gr")
    }
    
    
  })
  
  
  
  # Show/Hide Persons
  shinyjs::onclick("add.person2.gr",
                   shinyjs::toggle(id = "p2.gr", anim = TRUE))
  
  # Add the Person 3
  shinyjs::onclick("add.person3.gr",
                   shinyjs::toggle(id = "p3.gr", anim = TRUE))
  
  # Add the Person 4
  shinyjs::onclick("add.person4.gr",
                   shinyjs::toggle(id = "p4.gr", anim = TRUE))
  
  # Add the Person 5
  shinyjs::onclick("add.person5.gr",
                   shinyjs::toggle(id = "p5.gr", anim = TRUE))
  
  
  
  
  # Click "Submit" button to save the data
  observeEvent(input$submit.gr, {
    if (input$id.gr != "0") {
      UpdateDataGroupTreatment(formDataGroupTreatment(), session)
    } else { #new record
      CreateDataGroupTreatment(formDataGroupTreatment())
      UpdateInputsGroupTreatment(CreateDefaultGroupTreatment(), session)
    }
  }, priority = 1)
  
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$data.table.gr.treatment_rows_selected, {
    if (length(input$data.table.gr.treatment_rows_selected) > 0) {
      data <- ReadDataGroupTreatment()[input$data.table.gr.treatment_rows_selected, ]
      UpdateInputsGroupTreatment(data, session)
      
      if (data["person2.gr"] != "None" )
        shinyjs::show(id = "p2.gr", anim = TRUE)
      else
        shinyjs::hide(id = "p2.gr", anim = TRUE) 
      
      if (data["person3.gr"] != "None" )
        shinyjs::show(id = "p3.gr", anim = TRUE)
      else
        shinyjs::hide(id = "p3.gr", anim = TRUE) 
      
      if (data["person4.gr"] != "None" )
        shinyjs::show(id = "p4.gr", anim = TRUE)
      else
        shinyjs::hide(id = "p4.gr", anim = TRUE) 
      
      if (data["person5.gr"] != "None" )
        shinyjs::show(id = "p5.gr", anim = TRUE)
      else
        shinyjs::hide(id = "p5.gr", anim = TRUE) 
      
    }
  })
  
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete.gr, {
    DeleteDataGroupTreatment(formDataGroupTreatment())
    UpdateInputsGroupTreatment(CreateDefaultGroupTreatment(), session)
  }, priority = 1)
  
  # Press "Reset" button -> display empty record for IndivTreatment
  observeEvent(input$reset.gr, {
    UpdateInputsGroupTreatment(CreateDefaultGroupTreatment(), session)
    shinyjs::hide(id = "p2.gr", anim = TRUE)
    shinyjs::hide(id = "p3.gr", anim = TRUE)
    shinyjs::hide(id = "p4.gr", anim = TRUE)
    shinyjs::hide(id = "p5.gr", anim = TRUE)
  })
 
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # CRUD medication table
 

  formDataMedication <- reactive({
    sapply(names(GetTableMetadataMedication()$fields), function(x) input[[x]])
  })
  
  
  
  observe({
    if (is.null(input$label.med) || input$label.med == ""  || is.null(input$frequency.med)  || is.null(input$week.med) || is.null(input$year.med))
     {
      shinyjs::disable("submit.med")
      } else {
      shinyjs::enable("submit.med")
    }
    
    if (input$id.med == 0) {
      shinyjs::disable("delete.med")
    } else {
      shinyjs::enable("delete.med")
    }
    
  })
  
  
  # Click "Submit" button to save the data
  observeEvent(input$submit.med, {
    if (input$id.med != "0") {
      UpdateDataMedication(formDataMedication())
    } else { #new record
      CreateDataMedication(formDataMedication())
      UpdateInputsMedication(CreateDefaultMedication(), session)
    }
  }, priority = 1)
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$data.table.med_rows_selected, {
    if (length(input$data.table.med_rows_selected) > 0) {
      data <- ReadDataMedication()[input$data.table.med_rows_selected, ]
      UpdateInputsMedication(data, session)
      
    }
  })
  
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete.med, {
    DeleteDataMedication(formDataMedication())
    UpdateInputsMedication(CreateDefaultMedication(), session)
  }, priority = 1)
  
  
  # Press "Reset" button -> display empty record for IndivTreatment
  observeEvent(input$reset.med, {
    UpdateInputsMedication(CreateDefaultMedication(), session)

  })
  #############################################################
  ##########      Render DataTables   #########################
  #############################################################
  
  # Render the DataTable for the Individual Treatment data.frame
  output$data.table.ind.treatment <- DT::renderDataTable({
    #update after submit is clicked
    input$submit.ind
    #update after delete is clicked
    input$delete.ind
    ReadDataIndivTreatment()
  }, server = FALSE, selection = "single", 
  colnames = unname(GetTableMetadataIndivTreatment()$fields)[-1]
  , options = list(sDom  = '<"top">rt<"bottom">ip)'
                   #                   autoWidth = TRUE,
                   #                   columnDefs = list(list(width = '50px', targets = "_all"))
  ) 
  # to suppress search box
  # <"top">flrt<"bottom">ip - The syntax is a bit quirky, but basically the above says that f, l, r and t options
  # are to be placed in the top div with the i and p options in the bottom div. 
  #Please refer to the docs at http://legacy.datatables.net/usage/options for a more thorough explanation.
  # "f" is the "Filtering input" option (i.e. the search option) and by moving "f" to another div 
  # or omitting it we may move around or disable the search bar.
  # and omitting "l" will disable the show entries
  
  )     
  
  
  # Render the DataTable for the Group Treatment data.frame
  output$data.table.gr.treatment <- DT::renderDataTable({
    #update after submit is clicked
    input$submit.gr
    #update after delete is clicked
    input$delete.gr
    ReadDataGroupTreatment()
  }, server = FALSE, selection = "single", 
  colnames = unname(GetTableMetadataGroupTreatment()$fields)[-1]
  , options = list(sDom  = '<"top">rt<"bottom">ip)') # to suppress search box
  # <"top">flrt<"bottom">ip - The syntax is a bit quirky, but basically the above says that f, l, r and t options
  # are to be placed in the top div with the i and p options in the bottom div. 
  #Please refer to the docs at http://legacy.datatables.net/usage/options for a more thorough explanation.
  # "f" is the "Filtering input" option (i.e. the search option) and by moving "f" to another div 
  # or omitting it we may move around or disable the search bar.
  # and omitting "l" will disable the show entries
  
  ) 
  
  # Render the DataTable for the Medication data.frame
  output$data.table.med <- DT::renderDataTable({
    #update after submit is clicked
    input$submit.med
    #update after delete is clicked
    input$delete.med
    ReadDataMedication()
  }, server = FALSE, selection = "single", 
  colnames = unname(GetTableMetadataMedication()$fields)[-1]
  , options = list(sDom  = '<"top">rt<"bottom">ip)') # to suppress search box
  # <"top">flrt<"bottom">ip - The syntax is a bit quirky, but basically the above says that f, l, r and t options
  # are to be placed in the top div with the i and p options in the bottom div. 
  #Please refer to the docs at http://legacy.datatables.net/usage/options for a more thorough explanation.
  # "f" is the "Filtering input" option (i.e. the search option) and by moving "f" to another div 
  # or omitting it we may move around or disable the search bar.
  # and omitting "l" will disable the show entries
  
  )
   
  #############################################################
  ##########      Download Data     ###########################
  #############################################################
  
  # disable the download buttons if the dataset is empty
  # it doesn't work for now, need to use some reactive values 
  #observe({
  #  if (exists("df.ind.treatment") && nrow(df.ind.treatment) > 0 ) { 
  #    shinyjs::show("download.table.ind.treatment")
  #   } else {
  #    shinyjs::hide("download.table.ind.treatment")
  #  }
  #  if (exists("df.gr.treatment")) { # && nrow(df.gr.treatment) > 0 ) { 
  #    shinyjs::show("download.table.gr.treatment")
  #  } else {
  #    shinyjs::hide("download.table.gr.treatment")
  #  }
  #  if (exists("df.med")) { # && nrow(df.med) > 0 ) { 
  #    shinyjs::show("download.table.med")
  #  } else {
  #    shinyjs::hide("download.table.med")
  # }
    
  #})
  
  
  output$download.table.ind.treatment <- downloadHandler(
    filename = function() { 
      paste('protocol_individual_treatments', '.csv', sep='') 
    },
    content = function(file) {
      write.csv(ReadDataIndivTreatment(), file)
    }
  ) 
  
  output$download.table.gr.treatment <- downloadHandler(
    filename = function() { 
      paste('protocol_group_treatments', '.csv', sep='') 
    },
    content = function(file) {
      write.csv(ReadDataGroupTreatment(), file)
    }
  ) 
  
  output$download.table.med <- downloadHandler(
    filename = function() { 
      paste('protocol_medications', '.csv', sep='') 
    },
    content = function(file) {
      write.csv(ReadDataMedication(), file)
    }
  )
  
  # Render DataTable for looking up med prices
  output$DT.lookup.meds = DT::renderDataTable({
    df.meds %>%
      datatable(colnames = c("Medication", "Price"),
                rownames = FALSE,
                options = list(pageLength = 10)) %>%
      formatCurrency("price", digits = 2)
    })
  
  # Render DataTable for looking up person wages and compensations
  output$DT.lookup.comps = DT::renderDataTable({
    df.comps %>%
      datatable(colnames = c("Professional", "Full BLS Label", "Hourly Wage", "Hourly Compensation"),
                rownames = FALSE,
                options = list(dom = "ft",
                               pageLength = 50)) %>%
      formatCurrency(c("wage.per.hour", "comp.per.hour"), digits = 0)
    })
  
  
  # Display the resulting table
  
  output$summary_1 <- renderTable({
    as.data.frame(GetSummary())
    #colnames = FALSE
    
  },
  include.colnames=FALSE)
  
  output$summary_2 <- renderTable({
    as.data.frame(GetIndividualPsych())
    
  })
  
  output$summary_3 <- renderTable({
    as.data.frame(GetGroupPsych())
    
  })
  
  output$summary_4 <- renderTable({
    as.data.frame(GetMedicationCost())
    
  })
  
})

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
