library(shiny)
library(magrittr)
library(dplyr)
library(readr)
library(DT)
library(shinyjs)


shinyServer(function(input, output, session) {
  
 #use shiny js 
  shinyjs::useShinyjs()
  
  values <- reactiveValues()
  values$med.costs <- GetTotalCostsMedication()
  values$df.summary.ind  <- GetSummaryByIndivTreatment()
  values$df.summary.gr  <- GetSummaryByGroupTreatment()
  values$df.summary.med  <- GetSummaryByMedication()
  values$df.summary.person  <- GetSummaryByPerson()
  values$df.summary.parent  <- GetSummaryByParent()
  values$parent.costs <- sum(GetSummaryByParent()$Cost)
  values$prof.costs <- sum(GetSummaryByPerson()$Cost) - sum(GetSummaryByParent()$Cost)
 
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
     values$df.summary.ind  <- GetSummaryByIndivTreatment()
     
     # Update the summary by person and parent lists and costs
     values$df.summary.person  <- GetSummaryByPerson()
     values$df.summary.parent  <- GetSummaryByParent()
     values$parent.costs <- sum(GetSummaryByParent()$Cost)
     values$prof.costs <- sum(GetSummaryByPerson()$Cost) - sum(GetSummaryByParent()$Cost)
     
     
     
   }, priority = 1)
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$data.table.ind.treatment_rows_selected, {
    if (length(input$data.table.ind.treatment_rows_selected) > 0) {
      data <- ReadDataIndivTreatment()[input$data.table.ind.treatment_rows_selected, ]
      UpdateInputsIndivTreatment(data, session)
    
      if ( data["person2.ind"] != "" && data["person2.ind"] != "None" )
        shinyjs::show(id = "p2.ind", anim = TRUE)
       else
        shinyjs::hide(id = "p2.ind", anim = TRUE) 
      
      if ( data["person3.ind"] != "" &&  data["person3.ind"] != "None" )
        shinyjs::show(id = "p3.ind", anim = TRUE)
      else
        shinyjs::hide(id = "p3.ind", anim = TRUE) 
      
      if (data["person4.ind"] != "" && data["person4.ind"] != "None" )
        shinyjs::show(id = "p4.ind", anim = TRUE)
      else
        shinyjs::hide(id = "p4.ind", anim = TRUE) 
      
      if (data["person5.ind"] != "" && data["person5.ind"] != "None" )
        shinyjs::show(id = "p5.ind", anim = TRUE)
      else
        shinyjs::hide(id = "p5.ind", anim = TRUE) 
      
    }
  })
    
  # Press "Delete" button -> delete from data
  observeEvent(input$delete.ind, {
    
    DeleteDataIndivTreatment(formDataIndivTreatment())
    UpdateInputsIndivTreatment(CreateDefaultIndivTreatment(), session)
    values$df.summary.ind  <- GetSummaryByIndivTreatment()
    
    # Update the summary by person and parent lists and costs
    values$df.summary.person  <- GetSummaryByPerson()
    values$df.summary.parent  <- GetSummaryByParent()
    values$parent.costs <- sum(GetSummaryByParent()$Cost)
    values$prof.costs <- sum(GetSummaryByPerson()$Cost) - sum(GetSummaryByParent()$Cost)
    
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
    values$df.summary.gr  <- GetSummaryByGroupTreatment()
    
    
    # Update the summary by person and parent lists and costs
    values$df.summary.person  <- GetSummaryByPerson()
    values$df.summary.parent  <- GetSummaryByParent()
    values$parent.costs <- sum(GetSummaryByParent()$Cost)
    values$prof.costs <- sum(GetSummaryByPerson()$Cost) - sum(GetSummaryByParent()$Cost)
    
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
    values$df.summary.gr  <- GetSummaryByGroupTreatment()
    
    # Update the summary by person and parent lists and costs
    values$df.summary.person  <- GetSummaryByPerson()
    values$df.summary.parent  <- GetSummaryByParent()
    values$parent.costs <- sum(GetSummaryByParent()$Cost)
    values$prof.costs <- sum(GetSummaryByPerson()$Cost) - sum(GetSummaryByParent()$Cost)
    
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
    # values$med.costs <- values$med.costs + CalculateCostMedication(formDataMedication())      #sum(ReadDataMedication()$cost.med)
    values$med.costs <- GetTotalCostsMedication()
    values$df.summary.med  <- GetSummaryByMedication()
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
    values$med.costs <- GetTotalCostsMedication()
    values$df.summary.med  <- GetSummaryByMedication()
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
    if (!is.null(ReadDataIndivTreatment())) {
      ReadDataIndivTreatment() %>%
        mutate(cost.ind = paste0("$", sprintf("%.0f", round(cost.ind, 0))))
    }
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
    if (!is.null(ReadDataGroupTreatment())) {
      ReadDataGroupTreatment() %>%
        mutate(cost.gr = paste0("$", sprintf("%.0f", round(cost.gr, 0))))
    }
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
    if (!is.null(ReadDataMedication())) {
      ReadDataMedication() %>%
        mutate(cost.med = paste0("$", sprintf("%.0f", round(cost.med, 0))))
    }
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
  
  
  # hide/show download buttons  
  # observe(CheckData(df.ind.treatment),{
  #   if(data == TRUE){
  #     shinyjs::show("download.table.ind.treatment")
  #   }else{
  #     shinyjs::hide("download.table.ind.treatment")
  #   }
  # 
  #  })
  
  
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
  
  
  # Display the summary tables
  
  output$summary_1 <- renderTable({
    as.data.frame(GetSummary1(values$med.costs+values$prof.costs, values$parent.costs))
    #colnames = FALSE
    
  },
  include.colnames=FALSE,
  include.rownames=FALSE)
  
  # Summary 2
  output$summary_2 <- renderTable({
    as.data.frame(GetSummary2(values$med.costs, values$prof.costs, values$parent.costs)) 
    
  },
  include.colnames=FALSE,
  include.rownames=FALSE)
  
  output$summary_3 <- renderTable({
    as.data.frame(values$df.summary.ind)
    
  },
  include.rownames=FALSE)
  
  output$summary_4 <- renderTable({
    as.data.frame(values$df.summary.gr)
    
  },
  include.rownames=FALSE)
  
  output$summary_5 <- renderTable({
    as.data.frame(values$df.summary.person)
    
  },
  include.rownames=FALSE)
  
  output$summary_6 <- renderTable({
    as.data.frame(values$df.summary.med)
    
  },
  include.rownames=FALSE)
  
})

