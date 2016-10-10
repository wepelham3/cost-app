library(shiny)
library(magrittr)
library(dplyr)
library(readr)
library(DT)
library(shinyjs)
library(scales)


shinyServer(function(input, output, session) {
  
 #use shiny js 
  shinyjs::useShinyjs()
  
  values <- reactiveValues()
  values$med.costs <- GetTotalCostsMedication()
  values$df.summary.ind  <- GetSummaryByIndivTreatment()
  values$df.summary.gr  <- GetSummaryByGroupTreatment()
  values$df.summary.med  <- GetSummaryByMedication()
  
  
  
  UpdateSummaryByPersonAndParent <- function() {
    
  # Update the summary by person and parent lists and costs
  values$df.summary.person  <- GetSummaryByPerson()
  values$df.summary.parent  <- GetSummaryByParent()
  values$parent.costs <- sum(GetSummaryByParent()$cost)
  values$prof.costs <- sum(GetSummaryByPerson()$cost) - sum(GetSummaryByParent()$cost)
  
  }
  
  UpdateSummaryByPersonAndParent()
  
  #values$df.summary.person  <- GetSummaryByPerson()
  #values$df.summary.parent  <- GetSummaryByParent()
  #values$parent.costs <- sum(GetSummaryByParent()$cost)
  #values$prof.costs <- sum(GetSummaryByPerson()$cost) - sum(GetSummaryByParent()$cost)
  
 
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
  
  
  # Define here rules for mandatory fields 
  observe({
    if (is.null(input$label.ind) || input$label.ind == "" || input$frequency.ind == 0
        || input$duration.ind == 0  || input$person1.ind == "" || input$person1.ind == "N/A" ) {
      shinyjs::disable("submit.ind")
    } else {
      shinyjs::enable("submit.ind")
    }
    
    if (input$id.ind == 0) {
      shinyjs::disable("delete.ind")
    } else {
      shinyjs::enable("delete.ind")
    }
    
    # set commute time to 0 if the person is "N/A"
    
    if (input$person2.ind == "N/A"){
      updateTextInput(session, "p2.comm.ind", value = 0)
    }  
    if (input$person3.ind == "N/A"){
      updateTextInput(session, "p3.comm.ind", value = 0)
    }  
    if (input$person4.ind == "N/A"){
      updateTextInput(session, "p4.comm.ind", value = 0)
    } 
    if (input$person5.ind == "N/A"){
      updateTextInput(session, "p5.comm.ind", value = 0)
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
     
     UpdateSummaryByPersonAndParent()
     
     
    # values$df.summary.person  <- GetSummaryByPerson()
    # values$df.summary.parent  <- GetSummaryByParent()
    # values$parent.costs <- sum(GetSummaryByParent()$cost)
    # values$prof.costs <- sum(GetSummaryByPerson()$cost) - sum(GetSummaryByParent()$cost)
     
     # Show/Hide the Download Table Button
     
     if (nrow(ReadDataIndivTreatment()) > 0) { 
       shinyjs::show("download.table.ind.treatment")
     } else {
       shinyjs::hide("download.table.ind.treatment")
     }
     

     
   }, priority = 1)
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$data.table.ind.treatment_rows_selected, {
    if (length(input$data.table.ind.treatment_rows_selected) > 0) {
      data <- ReadDataIndivTreatment()[input$data.table.ind.treatment_rows_selected, ]
      UpdateInputsIndivTreatment(data, session)
    
      if ( data["person2.ind"] != "" && data["person2.ind"] != "N/A" )
        shinyjs::show(id = "p2.ind", anim = TRUE)
       else
        shinyjs::hide(id = "p2.ind", anim = TRUE) 
      
      if ( data["person3.ind"] != "" &&  data["person3.ind"] != "N/A" )
        shinyjs::show(id = "p3.ind", anim = TRUE)
      else
        shinyjs::hide(id = "p3.ind", anim = TRUE) 
      
      if (data["person4.ind"] != "" && data["person4.ind"] != "N/A" )
        shinyjs::show(id = "p4.ind", anim = TRUE)
      else
        shinyjs::hide(id = "p4.ind", anim = TRUE) 
      
      if (data["person5.ind"] != "" && data["person5.ind"] != "N/A" )
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
    
    UpdateSummaryByPersonAndParent()
    
    #values$df.summary.person  <- GetSummaryByPerson()
    #values$df.summary.parent  <- GetSummaryByParent()
    #values$parent.costs <- sum(GetSummaryByParent()$cost)
    #values$prof.costs <- sum(GetSummaryByPerson()$cost) - sum(GetSummaryByParent()$cost)
    
    
    # Show/Hide the Download Table Button
    
    if (nrow(ReadDataIndivTreatment()) > 0) { 
      shinyjs::show("download.table.ind.treatment")
    } else {
      shinyjs::hide("download.table.ind.treatment")
    }
    
    
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
    if (is.null(input$label.gr) || input$label.gr == "" || input$num.families.gr == 0 || input$frequency.gr == 0
        || input$duration.gr == 0  || input$person1.gr == "" || input$person1.gr == "N/A" )
    {
      shinyjs::disable("submit.gr")
    } else {
      shinyjs::enable("submit.gr")
    }
    
    if (input$id.gr == 0) {
      shinyjs::disable("delete.gr")
    } else {
      shinyjs::enable("delete.gr")
    }
    
    # set commute time to 0 if the person is "N/A"
    
    if (input$person2.gr == "N/A"){
      updateTextInput(session, "p2.comm.gr", value = 0)
    }  
    if (input$person3.gr == "N/A"){
      updateTextInput(session, "p3.comm.gr", value = 0)
    }  
    if (input$person4.gr == "N/A"){
      updateTextInput(session, "p4.comm.gr", value = 0)
    } 
    if (input$person5.gr == "N/A"){
      updateTextInput(session, "p5.comm.gr", value = 0)
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
    
    UpdateSummaryByPersonAndParent()
    
    #values$df.summary.person  <- GetSummaryByPerson()
    #values$df.summary.parent  <- GetSummaryByParent()
    #values$parent.costs <- sum(GetSummaryByParent()$cost)
    #values$prof.costs <- sum(GetSummaryByPerson()$cost) - sum(GetSummaryByParent()$cost)
    
    # Hide/Show the Download button
    if (nrow(ReadDataGroupTreatment()) > 0) { 
      shinyjs::show("download.table.gr.treatment")
    } else {
      shinyjs::hide("download.table.gr.treatment")
    }
    
  }, priority = 1)
  
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$data.table.gr.treatment_rows_selected, {
    if (length(input$data.table.gr.treatment_rows_selected) > 0) {
      data <- ReadDataGroupTreatment()[input$data.table.gr.treatment_rows_selected, ]
      UpdateInputsGroupTreatment(data, session)
      
      if ( data["person2.gr"] != "" && data["person2.gr"] != "N/A" )
        shinyjs::show(id = "p2.gr", anim = TRUE)
      else
        shinyjs::hide(id = "p2.gr", anim = TRUE) 
      
      if (data["person3.gr"] != "" && data["person3.gr"] != "N/A" )
        shinyjs::show(id = "p3.gr", anim = TRUE)
      else
        shinyjs::hide(id = "p3.gr", anim = TRUE) 
      
      if (data["person4.gr"] != "" && data["person4.gr"] != "N/A" )
        shinyjs::show(id = "p4.gr", anim = TRUE)
      else
        shinyjs::hide(id = "p4.gr", anim = TRUE) 
      
      if (data["person5.gr"] != "" && data["person5.gr"] != "N/A" )
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
    
    #values$df.summary.person  <- GetSummaryByPerson()
    #values$df.summary.parent  <- GetSummaryByParent()
    #values$parent.costs <- sum(GetSummaryByParent()$cost)
    #values$prof.costs <- sum(GetSummaryByPerson()$cost) - sum(GetSummaryByParent()$cost)
    
    UpdateSummaryByPersonAndParent()
    
    
    # Hide/Show the Download button
    if (nrow(ReadDataGroupTreatment()) > 0) { 
      shinyjs::show("download.table.gr.treatment")
    } else {
      shinyjs::hide("download.table.gr.treatment")
    }
    
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
    
    # Update Summary
    
    values$med.costs <- GetTotalCostsMedication()
    values$df.summary.med  <- GetSummaryByMedication()
    
    # Hide/show the Download Button
    
    if (nrow(ReadDataMedication()) > 0) { 
      shinyjs::show("download.table.med")
    } else {
      shinyjs::hide("download.table.med")
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
    
    # Updata Summary
    
    values$med.costs <- GetTotalCostsMedication()
    values$df.summary.med  <- GetSummaryByMedication()
    
    # Hide/show the Download Button
    
    if (nrow(ReadDataMedication()) > 0) { 
      shinyjs::show("download.table.med")
    } else {
      shinyjs::hide("download.table.med")
    }
    
    
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
  )
   
  #############################################################
  ##########      Download Data     ###########################
  #############################################################
  
  # disable the download buttons if the dataset is empty
  # it doesn't work for now, need to use some reactive values 
  observe({
    if (!is.null(ReadDataIndivTreatment()) && nrow(ReadDataIndivTreatment()) > 0) { 
      shinyjs::show("download.table.ind.treatment")
     } else {
      shinyjs::hide("download.table.ind.treatment")
     }
    if (!is.null(ReadDataGroupTreatment()) && nrow(ReadDataGroupTreatment()) > 0) { 
      shinyjs::show("download.table.gr.treatment")
    } else {
      shinyjs::hide("download.table.gr.treatment")
    }
    if (!is.null(ReadDataMedication()) && nrow(ReadDataMedication()) > 0) { 
      shinyjs::show("download.table.med")
    } else {
      shinyjs::hide("download.table.med")
    }
    
  })
  
  
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
  
  
  # Display the summary tables
  
  output$summary_1 <- renderTable({
    
    OutputSummary(GetSummary1(values$med.costs+values$prof.costs, values$parent.costs),  c("", ""))
    
    
    #df.summary1 <- as.data.frame(GetSummary1(values$med.costs+values$prof.costs, values$parent.costs))
    #df.summary1$costs <- comma(df.summary1$costs)
    #df.summary1$costs <- paste0("$", df.summary1$costs)
    #df.summary1
  },
  include.colnames=FALSE,
  include.rownames=FALSE)
  
  # Summary 2
  output$summary_2 <- renderTable({
    
    OutputSummary(GetSummary2(values$med.costs, values$prof.costs, values$parent.costs),  c("", ""))
    
    #df.summary2 <-as.data.frame(GetSummary2(values$med.costs, values$prof.costs, values$parent.costs)) 
    #df.summary2$costs <- comma(df.summary2$costs)
    #df.summary2$costs <- paste0("$", df.summary2$costs)
    #df.summary2
    
  },
  include.colnames=FALSE,
  include.rownames=FALSE)
  
  # Individual Components List
  output$summary_3 <- renderTable({
    
    OutputSummary(values$df.summary.ind,  c("By Individual Component", "Cost"))
    
    #df.summary.ind <- as.data.frame(values$df.summary.ind)
    #df.summary.ind$cost <- comma(df.summary.ind$cost)
    #df.summary.ind$cost <- paste0("$", df.summary.ind$cost)
    #names(df.summary.ind) = c("By Individual Component", "Cost")
    #df.summary.ind
    
  },
  include.rownames=FALSE)
  
  # Group Components List
  
  output$summary_4 <- renderTable({
    
    OutputSummary(values$df.summary.gr,  c("By Group Component", "Cost"))
    
    #df.summary.gr <- as.data.frame(values$df.summary.gr)
    #df.summary.gr$cost <- comma(df.summary.gr$cost)
    #df.summary.gr$cost <- paste0("$", df.summary.gr$cost)
    #names(df.summary.gr) = c("By Group Component", "Cost")
    #df.summary.gr
    
  },
  include.rownames=FALSE)
  
  # Persons List
  
  output$summary_5 <- renderTable({
    
    OutputSummary(values$df.summary.person,  c("By Person", "Cost"))
    
  #  df.persons <- as.data.frame(values$df.summary.person)
  #  df.persons$cost <- comma(df.persons$cost)
  #  df.persons$cost <- paste0("$", df.persons$cost)
  #  names(df.persons) = c("By Person", "Cost")
  #  df.persons
  }
  , include.rownames=FALSE)
  
  # Medication List
  
  output$summary_6 <- renderTable({
    #df.summary.med <- as.data.frame(values$df.summary.med)
    OutputSummary(values$df.summary.med,  c("By Medication", "Cost"))
    #df.summary.med$cost <- comma(df.summary.med$cost)
    #df.summary.med$cost <- paste0("$", df.summary.med$cost)
    #names(df.summary.med) = c("By Medication", "Cost")
    #df.summary.med
  }
  , include.rownames=FALSE)
  
  
  # Download the report
  
  output$download.report <- downloadHandler(
    filename = function() {
      paste("ADHD-CostCalculatorReport", "_", Sys.Date(), sep = ""
            , switch(input$report.format, PDF = '.pdf', HTML = '.html', Word = '.docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$report.format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  
  
})

