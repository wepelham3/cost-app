library(shiny)
library(magrittr)
library(dplyr)
library(readr)
library(DT)
library(shinyjs)
library(scales)
library(ggplot2)


shinyServer(function(input, output, session) {
  
  #use shiny js 
  shinyjs::useShinyjs()
  
  
  # uncomment for debugging: send all Input Values to the interface
  
  #  output$inputvals<-renderTable({
  #    as.data.frame(reactiveValuesToList(input))
  #  })
  
  
  
  #add values to be used in the reactive environment 
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
  
  #####################################################################################################
  # Uncommment for debugging 
  #  output$summary.by.person <- renderTable({
  #      as.data.frame(GetSummaryByPerson())
  #  })
  ###################################################################################################
  
  
  UpdateSummaryByPersonAndParent()
  

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
  
 
  ###################################################################################################
  # CRUD for Individual Treatment Components
  #
  ###################################################################################################
 
  
  # Get the current inputs with names defined in the TableMetadata for Individual Treatments
  
  formDataIndivTreatment <- reactive({
    sapply(names(GetTableMetadataIndivTreatment()$fields), function(x) input[[x]])
  })
  
  
 #####################################################################################################
 # Uncommment for debugging 
 #   output$mytable.ind.1 <- renderTable({
 #      as.data.frame(formDataIndivTreatment())
 #   })
 #
 #   output$mytable.ind.2<-renderTable({ 
 #      as.data.frame(ReadDataIndivTreatment()[input$data.table.ind.treatment_rows_selected, ])
 #   })
 ####################################################################################################
  
  
  # Mandatory fields for Individual Treatments
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
  
 
  ###################################################################################################
  # CRUD for Group Treatment Components
  #
  ###################################################################################################
  
  # Get the current inputs with names defined in the TableMetadata for Group Treatments
  
  formDataGroupTreatment <- reactive({
    sapply(names(GetTableMetadataGroupTreatment()$fields), function(x) input[[x]])
  })
  
  #####################################################################################################
  # Uncommment for debugging 
  #  output$mytable.gr.1 <- renderTable({
  #      as.data.frame(formDataGroupTreatment())
  #  })
  #
  #  output$mytable.gr.2 <- renderTable({ 
  #     as.data.frame(ReadDataGroupTreatment()[input$data.table.gr.treatment_rows_selected, ])
  #  })
  #  
  #
  # values$df.gr.treatment = ReadDataGroupTreatment()
  #  
  #  output$mytable.gr.3 <- renderTable({ 
  #    as.data.frame(values$df.gr.treatment)
  #  })
    
    
  ####################################################################################################
  
  # Mandatory fields for Individual Treatments
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
 
  
  ###################################################################################################
  # CRUD for Medications
  #
  ###################################################################################################

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
  
  ##################################################################################################
  # Render DataTables                                                                              #
  ##################################################################################################
  
  # Render the DataTable for the Individual Treatment data.frame
  output$data.table.ind.treatment <- DT::renderDataTable({
    #update after submit is clicked
    input$submit.ind
    #update after delete is clicked
    input$delete.ind
    if (!is.null(ReadDataIndivTreatment())) {
      ReadDataIndivTreatment() %>%
        mutate(cost.ind = paste0("$", sprintf("%.0f", round(cost.ind, 0)))) %>%
        select(cost.ind, label.ind, frequency.ind, duration.ind,
               person1.ind, person2.ind, person3.ind, person4.ind, person5.ind)
    }
  }, server = FALSE, selection = "single", 
  colnames = c("Cost", "Label", "Frequency", "Duration (min)",
               "Person 1", "Person 2", "Person 3", "Person 4", "Person 5"),
   options = list(sDom  = '<"top">rt<"bottom">ip)',
                                     autoWidth = TRUE
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
        mutate(cost.gr = paste0("$", sprintf("%.0f", round(cost.gr, 0)))) %>%
        select(cost.gr, label.gr, frequency.gr, duration.gr, num.families.gr,
               person1.gr, person2.gr, person3.gr, person4.gr, person5.gr)
    }
  }, server = FALSE, selection = "single", 
  colnames = c("Cost", "Label", "Frequency", "Duration (min)", "Num. Families",
               "Person 1", "Person 2", "Person 3", "Person 4", "Person 5"),
  options = list(sDom  = '<"top">rt<"bottom">ip)') # to suppress search box
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
   
  ##################################################################################################
  # Download Data                                                                                  #
  ##################################################################################################
  
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
  
  
  ##################################################################################################
  # Render DataTables for med prices and person wages and compensations                            #
  ##################################################################################################

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
      filter(! person == "N/A") %>%
      mutate(bls.label.and.link = ifelse(person == "Paraprofessional",
                                         "",
                                         paste0("<a href='", link.source.of.wage, "'>", full.label.bls, "</a>"))) %>%
      select(person, bls.label.and.link, mean.hourly.wage, mean.hourly.compensation) %>%
      datatable(colnames = c("Person", "Bureau of Labor Statistics Code (click link to view)", "Hourly Wage", "Hourly Compensation"),
                rownames = FALSE,
                options = list(dom = "ft",
                               pageLength = 50),
                escape = FALSE) %>%
      formatCurrency(c("mean.hourly.wage", "mean.hourly.compensation"), digits = 2)
    })
  
  
  ##################################################################################################
  # Display the summary tables                                                                     #
  ##################################################################################################
  
  output$summary_1 <- renderTable({
    
    OutputSummary(GetSummary1(values$med.costs+values$prof.costs, values$parent.costs),  c("", ""))
    
    },
    include.colnames=FALSE,
    include.rownames=FALSE
  
  )
  
  # Summary 2
  output$summary_2 <- renderTable({
    
    OutputSummary(GetSummary2(values$med.costs, values$prof.costs, values$parent.costs),  c("", ""))
    
    },
    include.colnames=FALSE,
    include.rownames=FALSE
  
  )
  
  # Individual Components List
  output$summary_3 <- renderTable({
    
    OutputSummary(values$df.summary.ind,  c("By Individual Component", "Cost"))
    
    }
    , include.rownames=FALSE
  
  )
  
  # Group Components List
  
  output$summary_4 <- renderTable({
    
    OutputSummary(values$df.summary.gr,  c("By Group Component", "Cost"))
  
    }
  
    , include.rownames=FALSE
  )
  
  # Persons List
  
  output$summary_5 <- renderTable({
    
    OutputSummary(values$df.summary.person,  c("By Person", "Cost"))
 
    }
    
    , include.rownames=FALSE
  )
  
  # Medication List
  
  output$summary_6 <- renderTable({
   
      OutputSummary(values$df.summary.med,  c("By Medication", "Cost"))
   
     }
    , include.rownames=FALSE
 
  )
  
  ##################################################################################################
  # Download the protocol report                                                                   #
  ##################################################################################################
  
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
  
  ##################################################################################################
  # Save the protocol to an internal data object list for future comparison                                                       
  ##################################################################################################
  
  observeEvent(input$save.protocol, {
    
    if (input$save.protocol != "0") {
  
      # Cast Protocol data
      values$protocol.data <- CastProtocolData(input$protocol.name
                                        , values$med.costs+values$prof.costs, values$parent.costs
                                        , values$med.costs, values$prof.costs, values$parent.costs)
      
      print(values$protocol.data)
      
      
      #updateNavbarPage(id="main.nav.bar", selected = "Compare protocols") #it doesn't work
      
    }
    
    
  })
  
  
  
  ##################################################################################################
  # Save the protocol to an external RDS file                                                             
  ##################################################################################################
  
  output$save.protocol.RDS <- downloadHandler(
    
    filename = function() { 
      
      paste(input$protocol.name, '.rds', sep='') 
      
    },
    
    content = function(file) {
     
       print(file) ##prints: [...]\\Local\\Temp\\RtmpEBYDXT\\fileab8c003878.csv
      
       SaveProtocolToRDS(file, input$protocol.name
                        , values$med.costs+values$prof.costs, values$parent.costs
                        , values$med.costs, values$prof.costs, values$parent.costs)
       
  })
  
  observe({
  
 
    if (input$upload.options.prt1 == "upload.from.rds"){
       
       if (!is.null(input$file.protocol1))
         shinyjs::enable("upload.protocol1")
       else 
         shinyjs::disable("upload.protocol1")
     }   
     
    else # if (input$upload.options.prt2 == "upload.just.saved" )
 
      {
       
       if(length(values$protocol.data) > 0)
      
           shinyjs::enable("upload.protocol1")
    
       else 
        
         shinyjs::disable("upload.protocol1")
      }  
    
    
     
    if (input$upload.options.prt2 == "upload.from.rds"){
      
      if (!is.null(input$file.protocol2))
        shinyjs::enable("upload.protocol2")
      else 
        shinyjs::disable("upload.protocol2")
    }   
    
    else # if (input$upload.options.prt2 == "upload.just.saved" )
      
    {
      
      if(length(values$protocol.data) > 0)
        
        shinyjs::enable("upload.protocol2")
      
      else 
        
        shinyjs::disable("upload.protocol2")
    }
    
    
    if (input$upload.options.prt3 == "upload.from.rds"){
      
      if (!is.null(input$file.protocol3))
        shinyjs::enable("upload.protocol3")
      else 
        shinyjs::disable("upload.protocol3")
    }   
    
    else # if (input$upload.options.prt3 == "upload.just.saved" )
      
    {
      
      if(length(values$protocol.data) > 0)
        
        shinyjs::enable("upload.protocol3")
      
      else 
        
        shinyjs::disable("upload.protocol3")
    }
    
    
    
    
 })
  
  
  
  ##########################################################################################
  # Upload the protocol previously saved or from an RDS files for comparison to 
  # the 1th Protocol Panel for Comparison
  ##########################################################################################
  
  observeEvent(input$upload.options.prt1, {
               
   if(input$upload.options.prt1 == "upload.from.rds"){
   
     shinyjs::show(id = "RDSfile1")
     shinyjs::hide(id = "empty.protocol1")
     
   }   
   
    else { # input$upload.options.prt1 == "upload.just.saved"
    
     shinyjs::hide(id = "RDSfile1")
     
     if(length(values$protocol.data) == 0){
       
         print("The list is empty")
         shinyjs::show(id = "empty.protocol1")
        
      } 
      
      else{
          
         shinyjs::hide(id = "empty.protocol1")
        
     }
   
    }  
    
    print(input$upload.options.prt1)
    
  })    
  

  
  observeEvent(input$upload.protocol1, {
    
    if (input$upload.protocol1 != "0") {
      
      # upload from the just-saved protocol or from an external file
      
      if(input$upload.options.prt1 == "upload.just.saved")
        
        values$protocol1.data <- values$protocol.data 
      
      else # input$upload.options.prt1 = "upload.from.rds"
        
      {
        
        if (!is.null(input$file.protocol1)){
          
          values$protocol1.data <- UploadProtocolFromRDSToList(input$file.protocol1)
          shinyjs::hide(id = "RDSfile1")
        }
        
      } # close else
      
      
      if(length(values$protocol1.data) == 0){
        
        print("The list is empty")
        
        shinyjs::show(id = "empty.protocol1")
        
        return()
        
      }
      
      
      shinyjs::hide(id = "empty.protocol1")  
      
      # Protocol Name
      
      output$protocol1.name <- renderText({
        
        paste0("Protocol Name: ", as.character(values$protocol1.data$protocol.name))
        
      })
      
      # Summary 1
      
      output$protocol1.summary_1 <- renderTable({
        
        OutputSummary(GetSummary1(as.numeric(values$protocol1.data$total.explicit.cost)
                                  , as.numeric(values$protocol1.data$total.implicit.cost)),  c("", ""))
        
      },
      include.colnames=FALSE,
      include.rownames=FALSE
      
      )
      
      # Summary 2
      output$protocol1.summary_2 <- renderTable({
        
        OutputSummary(GetSummary2(as.numeric(values$protocol1.data$total.cost.medications)
                                  , as.numeric(values$protocol1.data$total.cost.professional.time)
                                  , as.numeric(values$protocol1.data$total.cost.parent.time )),  c("", ""))
        
      },
      include.colnames=FALSE,
      include.rownames=FALSE
      
      )
      
      
      # Individual Components List
      output$protocol1.summary_3 <- renderTable({
        
        OutputSummary(values$protocol1.data$ind.component.summary,  c("By Individual Component", "Cost"))
        
      }
      , include.rownames=FALSE
      
      )
      
      # Group Components List
      
      output$protocol1.summary_4 <- renderTable({
        
        OutputSummary(values$protocol1.data$grp.component.summary,  c("By Group Component", "Cost"))
        
      }
      
      , include.rownames=FALSE
      )
      
      # Persons List
      
      output$protocol1.summary_5 <- renderTable({
        
        OutputSummary(values$protocol1.data$person.summary,  c("By Person", "Cost"))
        
      }
      
      , include.rownames=FALSE
      
      )
      
      # Medication List
      
      output$protocol1.summary_6 <- renderTable({
        
        OutputSummary(values$protocol1.data$med.component.summary,  c("By Medication", "Cost"))
        
      }
      , include.rownames=FALSE
      
      )
      
    }# close input.upload.protocol1
    
  }) # close observe if(input$upload.options.prt1 = "upload.just.saved")
  
  ##########################################################################################
  # Upload the protocol previously saved or from an RDS files for comparison to 
  # the 2-nd Protocol Panel for Comparison
  ##########################################################################################
  
  observeEvent(input$upload.options.prt2, {
    
    if(input$upload.options.prt2 == "upload.from.rds"){
      
      shinyjs::show(id = "RDSfile2")
      shinyjs::hide(id = "empty.protocol2")
      
    }   
    
    else { # input$upload.options.prt1 == "upload.just.saved"
      
      shinyjs::hide(id = "RDSfile2")
      
      if(length(values$protocol.data) == 0){
        
        print("The list is empty")
        shinyjs::show(id = "empty.protocol2")
        
      } 
      
      else{
        
        shinyjs::hide(id = "empty.protocol2")
        
      }
      
    }  
    
    print(input$upload.options.prt2)
    
    
  })    
  
  
  
  observeEvent(input$upload.protocol2, {
    
    if (input$upload.protocol2 != "0") {
      
      # upload from the just-saved protocol or from an external file
      
      if(input$upload.options.prt2 == "upload.just.saved")
        
        values$protocol2.data <- values$protocol.data 
      
      else # input$upload.options.prt1 = "upload.from.rds"
        
      {
        
        if (!is.null(input$file.protocol2)){
          
          values$protocol2.data <- UploadProtocolFromRDSToList(input$file.protocol2)
          shinyjs::hide(id = "RDSfile2")
          
        }
        
      } # close else
      
      
      if(length(values$protocol2.data) == 0){
        
        print("The list is empty")
        
        shinyjs::show(id = "empty.protocol2")
        
        return()
        
      }
      
      
      shinyjs::hide(id = "empty.protocol2")  
      
      # Protocol Name
      
      output$protocol2.name <- renderText({
        
        paste0("Protocol Name: ", as.character(values$protocol2.data$protocol.name))
        
      })
      
      # Summary 1
      
      output$protocol2.summary_1 <- renderTable({
        
        OutputSummary(GetSummary1(as.numeric(values$protocol2.data$total.explicit.cost)
                                  , as.numeric(values$protocol2.data$total.implicit.cost)),  c("", ""))
        
      },
      include.colnames=FALSE,
      include.rownames=FALSE
      
      )
      
      # Summary 2
      output$protocol2.summary_2 <- renderTable({
        
        OutputSummary(GetSummary2(as.numeric(values$protocol2.data$total.cost.medications)
                                  , as.numeric(values$protocol2.data$total.cost.professional.time)
                                  , as.numeric(values$protocol2.data$total.cost.parent.time )),  c("", ""))
        
      },
      include.colnames=FALSE,
      include.rownames=FALSE
      
      )
      
      
      # Individual Components List
      output$protocol2.summary_3 <- renderTable({
        
        OutputSummary(values$protocol2.data$ind.component.summary,  c("By Individual Component", "Cost"))
        
      }
      , include.rownames=FALSE
      
      )
      
      # Group Components List
      
      output$protocol2.summary_4 <- renderTable({
        
        OutputSummary(values$protocol2.data$grp.component.summary,  c("By Group Component", "Cost"))
        
      }
      
      , include.rownames=FALSE
      )
      
      # Persons List
      
      output$protocol2.summary_5 <- renderTable({
        
        OutputSummary(values$protocol2.data$person.summary,  c("By Person", "Cost"))
        
      }
      
      , include.rownames=FALSE
      
      )
      
      # Medication List
      
      output$protocol2.summary_6 <- renderTable({
        
        OutputSummary(values$protocol2.data$med.component.summary,  c("By Medication", "Cost"))
        
      }
      , include.rownames=FALSE
      
      )
      
    }# close input.upload.protocol2
    
  }) # close observe if(input$upload.options.prt2 = "upload.just.saved")
  
  
  ##########################################################################################
  # Upload the protocol previously saved or from an RDS files for comparison to 
  # the 3-rd Protocol Panel for Comparison
  ##########################################################################################
  
  observeEvent(input$upload.options.prt3, {
    
    if(input$upload.options.prt3 == "upload.from.rds"){
      
      shinyjs::show(id = "RDSfile3")
      shinyjs::hide(id = "empty.protocol3")
      
    }   
    
    else { # input$upload.options.prt1 == "upload.just.saved"
      
      shinyjs::hide(id = "RDSfile3")
      
      if(length(values$protocol.data) == 0){
        
        print("The list is empty")
        shinyjs::show(id = "empty.protocol3")
        
      } 
      
      else{
        
        shinyjs::hide(id = "empty.protocol3")
        
      }
      
    }  
    
    print(input$upload.options.prt3)
    
    
  })    
  
  
  
  observeEvent(input$upload.protocol3, {
    
    if (input$upload.protocol3 != "0") {
      
      # upload from the just-saved protocol or from an external file
      
      if(input$upload.options.prt3 == "upload.just.saved")
        
        values$protocol3.data <- values$protocol.data 
      
      else # input$upload.options.prt1 = "upload.from.rds"
        
      {
        
        if (!is.null(input$file.protocol3)){
          
          values$protocol3.data <- UploadProtocolFromRDSToList(input$file.protocol3)
          shinyjs::hide(id = "RDSfile3")
          
        }
        
      } # close else
      
      
      if(length(values$protocol3.data) == 0){
        
        print("The list is empty")
        
        shinyjs::show(id = "empty.protocol3")
        
        return()
        
      }
      
      
      shinyjs::hide(id = "empty.protocol3")  
      
      # Protocol Name
      
      output$protocol3.name <- renderText({
        
        paste0("Protocol Name: ", as.character(values$protocol3.data$protocol.name))
        
      })
      
      # Summary 1
      
      output$protocol3.summary_1 <- renderTable({
        
        OutputSummary(GetSummary1(as.numeric(values$protocol3.data$total.explicit.cost)
                                  , as.numeric(values$protocol3.data$total.implicit.cost)),  c("", ""))
        
      },
      include.colnames=FALSE,
      include.rownames=FALSE
      
      )
      
      # Summary 2
      output$protocol3.summary_2 <- renderTable({
        
        OutputSummary(GetSummary2(as.numeric(values$protocol3.data$total.cost.medications)
                                  , as.numeric(values$protocol3.data$total.cost.professional.time)
                                  , as.numeric(values$protocol3.data$total.cost.parent.time )),  c("", ""))
        
      },
      include.colnames=FALSE,
      include.rownames=FALSE
      
      )
      
      
      # Individual Components List
      output$protocol3.summary_3 <- renderTable({
        
        OutputSummary(values$protocol3.data$ind.component.summary,  c("By Individual Component", "Cost"))
        
      }
      , include.rownames=FALSE
      
      )
      
      # Group Components List
      
      output$protocol3.summary_4 <- renderTable({
        
        OutputSummary(values$protocol3.data$grp.component.summary,  c("By Group Component", "Cost"))
        
      }
      
      , include.rownames=FALSE
      )
      
      # Persons List
      
      output$protocol3.summary_5 <- renderTable({
        
        OutputSummary(values$protocol3.data$person.summary,  c("By Person", "Cost"))
        
      }
      
      , include.rownames=FALSE
      
      )
      
      # Medication List
      
      output$protocol3.summary_6 <- renderTable({
        
        OutputSummary(values$protocol3.data$med.component.summary,  c("By Medication", "Cost"))
        
      }
      , include.rownames=FALSE
      
      )
      
    }# close input.upload.protocol3
    
  }) # close observe if(input$upload.options.prt3 = "upload.just.saved")
  
  #############################################
  # Plots for Protocol Comparison
  ###############################################
  
  observe({
    
    cost.cat1 = c("Total Explicit Cost","Total Implicit Cost")
    cost.cat2 = c("Cost Medications","Cost Professional Time", "Costs Parent Time")
    
    protocol.names <- ifelse(length(values$protocol1.data) > 0, values$protocol1.data$protocol.name, "Protocol 1")
    protocol.names <- cbind(protocol.names, ifelse(length(values$protocol2.data) > 0 , values$protocol2.data$protocol.name, "Protocol 2"))
    protocol.names <- cbind(protocol.names, ifelse(length(values$protocol3.data) > 0 , values$protocol3.data$protocol.name, "Protocol 3"))
    
    explicit.costs <- ifelse(length(values$protocol1.data) > 0, values$protocol1.data$total.explicit.cost, 0)
    explicit.costs <- cbind(explicit.costs, ifelse(length(values$protocol2.data) > 0 , values$protocol2.data$total.explicit.cost, 0))
    explicit.costs <- cbind(explicit.costs, ifelse(length(values$protocol3.data) > 0 , values$protocol3.data$total.explicit.cost, 0))
    
    implicit.costs <- ifelse(length(values$protocol1.data) > 0, values$protocol1.data$total.implicit.cost, 0)
    implicit.costs <- cbind(implicit.costs, ifelse(length(values$protocol2.data) > 0 , values$protocol2.data$total.implicit.cost, 0))
    implicit.costs <- cbind(implicit.costs, ifelse(length(values$protocol3.data) > 0 , values$protocol3.data$total.implicit.cost, 0))
    
    medication.costs <- ifelse(length(values$protocol1.data) > 0, values$protocol1.data$total.cost.medications, 0)
    medication.costs <- cbind(medication.costs, ifelse(length(values$protocol2.data) > 0 , values$protocol2.data$total.cost.medications, 0))
    medication.costs <- cbind(medication.costs, ifelse(length(values$protocol3.data) > 0 , values$protocol3.data$total.cost.medications, 0))
    
    professional.costs <- ifelse(length(values$protocol1.data) > 0, values$protocol1.data$total.cost.professional.time, 0)
    professional.costs <- cbind(professional.costs, ifelse(length(values$protocol2.data) > 0 , values$protocol2.data$total.cost.professional.time, 0))
    professional.costs <- cbind(professional.costs, ifelse(length(values$protocol3.data) > 0 , values$protocol3.data$total.cost.professional.time, 0))
    
    parent.costs <- ifelse(length(values$protocol1.data) > 0, values$protocol1.data$total.cost.parent.time, 0)
    parent.costs <- cbind(parent.costs, ifelse(length(values$protocol2.data) > 0 , values$protocol2.data$total.cost.parent.time, 0))
    parent.costs <- cbind(parent.costs, ifelse(length(values$protocol3.data) > 0 , values$protocol3.data$total.cost.parent.time, 0))
    
    print(protocol.names) 
    print(explicit.costs)
    print(implicit.costs)
    
    
    output$protocols.bars1 <- renderPlot({
      
      
      bar.data <- data.frame(
        protocol = factor(c(rep(protocol.names[1] , 2), rep(protocol.names[2] , 2), rep(protocol.names[3] , 2))
                          , levels=protocol.names),
        cost.type = factor(rep(cost.cat1, 3),  levels = cost.cat1),
        total.cost = c(explicit.costs[1], implicit.costs[1]
                       , explicit.costs[2], implicit.costs[2] 
                       , explicit.costs[3], implicit.costs[3])
      )
      
      
      print(bar.data)
      
      # Stacked bar graph -- simple
      stacked.bar <- ggplot(data=bar.data, aes(x=protocol, y=total.cost, fill=cost.type)) +
        geom_bar(stat="identity")
      
      
      print(stacked.bar)
    })
    
    output$protocols.bars2 <- renderPlot({
      
      
      bar.data <- data.frame(
        protocol = factor(c(rep(protocol.names[1] , 3), rep(protocol.names[2] , 3), rep(protocol.names[3] , 3))
                          , levels=protocol.names),
        cost.type = factor(rep(cost.cat2, 3),  levels = cost.cat2),
        total.cost = c(medication.costs[1], professional.costs[1], parent.costs[1]
                       , medication.costs[2], professional.costs[2], parent.costs[2]
                       , medication.costs[3], professional.costs[3], parent.costs[3])
      )
      
      
      print(bar.data)
      
      # Stacked bar graph -- simple
      stacked.bar <- ggplot(data=bar.data, aes(x=protocol, y=total.cost, fill=cost.type)) +
        geom_bar(stat="identity")
      
      
      print(stacked.bar)
    })
    
    
  })
  
  
  
})

