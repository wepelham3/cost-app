#"graphing etc. will go here"

tabsetPanel(id = "protocol.comparison",
            

    tabPanel("Select protocols",
             fluidRow(
               column(4, offset= 0.5,
                 br(),
                 # download Panel
                 wellPanel(
                   
                   shinyjs::hidden(
                     div(id = "empty.protocol1",
                         p("You need first to save the current protocol and then upload it", style = "color: red")
                     )  
                   ),
                   
                   fluidRow(
                     
                     column(8, offset = 0.5,
                        radioButtons("upload.options.prt1", "Upload from:",
                                    choices  =  c("Just Saved Protocol" = "upload.just.saved",
                                                   "RDS External File" = "upload.from.rds"), selected = 'upload.just.saved'),
                        
                        
                        shinyjs::hidden(
                          div(id = "RDSfile1",
                   
                              fileInput('file.protocol1', 'Choose protocol file to upload', accept ='.rds')
                          )
                       )  
                   
                      
                      ),# close column=2
                    
                      column(4, offset = 0.5,
                   
                       actionButton(inputId = "upload.protocol1", label = "Upload", icon = icon("upload"))
                       
                     
                    )# close column       
                   
                   ) #close fluidRow 
                 ), # close wellPanel
                
                 wellPanel(
                   
                   h4(tags$b(textOutput("protocol1.name")),  style = "color: #4177b7"),
                   
                   #h4(tags$b("Total Costs"), style = "color: #4177b7"),
                 
                   tableOutput('protocol1.summary_1'),
                   tags$style(type="text/css", "#protocol1.summary_1 table {background-color: #CBA135}", media="screen"),
                   tags$style(type="text/css", "#protocol1.summary_1 tr:last-child {font-weight:bold; color: white}"),
                   
                 
                   tableOutput('protocol1.summary_2'),
                   tags$head(tags$style("#protocol1.summary_2 table {background-color: #CBA135}", media="screen", type="text/css"))
               
                  ),
                  wellPanel(
                    tableOutput('protocol1.summary_3'),
                    
                    tableOutput('protocol1.summary_4')
                    
                  ),
                 wellPanel(
                 
                   tableOutput('protocol1.summary_5'),
                 
                   tableOutput('protocol1.summary_6')
                 
                 )  
               
               
                ),   
               
               #######  Upload the Protocol 2 #################################################
               column(4, offset= 0.5,
                      br(),
                      # download Panel
                      wellPanel(
                        
                        shinyjs::hidden(
                          div(id = "empty.protocol2",
                              p("You need first to save the current protocol and then upload it", style = "color: red")
                          )  
                        ),
                        
                        fluidRow(
                          
                          column(8, offset = 0.5,
                                 radioButtons("upload.options.prt2", "Upload from:",
                                              choices  =  c("Just Saved Protocol" = "upload.just.saved",
                                                            "RDS External File" = "upload.from.rds"), selected = 'upload.just.saved'),
                                 
                                 
                                 shinyjs::hidden(
                                   div(id = "RDSfile2",
                                       
                                       fileInput('file.protocol2', 'Choose protocol file to upload', accept ='.rds')
                                   )
                                 )  
                                 
                                 
                          ),# close column=2
                          
                          column(4, offset = 0.5,
                                 
                                 actionButton(inputId = "upload.protocol2", label = "Upload", icon = icon("upload"))
                                 
                                 
                          )# close column       
                          
                        ) #close fluidRow 
                      ), # close wellPanel
                      
                      wellPanel(
                        
                        h4(tags$b(textOutput("protocol2.name")),  style = "color: #4177b7"),
                       # h4(tags$b("Total Costs"), style = "color: #4177b7"),
                    
                        tableOutput('protocol2.summary_1'),
                        tags$style(type="text/css", "#protocol2.summary_1 table {background-color: #CBA135}", media="screen"),
                        tags$style(type="text/css", "#protocol2.summary_1 tr:last-child {font-weight:bold; color: white}"),
                      
                    
                        tableOutput('protocol2.summary_2'),
                        tags$head(tags$style("#protocol2.summary_2 table {background-color: #CBA135}", media="screen", type="text/css"))
                    
                      ),
                    wellPanel(
                       tableOutput('protocol2.summary_3'),
                    
                       tableOutput('protocol2.summary_4')
                    
                     ),
                    wellPanel(
                    
                       tableOutput('protocol2.summary_5'),
                    
                       tableOutput('protocol2.summary_6')
                    
                    )  
                  
                  
               ),    
               
               #######  Upload the Protocol 3 #################################################
              
               column(4, offset= 0.5,
                 br(),  
                 
                 wellPanel(
                   
                   shinyjs::hidden(
                     div(id = "empty.protocol3",
                         p("You need first to save the current protocol and then upload it", style = "color: red")
                     )  
                   ),
                   
                   fluidRow(
                     
                     column(8, offset = 0.5,
                            radioButtons("upload.options.prt3", "Upload from:",
                                         choices  =  c("Just Saved Protocol" = "upload.just.saved",
                                                       "RDS External File" = "upload.from.rds"), selected = 'upload.just.saved'),
                            
                            
                            shinyjs::hidden(
                              div(id = "RDSfile3",
                                  
                                  fileInput('file.protocol3', 'Choose protocol file to upload', accept ='.rds')
                              )
                            )  
                            
                            
                     ),# close column=2
                     
                     column(4, offset = 0.5,
                            
                            actionButton(inputId = "upload.protocol3", label = "Upload", icon = icon("upload"))
                            
                            
                     )# close column       
                     
                   ) #close fluidRow 
                 ), # close wellPanel
                 
                 wellPanel(
                   
                   h4(tags$b(textOutput("protocol3.name")),  style = "color: #4177b7"),
                 
                   tableOutput('protocol3.summary_1'),
                # tags$style(type="text/css", "#protocol3.summary_1 table {background-color: #CBA135}", media="screen"),
                # tags$style(type="text/css", "#protocol3.summary_1 tr:last-child {font-weight:bold; color: white}"),
                 
                 
                  tableOutput('protocol3.summary_2')
                # tags$head(tags$style("#protocol3.summary_2 table {background-color: #CBA135}", media="screen", type="text/css"))
                 
               ),
               wellPanel(
                 tableOutput('protocol3.summary_3'),
                 
                 tableOutput('protocol3.summary_4')
                 
               ),
               wellPanel(
                 
                 tableOutput('protocol3.summary_5'),
                 
                 tableOutput('protocol3.summary_6')
                 
               )  
               
               
             ) 
       ) # close fluidRow     
             
    ), # close tabPanel         
                  
    tabPanel("Plots",
             fluidRow(
               column(4, offset= 0.5,
                      br(),
                      wellPanel(
                        plotOutput("protocols.bars1")
                      )
               ),
               column(4, offset= 0.5,
                      br(),
                      wellPanel(
                        
                        plotOutput("protocols.bars2")
                      )
               ),
               column(4, offset= 0.5,
                      br(),
                      wellPanel(
                        
                        plotOutput("protocols.bars3")
                      )
               )
               
             )
             
    )
)
