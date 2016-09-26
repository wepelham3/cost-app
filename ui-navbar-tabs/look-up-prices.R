tabsetPanel(
  tabPanel("Prices of medications",
           style = "color: #4177b7;",
           DT::dataTableOutput("DT.lookup.meds")),
  tabPanel("Wages of professionals",
           style = "color: #4177b7;",
           DT::dataTableOutput("DT.lookup.comps"))
)