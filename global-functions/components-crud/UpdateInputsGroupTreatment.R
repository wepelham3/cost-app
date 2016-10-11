# Fill the input fields with the values of the selected record in the table
UpdateInputsGroupTreatment <- function(data, session, num.persons.gr = 5) {
  updateTextInput(session, "id.gr", value = unname(rownames(data)))
  updateTextInput(session, "cost.gr", value = CalculateCostGroupTreatment(data, num.persons.gr))
  updateTextInput(session, "label.gr", value = unname(data["label.gr"]))
  updateSliderInput(session, "frequency.gr", value = as.integer(data["frequency.gr"]))
  updateTextInput(session, "duration.gr", value = as.integer(data["duration.gr"]))
  updateTextInput(session, "num.families.gr", value = as.integer(data["num.families.gr"]))
  
  #  updateTextInput(session, "num.persons.ind", value = as.integer(data["num.persons.ind"]))
  
  for (i in 1:num.persons.gr){ 
    updateSelectInput(session
                      , inputId = paste0("person", i, ".gr")
                      #  , label =  paste0("Person ", i)
                      #  , choices = df.comps$person
                      , selected = data[paste0("person", i, ".gr")])
    
  }
  
  #updateSelectInput(session, "person1.ind", label = "Person1", selected = data["person1.ind"])
  #updateSelectInput(session, "person2.ind", label = "Person2", selected = data["person2.ind"])
  #updateSelectInput(session, "person3.ind", label = "Person3", selected = data["person3.ind"])
  #updateSelectInput(session, "person4.ind", label = "Person4", selected = data["person4.ind"])
  #updateSelectInput(session, "person5.ind", label = "Person5", selected = data["person5.ind"])
  
  for (i in 1:num.persons.gr) {
    
    updateTextInput(session, paste0("p", i, ".comm.gr"), value = as.integer(data[paste0("p", i, ".comm.gr")])) 
    
    #updateRadioButtons(session 
    #                  , paste0("p", i, ".ind.yn")
    #                   #  , label = tags$h5(paste0("Is the person ", i, " commuting to the session?"))
    #                   #  , choices = c("Yes", "No")
    #                   , selected = data[paste0("p", i, ".ind.yn")]           
    #)
  } # close for                    
  
}