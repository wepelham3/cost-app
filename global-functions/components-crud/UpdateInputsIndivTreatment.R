# Fill the input fields with the values of the selected record in the table
UpdateInputsIndivTreatment <- function(data, session, num.persons.ind = 5) {
  updateTextInput(session, "id.ind", value = unname(rownames(data)))
  updateTextInput(session, "cost.ind", value = CalculateCostIndivTreatment(data, num.persons.ind))
  updateTextInput(session, "label.ind", value = unname(data["label.ind"]))
  updateSliderInput(session, "frequency.ind", value = as.integer(data["frequency.ind"]))
  updateTextInput(session, "duration.ind", value = as.integer(data["duration.ind"]))
  
  #  updateTextInput(session, "num.persons.ind", value = as.integer(data["num.persons.ind"]))
  
  for (i in 1:num.persons.ind){ 
    updateSelectInput(session
                      , inputId = paste0("person", i, ".ind")
                      #  , label =  paste0("Person ", i)
                      #  , choices = df.comps$person
                      , selected = data[paste0("person", i, ".ind")])
    
  }
  
  
  for (i in 1:num.persons.ind) {
    
    updateTextInput(session, paste0("p", i, ".comm.ind"), value = as.integer(data[paste0("p", i, ".comm.ind")])) 
    
    
  } # close for                    
  
}