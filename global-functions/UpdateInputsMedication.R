# Fill the input fields with the values of the selected record in the table
UpdateInputsMedication <- function(data, session) {
  
  updateTextInput(session, "id.med", value = unname(rownames(data)))
  updateTextInput(session, "cost.med", value = CalculateCostMedication(data))
  updateTextInput(session, "label.med", value = unname(data["label.med"]))
  updateSliderInput(session, "frequency.med", value = as.integer(data["frequency.med"]))
  updateTextInput(session, "week.med", value = unname(data["week.med"]))
  updateTextInput(session, "year.med", value = unname(data["year.med"]))
  
}