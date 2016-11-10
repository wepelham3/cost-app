#-----------------------------------------------------------------------------------
# CREATE, READ, UPDATE. DELETE (CRUD) Functions for Medications
# 
#-----------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Define the data.frame metadata (columns specification with labels)
# ------------------------------------------------------------------------------
GetTableMetadataMedication <- function() {
  fields <- c(  id.med = "Id" 
                , cost.med ="Cost"
                , label.med = "Label" 
                , frequency.med = "Freq/Year" 
                , week.med = "Weekly Schedule"
                , year.med = "Yearly Schedule"
  )
  
  
  result <- list(fields = fields)
  return (result)
}


#-----------------------------------------------------------------------------------------
# Generate the next ID of a new record
# ----------------------------------------------------------------------------------------
GetNextIdMedication <- function() {
  if (exists("df.med") && nrow(df.med) > 0) {
    max(as.integer(rownames(df.med))) + 1
  } else {
    return (1)
  }
}

#-----------------------------------------------------------------------------------------
# Cast Data from Inputs to a one-row data.frame
#-----------------------------------------------------------------------------------------
CastDataMedication <- function(data) {
  datar <- data.frame(  cost.med = CalculateCostMedication(data)
                        , label.med = data["label.med"] 
                        , frequency.med = as.integer(data["frequency.med"])
                        , week.med = data["week.med"]
                        , year.med = data["year.med"]
                        , stringsAsFactors = FALSE)
  rownames(datar) <- data["id.med"]
  return (datar)
}

#-----------------------------------------------------------------------------------------
# #C - CREATE
# Add the new row casting from inputs to the data.frame 
#-----------------------------------------------------------------------------------------
CreateDataMedication <- function(data) {
  
  data <- CastDataMedication(data)
  rownames(data) <- GetNextIdMedication()
  if (exists("df.med")) {
    df.med <<- rbind(df.med, data)
  } else {
    df.med <<- data
  }
}


#-----------------------------------------------------------------------------------------
# R - READ
# Return the whole data.frame if this is not null
#-----------------------------------------------------------------------------------------
ReadDataMedication <- function() {
  if (exists("df.med")) {
    df.med
  }
}

#-------------------------------------------------------------------------------------------
# U - UPDATE
# uPDATE the row with row.names matching row.names(data), where data is casting from inputs 
#-------------------------------------------------------------------------------------------E
UpdateDataMedication <- function(data) {
  data <- CastDataMedication(data)
  df.med[row.names(df.med) == row.names(data), ] <<- data
}

#------------------------------------------------------------------------------------------
# D - DELETE
# DELETE the row with row.names matching row.names(data), where data is casting from inputs 
#-----------------------------------------------------------------------------------------=
DeleteDataMedication <- function(data) {
  df.med <<- df.med[row.names(df.med) != unname(data["id.med"]), ]
  # update the ids
  if(nrow(df.med) > 0){
    row.names(df.med) <<- c(1:nrow(df.med)) 
  }
}

#-------------------------------------------------------------------------------------------
# Create the default record
#------------------------------------------------------------------------------------------
CreateDefaultMedication <- function() {
  default.medication <- CastDataMedication(
    list(id.med = "0", cost.med = 0, label.med = "", frequency.med = "0", week.med = "", year.med = ""))
  
  return (default.medication)
}

#-------------------------------------------------------------------------------------------
# Update the inputs with the values of the current record stored in the data object
#-------------------------------------------------------------------------------------------
UpdateInputsMedication <- function(data, session) {
  
  updateTextInput(session, "id.med", value = unname(rownames(data)))
  updateTextInput(session, "cost.med", value = CalculateCostMedication(data))
  updateTextInput(session, "label.med", value = unname(data["label.med"]))
  updateSliderInput(session, "frequency.med", value = as.integer(data["frequency.med"]))
  updateTextInput(session, "week.med", value = unname(data["week.med"]))
  updateTextInput(session, "year.med", value = unname(data["year.med"]))
  
}
