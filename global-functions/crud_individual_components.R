# ----------------------------------------------------------------------------
# CRUD Functions for Individual Treatments
#
# ----------------------------------------------------------------------------- 

# ------------------------------------------------------------------------------
# Define the data.frame metadata (column specifications with labels)
# ------------------------------------------------------------------------------
GetTableMetadataIndivTreatment <- function(num.persons.ind = 5) {
  fields <- c(  id.ind = "Id" 
                , cost.ind = "Cost" 
                , label.ind = "Label" 
                , frequency.ind = "Freq/Year" 
                , duration.ind = "Duration"
                , person1.ind = "Person 1"
                , p1.comm.ind = "P1 Commute"
                , person2.ind = "Person 2"
                , p2.comm.ind = "P2 Commute"
                , person3.ind = "Person 3"
                , p3.comm.ind = "P3 Commute"
                , person4.ind = "Person 4"
                , p4.comm.ind = "P4 Commute"
                , person5.ind = "Person 5"
                , p5.comm.ind = "P5 Commute"
  )
  
  # for (i in 1:num.persons.ind){
  #  fields<- c(fields, setNames(paste0("Person ", i),paste0("person", i, ".ind")))
  #  fields<- c(fields, setNames(paste0("P", i, " Commute"), paste0("p", i, ".comm.ind")))
  #}
  
  result <- list(fields = fields)
  return (result)
}

#-----------------------------------------------------------------------------------------
# Generate the next ID of a new record
# ----------------------------------------------------------------------------------------
GetNextIdIndivTreatment <- function() {
  if (exists("df.ind.treatment") && nrow(df.ind.treatment) > 0) {
    max(as.integer(rownames(df.ind.treatment))) + 1
  } else {
    return (1)
  }
}

#-----------------------------------------------------------------------------------------
# Cast Data from Inputs to a one-row data.frame
#-----------------------------------------------------------------------------------------
CastDataIndivTreatment <- function(data, num.persons.ind = 5) {
  datar <- data.frame(  cost.ind = CalculateCostIndivTreatment(data, num.persons.ind)
                        , label.ind = data["label.ind"] 
                        , frequency.ind = as.integer(data["frequency.ind"])
                        , duration.ind = as.integer(data["duration.ind"])
                        , person1.ind = data["person1.ind"]
                        , p1.comm.ind = as.integer(data["p1.comm.ind"])
                        , person2.ind = if(data["person2.ind"] != "N/A") data["person2.ind"] else ""
                        , p2.comm.ind = as.integer(data["p2.comm.ind"])
                        , person3.ind = if(data["person3.ind"] != "N/A") data["person3.ind"] else ""
                        , p3.comm.ind = as.integer(data["p3.comm.ind"])
                        , person4.ind = if(data["person4.ind"] != "N/A") data["person4.ind"] else ""
                        , p4.comm.ind = as.integer(data["p4.comm.ind"])
                        , person5.ind = if(data["person5.ind"] != "N/A") data["person5.ind"] else ""
                        , p5.comm.ind = as.integer(data["p5.comm.ind"])
                        , stringsAsFactors = FALSE)
  
  # implement in the future with the parameter num.persons.ind
  #p.ind <- 5
  #for (i in 1:num.persons.ind){
  #  datar <- cbind(datar, setNames(data[p.ind], paste0("person", i, ".ind")) )  
  #  datar <- cbind(datar, setNames(data[p.ind + 1], paste0("p", i, ".comm.ind")) )  
  #  p.ind <- p.ind + 2
  #}
  
  rownames(datar) <- data["id.ind"]
  return (datar)
}

#-----------------------------------------------------------------------------------------
# #C - CREATE
# Add the new row casting from inputs to the data.frame 
#-----------------------------------------------------------------------------------------
CreateDataIndivTreatment <- function(data, num.persons.ind = 5) {
  
  data <- CastDataIndivTreatment(data, num.persons.ind)
  rownames(data) <- GetNextIdIndivTreatment()
  if (exists("df.ind.treatment")) {
    df.ind.treatment <<- rbind(df.ind.treatment, data)
  } else {
    df.ind.treatment <<- data
  }
}

#-----------------------------------------------------------------------------------------
# R - READ
# Return the whole data.frame if this is not null
#-----------------------------------------------------------------------------------------
ReadDataIndivTreatment <- function() {
  if (exists("df.ind.treatment")) {
    df.ind.treatment
  }
}

#-------------------------------------------------------------------------------------------
# U - UPDATE
# uPDATE the row with row.names matching row.names(data), where data is casting from inputs 
#-------------------------------------------------------------------------------------------
UpdateDataIndivTreatment <- function(data, num.persons.ind = 5) {
  data <- CastDataIndivTreatment(data, num.persons.ind)
  df.ind.treatment[row.names(df.ind.treatment) == row.names(data), ] <<- data
}

#------------------------------------------------------------------------------------------
# D - DELETE
# DELETE the row with row.names matching row.names(data), where data is casting from inputs 
#-----------------------------------------------------------------------------------------=
DeleteDataIndivTreatment <- function(data) {
  df.ind.treatment <<- df.ind.treatment[row.names(df.ind.treatment) != unname(data["id.ind"]), ]
  # update the ids
  if (nrow(df.ind.treatment) > 0){
    row.names(df.ind.treatment) <<- c(1:nrow(df.ind.treatment))
    
  }
  
}

#-------------------------------------------------------------------------------------------
# Create the default record
#------------------------------------------------------------------------------------------
CreateDefaultIndivTreatment <- function() {
  default.indiv.treatment <- CastDataIndivTreatment(
    list(id.ind = "0", cost.ind = "0", label.ind = "", frequency.ind = "0", duration.ind = "0"
         , person1.ind = "", p1.comm.ind = "0"
         , person2.ind = "", p2.comm.ind = "0"
         , person3.ind = "", p3.comm.ind = "0"
         , person4.ind = "", p4.comm.ind = "0"
         , person5.ind = "", p5.comm.ind = "0"  
    )
    , 5) 
  
  return (default.indiv.treatment)
}

#-------------------------------------------------------------------------------------------
# Update the inputs with the values of the current record stored in the data object
#-------------------------------------------------------------------------------------------
UpdateInputsIndivTreatment <- function(data, session, num.persons.ind = 5) {
  updateTextInput(session, "id.ind", value = unname(rownames(data)))
  updateTextInput(session, "cost.ind", value = CalculateCostIndivTreatment(data, num.persons.ind))
  updateTextInput(session, "label.ind", value = unname(data["label.ind"]))
  updateSliderInput(session, "frequency.ind", value = as.integer(data["frequency.ind"]))
  updateTextInput(session, "duration.ind", value = as.integer(data["duration.ind"]))
  
  
  for (i in 1:num.persons.ind){ 
    updateSelectInput(session
                      , inputId = paste0("person", i, ".ind")
                      , selected = data[paste0("person", i, ".ind")])
    
  }
  
  
  for (i in 1:num.persons.ind) {
    
    updateTextInput(session, paste0("p", i, ".comm.ind"), value = as.integer(data[paste0("p", i, ".comm.ind")])) 
    
    
  } # close for                    
  
}
