#-----------------------------------------------------------------------------------
# CREATE, READ, UPDATE. DELETE (CRUD) Functions for Group Treatments
#
#-----------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Define the data.frame metadata (columns specification with labels)
# ------------------------------------------------------------------------------
GetTableMetadataGroupTreatment <- function(num.persons.gr = 5) {
  fields <- c(  id.gr = "Id" 
                , cost.gr = "Cost" 
                , label.gr = "Label" 
                , frequency.gr = "Freq/Year" 
                , duration.gr = "Duration"
                , num.families.gr = "Num Families"
                
  )
  
  for (i in 1:num.persons.gr){
     
     fields<- c(fields, setNames(paste0("Person ", i),paste0("person", i, ".gr")))
     fields<- c(fields, setNames(paste0("P", i, " Commute"), paste0("p", i, ".comm.gr")))
     fields<- c(fields, setNames(paste0("P", i, " Leader"), paste0("p", i, ".lead.gr")))
    
 }
  
  result <- list(fields = fields)
  return (result)
}

#-----------------------------------------------------------------------------------------
# Generate the next ID of a new record
# ----------------------------------------------------------------------------------------
GetNextIdGroupTreatment <- function() {
 
   if (exists("df.gr.treatment") && nrow(df.gr.treatment) > 0) {
    max(as.integer(rownames(df.gr.treatment))) + 1
  } else {
    return (1)
  }

}

#-----------------------------------------------------------------------------------------
# Cast Data from Inputs to a one-row data.frame
#-----------------------------------------------------------------------------------------
CastDataGroupTreatment <- function(data, num.persons.gr = 5) {
  datar <- data.frame(  cost.gr = CalculateCostGroupTreatment(data, num.persons.gr)
                        , label.gr = data["label.gr"] 
                        , frequency.gr = as.integer(data["frequency.gr"])
                        , duration.gr = as.integer(data["duration.gr"])
                        , num.families.gr = as.integer(data["num.families.gr"])
                        , person1.gr = data["person1.gr"] 
                        , p1.comm.gr = as.integer(data["p1.comm.gr"])
                        , p1.lead.gr = as.logical(data["p1.lead.gr"])
                        , person2.gr = if(data["person2.gr"] != "N/A") data["person2.gr"] else ""
                        , p2.comm.gr = as.integer(data["p2.comm.gr"])
                        , p2.lead.gr = as.logical(data["p2.lead.gr"])
                        , person3.gr = if(data["person3.gr"] != "N/A") data["person3.gr"] else ""
                        , p3.comm.gr = as.integer(data["p3.comm.gr"])
                        , p3.lead.gr = as.logical(data["p3.lead.gr"])
                        , person4.gr = if(data["person4.gr"] != "N/A") data["person4.gr"] else ""
                        , p4.comm.gr = as.integer(data["p4.comm.gr"])
                        , p4.lead.gr = as.logical(data["p4.lead.gr"])
                        , person5.gr = if(data["person5.gr"] != "N/A") data["person5.gr"] else ""
                        , p5.comm.gr = as.integer(data["p5.comm.gr"])
                        , p5.lead.gr = as.logical(data["p5.lead.gr"])
                        , stringsAsFactors = FALSE)
  
  # implement in the future with the parameter num.persons.gr
  #p.gr <- 5
  #for (i in 1:num.persons.gr){
  #  datar <- cbind(datar, setNames(data[p.gr], paste0("person", i, ".gr")) )  
  #  datar <- cbind(datar, setNames(data[p.gr + 1], paste0("p", i, ".comm.gr")) )  
  #  p.gr <- p.gr + 2
  #}
  
  rownames(datar) <- data["id.gr"]
  return (datar)
}


#-----------------------------------------------------------------------------------------
# #C - CREATE
# Add the new row just casted from inputs to the data.frame 
#-----------------------------------------------------------------------------------------
CreateDataGroupTreatment <- function(data, num.persons.gr = 5) {
  
  data <- CastDataGroupTreatment(data, num.persons.gr)
  rownames(data) <- GetNextIdGroupTreatment()
  if (exists("df.gr.treatment")) {
    df.gr.treatment <<- rbind(df.gr.treatment, data)
  } else {
    df.gr.treatment <<- data
  }
}

#-----------------------------------------------------------------------------------------
# R - READ
# Return the whole data.frame if this is not null
#-----------------------------------------------------------------------------------------
ReadDataGroupTreatment <- function() {
 
  if (exists("df.gr.treatment")) {
    df.gr.treatment
  }
}


#-------------------------------------------------------------------------------------------
# U - UPDATE
# uPDATE the row with row.names matching row.names(data), where data is casting from inputs 
#-------------------------------------------------------------------------------------------E
UpdateDataGroupTreatment <- function(data, num.persons.gr = 5) {

  data <- CastDataGroupTreatment(data, num.persons.gr)
  df.gr.treatment[row.names(df.gr.treatment) == row.names(data), ] <<- data

}


#------------------------------------------------------------------------------------------
# D - DELETE
# DELETE the row with row.names matching row.names(data), where data is casting from inputs 
#-----------------------------------------------------------------------------------------=
DeleteDataGroupTreatment <- function(data) {
  df.gr.treatment <<- df.gr.treatment[row.names(df.gr.treatment) != unname(data["id.gr"]), ]
  # update the ids
  if(nrow(df.gr.treatment) >0){
    row.names(df.gr.treatment) <<- c(1:nrow(df.gr.treatment))   
  }
  
  
}

#-------------------------------------------------------------------------------------------
# Create the default record
#------------------------------------------------------------------------------------------
CreateDefaultGroupTreatment <- function() {
  default.group.treatment <- CastDataGroupTreatment(
    list(id.gr = "0", cost.gr = "0", label.gr = "", frequency.gr = "0", duration.gr = "0", num.families.gr = "1"
         , person1.gr = "", p1.comm.gr = "0", p1.lead.gr = FALSE
         , person2.gr = "", p2.comm.gr = "0", p2.lead.gr = FALSE
         , person3.gr = "", p3.comm.gr = "0", p3.lead.gr = FALSE
         , person4.gr = "", p4.comm.gr = "0", p4.lead.gr = FALSE
         , person5.gr = "", p5.comm.gr = "0", p5.lead.gr = FALSE  
    )
    , 5) 
  
  return (default.group.treatment)
}


#-------------------------------------------------------------------------------------------
# Update the inputs with the values of the current record stored in the data object
#-------------------------------------------------------------------------------------------
UpdateInputsGroupTreatment <- function(data, session, num.persons.gr = 5) {
  
  updateTextInput(session, "id.gr", value = unname(rownames(data)))
  updateTextInput(session, "cost.gr", value = CalculateCostGroupTreatment(data, num.persons.gr))
  updateTextInput(session, "label.gr", value = unname(data["label.gr"]))
  updateSliderInput(session, "frequency.gr", value = as.integer(data["frequency.gr"]))
  updateTextInput(session, "duration.gr", value = as.integer(data["duration.gr"]))
  updateTextInput(session, "num.families.gr", value = as.integer(data["num.families.gr"]))
  
  for (i in 1:num.persons.gr){ 
    updateSelectInput(session
                      , inputId = paste0("person", i, ".gr")
                      , selected = data[paste0("person", i, ".gr")])
    
  }
  
 
  for (i in 1:num.persons.gr) {
    
    updateTextInput(session, paste0("p", i, ".comm.gr"), value = as.integer(data[paste0("p", i, ".comm.gr")])) 
    
  } # close for
  
  for (i in 1:num.persons.gr) {
    
    updateCheckboxInput(session, paste0("p", i, ".lead.gr"), value = as.logical(data[paste0("p", i, ".lead.gr")]))
    
  } # close for 
  
}
