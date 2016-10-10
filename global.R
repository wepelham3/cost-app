library(magrittr)
library(readr)
library(plyr)

df.meds = read_csv("data/data-meds.csv", col_names = TRUE)
df.comps = read_csv("data/data-comps.csv", col_names = TRUE)

# --------------------------------------------------------------------

# CRUD Functions for Individual Treatments

# Get table metadata
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

# Find the next ID of a new record
GetNextIdIndivTreatment <- function() {
  if (exists("df.ind.treatment") && nrow(df.ind.treatment) > 0) {
    max(as.integer(rownames(df.ind.treatment))) + 1
  } else {
    return (1)
  }
}



# Cast from Inputs to a one-row data.frame
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

#C - CREATE
CreateDataIndivTreatment <- function(data, num.persons.ind = 5) {
  
  data <- CastDataIndivTreatment(data, num.persons.ind)
  rownames(data) <- GetNextIdIndivTreatment()
  if (exists("df.ind.treatment")) {
    df.ind.treatment <<- rbind(df.ind.treatment, data)
  } else {
    df.ind.treatment <<- data
  }
}

#R - READ
ReadDataIndivTreatment <- function() {
  if (exists("df.ind.treatment")) {
    df.ind.treatment
  }
}


#U - UPDATE
UpdateDataIndivTreatment <- function(data, num.persons.ind = 5) {
  data <- CastDataIndivTreatment(data, num.persons.ind)
  df.ind.treatment[row.names(df.ind.treatment) == row.names(data), ] <<- data
}

#D - DELETE
DeleteDataIndivTreatment <- function(data) {
  df.ind.treatment <<- df.ind.treatment[row.names(df.ind.treatment) != unname(data["id.ind"]), ]
  # update the ids
  if (nrow(df.ind.treatment) > 0){
    row.names(df.ind.treatment) <<- c(1:nrow(df.ind.treatment))
       
  }
 
}


# Return an empty, new record
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


#-----------------------------------------------------------------------------------------------------------------------------------------------

# CRUD Functions for Group Treatment

# Get table metadata
GetTableMetadataGroupTreatment <- function(num.persons.gr = 5) {
  fields <- c(  id.gr = "Id" 
                , cost.gr = "Cost" 
                , label.gr = "Label" 
                , frequency.gr = "Freq/Year" 
                , duration.gr = "Duration"
                , num.families.gr = "Num Families"
                , person1.gr = "Person 1"
                , p1.comm.gr = "P1 Commute"
                , person2.gr = "Person 2"
                , p2.comm.gr = "P2 Commute"
                , person3.gr = "Person 3"
                , p3.comm.gr = "P3 Commute"
                , person4.gr = "Person 4"
                , p4.comm.gr = "P4 Commute"
                , person5.gr = "Person 5"
                , p5.comm.gr = "P5 Commute"
  )
  
  # for (i in 1:num.persons.ind){
  #  fields<- c(fields, setNames(paste0("Person ", i),paste0("person", i, ".ind")))
  #  fields<- c(fields, setNames(paste0("P", i, " Commute"), paste0("p", i, ".comm.ind")))
  #}
  
  result <- list(fields = fields)
  return (result)
}


# Find the next ID of a new record
GetNextIdGroupTreatment <- function() {
  if (exists("df.gr.treatment") && nrow(df.gr.treatment) > 0) {
    max(as.integer(rownames(df.gr.treatment))) + 1
  } else {
    return (1)
  }
}

# Cast from Inputs to a one-row data.frame
CastDataGroupTreatment <- function(data, num.persons.gr = 5) {
  datar <- data.frame(  cost.gr = CalculateCostGroupTreatment(data, num.persons.gr)
                        , label.gr = data["label.gr"] 
                        , frequency.gr = as.integer(data["frequency.gr"])
                        , duration.gr = as.integer(data["duration.gr"])
                        , num.families.gr = as.integer(data["num.families.gr"])
                        , person1.gr = data["person1.gr"] 
                        , p1.comm.gr = as.integer(data["p1.comm.gr"])
                        , person2.gr = if(data["person2.gr"] != "N/A") data["person2.gr"] else ""
                        , p2.comm.gr = as.integer(data["p2.comm.gr"])
                        , person3.gr = if(data["person3.gr"] != "N/A") data["person3.gr"] else ""
                        , p3.comm.gr = as.integer(data["p3.comm.gr"])
                        , person4.gr = if(data["person4.gr"] != "N/A") data["person4.gr"] else ""
                        , p4.comm.gr = as.integer(data["p4.comm.gr"])
                        , person5.gr = if(data["person5.gr"] != "N/A") data["person5.gr"] else ""
                        , p5.comm.gr = as.integer(data["p5.comm.gr"])
                        , stringsAsFactors = FALSE)
  
  # implement in the future with the parameter num.persons.ind
  #p.ind <- 5
  #for (i in 1:num.persons.ind){
  #  datar <- cbind(datar, setNames(data[p.ind], paste0("person", i, ".ind")) )  
  #  datar <- cbind(datar, setNames(data[p.ind + 1], paste0("p", i, ".comm.ind")) )  
  #  p.ind <- p.ind + 2
  #}
  
  rownames(datar) <- data["id.gr"]
  return (datar)
}


#C - CREATE
CreateDataGroupTreatment <- function(data, num.persons.gr = 5) {
  
  data <- CastDataGroupTreatment(data, num.persons.gr)
  rownames(data) <- GetNextIdGroupTreatment()
  if (exists("df.gr.treatment")) {
    df.gr.treatment <<- rbind(df.gr.treatment, data)
  } else {
    df.gr.treatment <<- data
  }
}

#R - READ
ReadDataGroupTreatment <- function() {
  if (exists("df.gr.treatment")) {
    df.gr.treatment
  }
}


#U - UPDATE
UpdateDataGroupTreatment <- function(data, num.persons.gr = 5) {
  data <- CastDataGroupTreatment(data, num.persons.gr)
  df.gr.treatment[row.names(df.gr.treatment) == row.names(data), ] <<- data
}

#D - DELETE
DeleteDataGroupTreatment <- function(data) {
  df.gr.treatment <<- df.gr.treatment[row.names(df.gr.treatment) != unname(data["id.gr"]), ]
  # update the ids
  if(nrow(df.gr.treatment) >0){
    row.names(df.gr.treatment) <<- c(1:nrow(df.gr.treatment))   
  }
 
  
}

# Return an empty, new record
CreateDefaultGroupTreatment <- function() {
  default.group.treatment <- CastDataGroupTreatment(
    list(id.gr = "0", cost.gr = "0", label.gr = "", frequency.gr = "0", duration.gr = "0", num.families.gr = "1"
         , person1.gr = "", p1.comm.gr = "0"
         , person2.gr = "", p2.comm.gr = "0"
         , person3.gr = "", p3.comm.gr = "0"
         , person4.gr = "", p4.comm.gr = "0"
         , person5.gr = "", p5.comm.gr = "0"  
    )
    , 5) 
  
  return (default.group.treatment)
}


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




#-----------------------------------------------------------------------------------------------------------------------------------------------

# CRUD Functions for Medication

# Get table metadata
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


# Find the next ID of a new record
GetNextIdMedication <- function() {
  if (exists("df.med") && nrow(df.med) > 0) {
    max(as.integer(rownames(df.med))) + 1
  } else {
    return (1)
  }
}


# Cast from Inputs to a one-row data.frame
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

#C - CREATE
CreateDataMedication <- function(data) {
  
  data <- CastDataMedication(data)
  rownames(data) <- GetNextIdMedication()
  if (exists("df.med")) {
    df.med <<- rbind(df.med, data)
  } else {
    df.med <<- data
  }
}


#R - READ
ReadDataMedication <- function() {
  if (exists("df.med")) {
    df.med
  }
}


#U - UPDATE
UpdateDataMedication <- function(data) {
  data <- CastDataMedication(data)
  df.med[row.names(df.med) == row.names(data), ] <<- data
}

#D - DELETE
DeleteDataMedication <- function(data) {
  df.med <<- df.med[row.names(df.med) != unname(data["id.med"]), ]
  # update the ids
  if(nrow(df.med) > 0){
    row.names(df.med) <<- c(1:nrow(df.med)) 
  }
 
  
}

# Return an empty, new record
CreateDefaultMedication <- function() {
  default.medication <- CastDataMedication(
    list(id.med = "0", cost.med = 0, label.med = "", frequency.med = "0", week.med = "", year.med = ""))
  
  return (default.medication)
}


# Fill the input fields with the values of the selected record in the table
UpdateInputsMedication <- function(data, session) {
 
  updateTextInput(session, "id.med", value = unname(rownames(data)))
  updateTextInput(session, "cost.med", value = CalculateCostMedication(data))
  updateTextInput(session, "label.med", value = unname(data["label.med"]))
  updateSliderInput(session, "frequency.med", value = as.integer(data["frequency.med"]))
  updateTextInput(session, "week.med", value = unname(data["week.med"]))
  updateTextInput(session, "year.med", value = unname(data["year.med"]))
  
}


# Calculate Costs

GetProfessionalPrice <- function(person){
  
  price <- as.numeric(df.comps[which(df.comps$person == person), "mean.hourly.compensation"])
  
  if (is.null(price) || is.na(price)) price <- 0
  
  return (price)  
}


CalculateCostIndivTreatment <- function(data, num.persons.ind = 5) {
  
  num.persons.ind <- 5  # the default parameter is not working during the updating process
  prof.prices <- 0
  
  for (i in 1:num.persons.ind){
    prof.prices <- prof.prices  + ((as.integer(data["duration.ind"]) + (as.integer(data[paste("p",i,".comm.ind",sep='')]) * 2))/60
                                   * GetProfessionalPrice(as.character(data[paste("person", i, ".ind", sep="")])))
    
  }
  
  cost <- round(as.integer(data["frequency.ind"])  * prof.prices, 2)
  
  
  return (cost)
  
}

CalculateCostGroupTreatment <- function(data, num.persons.gr = 5) {
  
  num.persons.gr <- 5  # the default parameter is not working during the updating process
  prof.prices <- 0
  
  for (i in 1:num.persons.gr){
    prof.prices <- prof.prices  + ((as.integer(data["duration.gr"]) + (as.integer(data[paste("p",i,".comm.gr",sep='')]) * 2))/60
                                   * GetProfessionalPrice(as.character(data[paste("person", i, ".gr", sep="")])))
    
  }
  
  cost <- round((as.integer(data["frequency.gr"])  * prof.prices) /as.integer(data["num.families.gr"]), 2)
  
  
  return (cost)
  
}

# Calculate Cost for Medications

GetMedicationPrice <- function(medication){
  
  price <- as.numeric(df.meds[which(df.meds$name == medication), 2])
  
  if (is.null(price) || is.na(price)) price <- 0
  
  return (price)  
}

GetWeeklyDays <- function(weekly.schedule){
  
  if (is.null(weekly.schedule) || weekly.schedule == "" ){
    return(0)
    
  }  
  
  if (weekly.schedule == "Everyday") 7
  else if (weekly.schedule == "Weekdays only") 5
  else if (weekly.schedule == "Weekends only") 2
  
}  

GetYearMultiplier <- function(yearly.schedule){
  if (is.null(yearly.schedule) || yearly.schedule == "" ){
    return(0)
  }  
  
  if (yearly.schedule == "School year only") 0.75
  else if (yearly.schedule == "Summer only") 0.25
  else if (yearly.schedule == "Year-round") 1.00
}   


CalculateCostMedication <- function(data) {
  
  # 52*B34*(VLOOKUP(A34,meds,2,FALSE))*(VLOOKUP(C34,weekly,2,FALSE))*(VLOOKUP(D34,yearly,2,FALSE)),
  
  cost <- 52 * as.integer(data["frequency.med"]) * GetMedicationPrice(data["label.med"]) * as.numeric(GetWeeklyDays(as.character(data["week.med"]))) * as.numeric(GetYearMultiplier(as.character(data["year.med"])))
  
  cost <-round(cost, 2)
  
  return(cost)
  
  
}

# Summary 1

GetSummary1 <- function(explicit_costs, implicit_costs) {
  total.label <- c(" Total explicit cost...",
                   "+ Total implicit cost...",
                   "= Total combined cost...")
  cost <- c(explicit_costs, implicit_costs, explicit_costs + implicit_costs)
  df.summary <- data.frame(total.label, cost)
  return(df.summary)
  
}

# Summary 2

# Calculate Total Costs
GetTotalCostsMedication <- function() {
  
  if (exists("df.med")) {
    total.cost.med <<- sum(df.med$cost.med)
  } else {
    total.cost.med <<- 0
  }
  return(total.cost.med)  
}



GetSummary2 <- function(med.costs, prof.costs, parent.costs) {
  total.label <- c("Total cost of medications...",
                   "Total cost of professional time...",
                   "Total cost of parent time...")
  cost <- c(med.costs, prof.costs, parent.costs)
  df.summary <- data.frame(total.label, cost)
  return(df.summary)
  
}

# By Individual Component

GetSummaryByIndivTreatment <- function() {
  
  if (exists("df.ind.treatment") && nrow(df.ind.treatment) > 0){
    
    individual.component <- paste(df.ind.treatment$label.ind,", ",df.ind.treatment$frequency.ind,"x/yr, "
                                  ,df.ind.treatment$duration.ind,"min each", sep="")
   
    cost <- df.ind.treatment$cost
      
    df.summary.ind <- data.frame(individual.component, cost)
   
  } 
  
  else {
    
    df.summary.ind  <- data.frame(  individual.component = "-"
                                  , cost = 0 )
    
    
    
  }
  
 
  return(df.summary.ind)
  
  
} # close GetSummaryByIndivTreatment

# By Group Component


GetSummaryByGroupTreatment <- function() {
  
  if (exists("df.gr.treatment") && nrow(df.gr.treatment) > 0){
    
    group.component <- paste(df.gr.treatment$label.gr,", ",df.gr.treatment$frequency.gr,"x/yr, "
                                  ,df.gr.treatment$duration.gr,"min each", sep="")
    
    #cost <- paste("$", df.gr.treatment$cost)
    
    cost <- df.gr.treatment$cost
    
    df.summary.gr <- data.frame(group.component, cost)
    
  } 
  else {
    
    df.summary.gr <- data.frame(  group.component = "-"
                                   , cost = 0 )
    
    
    
  }
    

  return(df.summary.gr)
  
} # close GetSummaryByGroupTreatment


# By Person

CalculateCostByPersonIndivTreatment<- function(data, i){
  
  
  # Freq*((Duration+(CommuteTime*2))/60)*Professional_Price
  
  cost <- as.integer(data["frequency.ind"]) * ((as.integer(data["duration.ind"]) + (as.integer(data[paste("p",i,".comm.ind",sep='')]) * 2))/60) * GetProfessionalPrice(as.character(data[paste("person", i, ".ind", sep="")])) 
  
  
  return(cost)
  
}

CalculateCostByPersonGroupTreatment<- function(data, i){
  
  # Frequency * ((duration +(commute_time*2))/60)* ProfessionalPrice/how_many_per_group
  
  if(data["num.families.gr"] > 0){
    
    cost <- as.integer(data["frequency.gr"]) * ((as.integer(data["duration.gr"]) + (as.integer(data[paste("p",i,".comm.gr",sep='')]) * 2))/60)  * GetProfessionalPrice(as.character(data[paste("person", i, ".gr", sep="")])) / as.integer(data["num.families.gr"])
    
  }
  
  return(cost)
  
}

GetSummaryByPerson <- function(num.persons = 5){
  
  
  list.persons = list()
  
  k <- 1
  
  
  if (exists("df.ind.treatment") && nrow(df.ind.treatment) > 0 ) {
    
    for (i in 1:nrow(df.ind.treatment)){
      
      data <- df.ind.treatment[i, ];
      
      for (j in 1:num.persons){ 
        
        person = as.character(data[paste("person", j, ".ind", sep="")])
        
        if(person != ""){
          cost_by_person <- data.frame(person = person
                                       , cost = CalculateCostByPersonIndivTreatment(data, j)
                                       , stringsAsFactors = TRUE 
          )
          
          list.persons[[k]] <- cost_by_person
          
          k <- k + 1
        }  
      }  
      
    }   
    
  }  
  
  if (exists("df.gr.treatment") && nrow(df.gr.treatment) > 0 )  {
    
    for (i in 1:nrow(df.gr.treatment) && nrow(df.gr.treatment) > 0){
      
      data <- df.gr.treatment[i, ];
      
      for (j in 1:num.persons){ 
        
        person = as.character(data[paste("person", j, ".gr", sep="")])
        
        if(person != ""){
          cost_by_person <- data.frame(person = person
                                       , cost = CalculateCostByPersonGroupTreatment(data, j)
                                       , stringsAsFactors = TRUE 
          )
          
          list.persons[[k]] <- cost_by_person
          
          k <- k + 1
        }  
      }  
      
    }   
    
  }  
  
  if(k > 1) {
  
     df.persons <- do.call(rbind, list.persons)
     df.persons <- ddply(df.persons, .(person), summarise, cost=sum(cost))
     df.persons$cost = round(df.persons$cost, 2)
     
   }
  
  else {
    
    df.persons <- data.frame(  person = "-"
                               , cost = 0)
    
  }

  return(df.persons)   
    
} # close GetSummaryByPerson


GetSummaryByParent <- function(num.persons = 5){
  
  
  list.persons = list()
  
  k <- 1
  
  
  if (exists("df.ind.treatment") &&  nrow(df.ind.treatment) > 0)  {
    
    for (i in 1:nrow(df.ind.treatment)){
      
      data <- df.ind.treatment[i, ];
      
      for (j in 1:num.persons){ 
        
        person = as.character(data[paste("person", j, ".ind", sep="")])
        
        if(person == "Parent"){
          cost_by_person <- data.frame(person = person
                                       , cost = CalculateCostByPersonIndivTreatment(data, j)
                                       , stringsAsFactors = TRUE 
          )
          
          list.persons[[k]] <- cost_by_person
          
          k <- k + 1
        }  
      }  
      
    }   
    
  }  
  
  if (exists("df.gr.treatment") && nrow(df.gr.treatment) > 0) {
    
    for (i in 1:nrow(df.gr.treatment)){
      
      data <- df.gr.treatment[i, ];
      
      for (j in 1:num.persons){ 
        
        person = as.character(data[paste("person", j, ".gr", sep="")])
        
        if(person == "Parent"){
          cost_by_person <- data.frame(person = person
                                       , cost = CalculateCostByPersonGroupTreatment(data, j)
                                       , stringsAsFactors = TRUE 
          )
          
          list.persons[[k]] <- cost_by_person
          
          k <- k + 1
        }  
      }  
      
    }   
    
  }  
  
  if(k > 1) {
    
    df.persons <- do.call(rbind, list.persons)
    df.persons <- ddply(df.persons, .(person), summarise, cost=sum(cost))
    df.persons$cost = round(df.persons$cost, 2)
    
  }
  
  else {
    
   df.persons <- data.frame( person = character()
                             , cost = integer()
   )
                                                        
  }
  

  return(df.persons)   
  
} # close GetSummaryByParent



# By Medication

GetSummaryByMedication <- function() {
  
  if (exists("df.med") &&  nrow(df.med) > 0) {
    
    med.component <- paste(df.med$label.med,", ",df.med$frequency.med,"x/day, ", df.med$week.med
                           , ", ", df.med$year.med, sep="")
    
  #  cost <- paste("$", df.med$cost)
    
    cost <- df.med$cost
    df.summary.med <- data.frame(med.component, cost)
    
  } 
  
  else
    {
      
      df.summary.med <- data.frame(  medication = "-"
                                   , cost = 0)
    
    }
      

 
  return(df.summary.med)   
    
} # close GetSummaryByMedication

OutputSummary <- function(df.summary, column.names) {

df.summary.out <- as.data.frame(df.summary)
df.summary.out$cost <- comma(df.summary.out$cost)
df.summary.out$cost <- paste0("$", df.summary.out$cost)
names(df.summary.out) <- column.names

return(df.summary.out)

}



