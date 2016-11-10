####################################################################################################
# Functions for Cost Calculations 
#
####################################################################################################
GetProfessionalPrice <- function(person){
  
  price <- as.numeric(df.comps[which(df.comps$person == person), "mean.hourly.compensation"])
  
  if (is.null(price) || is.na(price)) price <- 0
  
  return (price)  
}

####################################################################################################
# Calculate Cost for Individual Treatments
#
####################################################################################################
CalculateCostByPersonIndivTreatment <- function(data, i){
  
  
  # Freq*((Duration+(CommuteTime*2))/60)*Professional_Price
  
  person.cost <- as.integer(data["frequency.ind"]) * ((as.integer(data["duration.ind"]) + (as.integer(data[paste("p",i,".comm.ind",sep='')]) * 2))/60) * GetProfessionalPrice(as.character(data[paste("person", i, ".ind", sep="")])) 
  
  
  return(person.cost)
  
}

######################################################################################################
CalculateCostIndivTreatment <- function(data, num.persons.ind = 5) {
  
  num.persons.ind <- 5  # the default parameter is not working during the updating process
  cost <- 0
  
  for (i in 1:num.persons.ind){
    
    person = as.character(data[paste("person", i, ".ind", sep="")])
    
    if(person != "")
    
       cost <- cost + CalculateCostByPersonIndivTreatment(data, i) 
    
    
  }
  
 
  return (round(cost, 2))
  
}

####################################################################################################
# Calculate Cost for Group Treatments
#
####################################################################################################

CalculateCostByPersonGroupTreatment <- function(data, i){
  
  # Frequency * ((duration +(commute_time*2))/60)* ProfessionalPrice/how_many_per_group
  # The calculations for the group components must differentiate between 
  # those that are LEADING the group (typically a psychologist) and those 
  # who are ATTENDING the group (typically a parent). 
  # For the LEADERS, the cost is divided by the number of families in the group. 
  # For the ATTENDERS, the cost is NOT divided by the number of families in the group.
  
  
  person.cost <- as.integer(data["frequency.gr"]) * ((as.integer(data["duration.gr"]) + (as.integer(data[paste("p",i,".comm.gr",sep='')]) * 2))/60)  
  
  if (data[paste("p",i,".lead.gr",sep='')] == TRUE && data["num.families.gr"] > 0)
    
    person.cost <- person.cost * GetProfessionalPrice(as.character(data[paste("person", i, ".gr", sep="")])) / as.integer(data["num.families.gr"])
  
  else 
    
    person.cost <- person.cost * GetProfessionalPrice(as.character(data[paste("person", i, ".gr", sep="")]))   
  
  
  return(person.cost)
  
}

####################################################################################################################

CalculateCostGroupTreatment <- function(data, num.persons.gr = 5) {
  
  num.persons.gr <- 5  # the default parameter is not working during the updating process
  cost <- 0
  
  for (i in 1:num.persons.gr){
   
    person <- as.character(data[paste("person", i, ".gr", sep="")])
    
    if(person != "")
      
      cost <- cost + CalculateCostByPersonGroupTreatment(data, i) 
    
   }
  
  
  return (round(cost, 2))
  
}


####################################################################################################
# Calculate Cost for Medications
#
####################################################################################################
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

# Calculate Total Costs
GetTotalCostsMedication <- function() {
  
  if (exists("df.med")) {
    total.cost.med <<- sum(df.med$cost.med)
  } else {
    total.cost.med <<- 0
  }
  return(total.cost.med)  
}

  
