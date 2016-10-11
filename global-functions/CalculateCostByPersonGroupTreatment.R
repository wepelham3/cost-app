CalculateCostByPersonGroupTreatment<- function(data, i){
  
  # Frequency * ((duration +(commute_time*2))/60)* ProfessionalPrice/how_many_per_group
  
  if(data["num.families.gr"] > 0){
    
    cost <- as.integer(data["frequency.gr"]) * ((as.integer(data["duration.gr"]) + (as.integer(data[paste("p",i,".comm.gr",sep='')]) * 2))/60)  * GetProfessionalPrice(as.character(data[paste("person", i, ".gr", sep="")])) / as.integer(data["num.families.gr"])
    
  }
  
  return(cost)
  
}