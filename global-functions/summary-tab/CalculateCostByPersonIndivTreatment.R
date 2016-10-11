CalculateCostByPersonIndivTreatment<- function(data, i){
  
  
  # Freq*((Duration+(CommuteTime*2))/60)*Professional_Price
  
  cost <- as.integer(data["frequency.ind"]) * ((as.integer(data["duration.ind"]) + (as.integer(data[paste("p",i,".comm.ind",sep='')]) * 2))/60) * GetProfessionalPrice(as.character(data[paste("person", i, ".ind", sep="")])) 
  
  
  return(cost)
  
}