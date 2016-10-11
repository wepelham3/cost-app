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