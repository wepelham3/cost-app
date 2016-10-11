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