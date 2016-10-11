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