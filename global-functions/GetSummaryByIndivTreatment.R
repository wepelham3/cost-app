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