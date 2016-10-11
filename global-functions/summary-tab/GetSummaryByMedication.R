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