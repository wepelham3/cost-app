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