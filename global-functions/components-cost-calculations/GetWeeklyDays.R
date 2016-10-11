GetWeeklyDays <- function(weekly.schedule){
  
  if (is.null(weekly.schedule) || weekly.schedule == "" ){
    return(0)
    
  }  
  
  if (weekly.schedule == "Everyday") 7
  else if (weekly.schedule == "Weekdays only") 5
  else if (weekly.schedule == "Weekends only") 2
  
}  