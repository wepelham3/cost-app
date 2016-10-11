GetYearMultiplier <- function(yearly.schedule){
  if (is.null(yearly.schedule) || yearly.schedule == "" ){
    return(0)
  }  
  
  if (yearly.schedule == "School year only") 0.75
  else if (yearly.schedule == "Summer only") 0.25
  else if (yearly.schedule == "Year-round") 1.00
}   