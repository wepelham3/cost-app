# Calculate Total Costs
GetTotalCostsMedication <- function() {
  
  if (exists("df.med")) {
    total.cost.med <<- sum(df.med$cost.med)
  } else {
    total.cost.med <<- 0
  }
  return(total.cost.med)  
}