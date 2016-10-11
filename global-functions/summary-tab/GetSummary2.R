GetSummary2 <- function(med.costs, prof.costs, parent.costs) {
  total.label <- c("Total cost of medications...",
                   "Total cost of professional time...",
                   "Total cost of parent time...")
  cost <- c(med.costs, prof.costs, parent.costs)
  df.summary <- data.frame(total.label, cost)
  return(df.summary)
  
}