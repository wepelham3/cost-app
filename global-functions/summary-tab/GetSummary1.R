GetSummary1 <- function(explicit_costs, implicit_costs) {
  total.label <- c(" Total explicit cost...",
                   "+ Total implicit cost...",
                   "= Total combined cost...")
  cost <- c(explicit_costs, implicit_costs, explicit_costs + implicit_costs)
  df.summary <- data.frame(total.label, cost)
  return(df.summary)
  
}