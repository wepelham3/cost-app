OutputSummary <- function(df.summary, column.names) {
  
  df.summary.out <- as.data.frame(df.summary)
  df.summary.out$cost <- comma(df.summary.out$cost)
  df.summary.out$cost <- paste0("$", df.summary.out$cost)
  names(df.summary.out) <- column.names
  
  return(df.summary.out)
  
}