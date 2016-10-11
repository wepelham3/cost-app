# Find the next ID of a new record
GetNextIdIndivTreatment <- function() {
  if (exists("df.ind.treatment") && nrow(df.ind.treatment) > 0) {
    max(as.integer(rownames(df.ind.treatment))) + 1
  } else {
    return (1)
  }
}