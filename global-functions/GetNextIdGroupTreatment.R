# Find the next ID of a new record
GetNextIdGroupTreatment <- function() {
  if (exists("df.gr.treatment") && nrow(df.gr.treatment) > 0) {
    max(as.integer(rownames(df.gr.treatment))) + 1
  } else {
    return (1)
  }
}
