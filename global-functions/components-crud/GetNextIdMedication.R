# Find the next ID of a new record
GetNextIdMedication <- function() {
  if (exists("df.med") && nrow(df.med) > 0) {
    max(as.integer(rownames(df.med))) + 1
  } else {
    return (1)
  }
}