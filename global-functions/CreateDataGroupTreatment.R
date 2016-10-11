#C - CREATE
CreateDataGroupTreatment <- function(data, num.persons.gr = 5) {
  
  data <- CastDataGroupTreatment(data, num.persons.gr)
  rownames(data) <- GetNextIdGroupTreatment()
  if (exists("df.gr.treatment")) {
    df.gr.treatment <<- rbind(df.gr.treatment, data)
  } else {
    df.gr.treatment <<- data
  }
}