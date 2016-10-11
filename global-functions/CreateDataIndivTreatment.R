#C - CREATE
CreateDataIndivTreatment <- function(data, num.persons.ind = 5) {
  
  data <- CastDataIndivTreatment(data, num.persons.ind)
  rownames(data) <- GetNextIdIndivTreatment()
  if (exists("df.ind.treatment")) {
    df.ind.treatment <<- rbind(df.ind.treatment, data)
  } else {
    df.ind.treatment <<- data
  }
}