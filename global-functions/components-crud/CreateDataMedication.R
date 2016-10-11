#C - CREATE
CreateDataMedication <- function(data) {
  
  data <- CastDataMedication(data)
  rownames(data) <- GetNextIdMedication()
  if (exists("df.med")) {
    df.med <<- rbind(df.med, data)
  } else {
    df.med <<- data
  }
}