#U - UPDATE
UpdateDataIndivTreatment <- function(data, num.persons.ind = 5) {
  data <- CastDataIndivTreatment(data, num.persons.ind)
  df.ind.treatment[row.names(df.ind.treatment) == row.names(data), ] <<- data
}