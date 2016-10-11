#U - UPDATE
UpdateDataGroupTreatment <- function(data, num.persons.gr = 5) {
  data <- CastDataGroupTreatment(data, num.persons.gr)
  df.gr.treatment[row.names(df.gr.treatment) == row.names(data), ] <<- data
}