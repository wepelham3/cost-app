#U - UPDATE
UpdateDataMedication <- function(data) {
  data <- CastDataMedication(data)
  df.med[row.names(df.med) == row.names(data), ] <<- data
}