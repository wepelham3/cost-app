# Return an empty, new record
CreateDefaultMedication <- function() {
  default.medication <- CastDataMedication(
    list(id.med = "0", cost.med = 0, label.med = "", frequency.med = "0", week.med = "", year.med = ""))
  
  return (default.medication)
}