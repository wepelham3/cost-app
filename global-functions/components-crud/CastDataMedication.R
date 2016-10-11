# Cast from Inputs to a one-row data.frame
CastDataMedication <- function(data) {
  datar <- data.frame(  cost.med = CalculateCostMedication(data)
                        , label.med = data["label.med"] 
                        , frequency.med = as.integer(data["frequency.med"])
                        , week.med = data["week.med"]
                        , year.med = data["year.med"]
                        , stringsAsFactors = FALSE)
  rownames(datar) <- data["id.med"]
  return (datar)
}