CalculateCostMedication <- function(data) {
  
  # 52*B34*(VLOOKUP(A34,meds,2,FALSE))*(VLOOKUP(C34,weekly,2,FALSE))*(VLOOKUP(D34,yearly,2,FALSE)),
  
  cost <- 52 * as.integer(data["frequency.med"]) * GetMedicationPrice(data["label.med"]) * as.numeric(GetWeeklyDays(as.character(data["week.med"]))) * as.numeric(GetYearMultiplier(as.character(data["year.med"])))
  
  cost <-round(cost, 2)
  
  return(cost)
  
  
}