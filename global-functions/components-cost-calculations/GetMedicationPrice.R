GetMedicationPrice <- function(medication){
  
  price <- as.numeric(df.meds[which(df.meds$name == medication), 2])
  
  if (is.null(price) || is.na(price)) price <- 0
  
  return (price)  
}