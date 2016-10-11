GetProfessionalPrice <- function(person){
  
  price <- as.numeric(df.comps[which(df.comps$person == person), "mean.hourly.compensation"])
  
  if (is.null(price) || is.na(price)) price <- 0
  
  return (price)  
}