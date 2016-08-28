get.professional.prices <- function() {

  professional.prices <- read.csv("professional_prices.csv",head=TRUE, stringsAsFactors = F);
  professional.prices$provider <- as.character(professional.prices$provider);
  professional.choices <- as.list(professional.prices$price)
  names(professional.choices) <- professional.prices$provider

return(professional.choices)
}