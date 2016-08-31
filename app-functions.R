get.professional.prices <- function() {

  professional.prices <- read.csv("data-comps.csv",head=TRUE, stringsAsFactors = F);
  professional.prices$provider <- as.character(professional.prices$provider);
  professional.choices <- as.list(professional.prices$price)
  names(professional.choices) <- professional.prices$provider

return(professional.choices)
}

GetTableMetadataIndivTreatment <- function() {
  fields <- c(id.ind = "Id", 
              label.ind = "Label", 
              frequency.ind = "Frequency", 
              duration.ind = "Duration",
              num.persons.ind = "Num Persons")
  result <- list(fields = fields)
  return (result)
}

inputIndivTreatment <- function() {
  
  id.ind = 1
  label.ind = "aa"
  frequency.ind = 2
  duration.ind = 5
  num.persons.ind = 4
  #list <- as.list(id.ind, label.ind, frequency.ind, duration.ind, num.persons.ind)
  list  <- setNames(as.list(c(id.ind, label.ind, frequency.ind, duration.ind, num.persons.ind))
                  , c("id.ind", "label.ind", "frequency.ind", "duration.ind", "num.persons.ind"))
  return(list)
  
}

#Here we will use sapply, which works on a list or vector of data. 
#sapply(1:3, function(x) x^2)
#[1] 1 4 9

# Indiv. Treatment input fields are treated as a group
#formDataIndivTreatment <- reactive({
# sapply(names(GetTableMetadataIndivTreatment()$fields), function(x) input[[x]])
#})

formDataIndivTreatment <- function(){
  sapply(names(GetTableMetadataIndivTreatment()$fields), function(x) inputIndivTreatment())
}

GetTableMetadataIndivTreatment()$fields

inputIndivTreatment()

formDataIndivTreatment()
#$id.ind
#NULL

#$label.ind
#NULL

#$frequency.ind
#NULL

#$duration.ind
#NULL

#$num.persons.ind
#NULL





person1_ind = 'Pediatricians'

df.comps = read_csv("data-comps.csv", col_names = TRUE)

person1_price <- as.numeric(df.comps[which(df.comps$person == person1_ind), 2]);


d <- which(as.character(df.comps[ ,1]) == 'Pediatricians');


d <- which(df.comps$person == person1_ind)


person1_price