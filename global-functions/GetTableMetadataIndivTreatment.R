# Get table metadata
GetTableMetadataIndivTreatment <- function(num.persons.ind = 5) {
  fields <- c(  id.ind = "Id" 
                , cost.ind = "Cost" 
                , label.ind = "Label" 
                , frequency.ind = "Freq/Year" 
                , duration.ind = "Duration"
                , person1.ind = "Person 1"
                , p1.comm.ind = "P1 Commute"
                , person2.ind = "Person 2"
                , p2.comm.ind = "P2 Commute"
                , person3.ind = "Person 3"
                , p3.comm.ind = "P3 Commute"
                , person4.ind = "Person 4"
                , p4.comm.ind = "P4 Commute"
                , person5.ind = "Person 5"
                , p5.comm.ind = "P5 Commute"
  )
  
  # for (i in 1:num.persons.ind){
  #  fields<- c(fields, setNames(paste0("Person ", i),paste0("person", i, ".ind")))
  #  fields<- c(fields, setNames(paste0("P", i, " Commute"), paste0("p", i, ".comm.ind")))
  #}
  
  result <- list(fields = fields)
  return (result)
}