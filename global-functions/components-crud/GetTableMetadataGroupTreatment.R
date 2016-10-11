# Get table metadata
GetTableMetadataGroupTreatment <- function(num.persons.gr = 5) {
  fields <- c(  id.gr = "Id" 
                , cost.gr = "Cost" 
                , label.gr = "Label" 
                , frequency.gr = "Freq/Year" 
                , duration.gr = "Duration"
                , num.families.gr = "Num Families"
                , person1.gr = "Person 1"
                , p1.comm.gr = "P1 Commute"
                , person2.gr = "Person 2"
                , p2.comm.gr = "P2 Commute"
                , person3.gr = "Person 3"
                , p3.comm.gr = "P3 Commute"
                , person4.gr = "Person 4"
                , p4.comm.gr = "P4 Commute"
                , person5.gr = "Person 5"
                , p5.comm.gr = "P5 Commute"
  )
  
  # for (i in 1:num.persons.ind){
  #  fields<- c(fields, setNames(paste0("Person ", i),paste0("person", i, ".ind")))
  #  fields<- c(fields, setNames(paste0("P", i, " Commute"), paste0("p", i, ".comm.ind")))
  #}
  
  result <- list(fields = fields)
  return (result)
}