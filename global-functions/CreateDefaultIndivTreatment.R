# Return an empty, new record
CreateDefaultIndivTreatment <- function() {
  default.indiv.treatment <- CastDataIndivTreatment(
    list(id.ind = "0", cost.ind = "0", label.ind = "", frequency.ind = "0", duration.ind = "0"
         , person1.ind = "", p1.comm.ind = "0"
         , person2.ind = "", p2.comm.ind = "0"
         , person3.ind = "", p3.comm.ind = "0"
         , person4.ind = "", p4.comm.ind = "0"
         , person5.ind = "", p5.comm.ind = "0"  
    )
    , 5) 
  
  return (default.indiv.treatment)
}