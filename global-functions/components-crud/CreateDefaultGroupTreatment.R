# Return an empty, new record
CreateDefaultGroupTreatment <- function() {
  default.group.treatment <- CastDataGroupTreatment(
    list(id.gr = "0", cost.gr = "0", label.gr = "", frequency.gr = "0", duration.gr = "0", num.families.gr = "1"
         , person1.gr = "", p1.comm.gr = "0", p1.lead.gr = FALSE
         , person2.gr = "", p2.comm.gr = "0"
         , person3.gr = "", p3.comm.gr = "0"
         , person4.gr = "", p4.comm.gr = "0"
         , person5.gr = "", p5.comm.gr = "0"  
    )
    , 5) 
  
  return (default.group.treatment)
}