# Cast from Inputs to a one-row data.frame
CastDataGroupTreatment <- function(data, num.persons.gr = 5) {
  datar <- data.frame(  cost.gr = CalculateCostGroupTreatment(data, num.persons.gr)
                        , label.gr = data["label.gr"] 
                        , frequency.gr = as.integer(data["frequency.gr"])
                        , duration.gr = as.integer(data["duration.gr"])
                        , num.families.gr = as.integer(data["num.families.gr"])
                        , person1.gr = data["person1.gr"] 
                        , p1.comm.gr = as.integer(data["p1.comm.gr"])
                        , person2.gr = if(data["person2.gr"] != "N/A") data["person2.gr"] else ""
                        , p2.comm.gr = as.integer(data["p2.comm.gr"])
                        , person3.gr = if(data["person3.gr"] != "N/A") data["person3.gr"] else ""
                        , p3.comm.gr = as.integer(data["p3.comm.gr"])
                        , person4.gr = if(data["person4.gr"] != "N/A") data["person4.gr"] else ""
                        , p4.comm.gr = as.integer(data["p4.comm.gr"])
                        , person5.gr = if(data["person5.gr"] != "N/A") data["person5.gr"] else ""
                        , p5.comm.gr = as.integer(data["p5.comm.gr"])
                        , stringsAsFactors = FALSE)
  
  # implement in the future with the parameter num.persons.ind
  #p.ind <- 5
  #for (i in 1:num.persons.ind){
  #  datar <- cbind(datar, setNames(data[p.ind], paste0("person", i, ".ind")) )  
  #  datar <- cbind(datar, setNames(data[p.ind + 1], paste0("p", i, ".comm.ind")) )  
  #  p.ind <- p.ind + 2
  #}
  
  rownames(datar) <- data["id.gr"]
  return (datar)
}