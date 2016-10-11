# Cast from Inputs to a one-row data.frame
CastDataIndivTreatment <- function(data, num.persons.ind = 5) {
  datar <- data.frame(  cost.ind = CalculateCostIndivTreatment(data, num.persons.ind)
                        , label.ind = data["label.ind"] 
                        , frequency.ind = as.integer(data["frequency.ind"])
                        , duration.ind = as.integer(data["duration.ind"])
                        , person1.ind = data["person1.ind"]
                        , p1.comm.ind = as.integer(data["p1.comm.ind"])
                        , person2.ind = if(data["person2.ind"] != "N/A") data["person2.ind"] else ""
                        , p2.comm.ind = as.integer(data["p2.comm.ind"])
                        , person3.ind = if(data["person3.ind"] != "N/A") data["person3.ind"] else ""
                        , p3.comm.ind = as.integer(data["p3.comm.ind"])
                        , person4.ind = if(data["person4.ind"] != "N/A") data["person4.ind"] else ""
                        , p4.comm.ind = as.integer(data["p4.comm.ind"])
                        , person5.ind = if(data["person5.ind"] != "N/A") data["person5.ind"] else ""
                        , p5.comm.ind = as.integer(data["p5.comm.ind"])
                        , stringsAsFactors = FALSE)
  
  # implement in the future with the parameter num.persons.ind
  #p.ind <- 5
  #for (i in 1:num.persons.ind){
  #  datar <- cbind(datar, setNames(data[p.ind], paste0("person", i, ".ind")) )  
  #  datar <- cbind(datar, setNames(data[p.ind + 1], paste0("p", i, ".comm.ind")) )  
  #  p.ind <- p.ind + 2
  #}
  
  rownames(datar) <- data["id.ind"]
  return (datar)
}