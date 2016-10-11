#D - DELETE
DeleteDataIndivTreatment <- function(data) {
  df.ind.treatment <<- df.ind.treatment[row.names(df.ind.treatment) != unname(data["id.ind"]), ]
  # update the ids
  if (nrow(df.ind.treatment) > 0){
    row.names(df.ind.treatment) <<- c(1:nrow(df.ind.treatment))
    
  }
  
}