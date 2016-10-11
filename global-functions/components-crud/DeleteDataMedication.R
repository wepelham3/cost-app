#D - DELETE
DeleteDataMedication <- function(data) {
  df.med <<- df.med[row.names(df.med) != unname(data["id.med"]), ]
  # update the ids
  if(nrow(df.med) > 0){
    row.names(df.med) <<- c(1:nrow(df.med)) 
  }
  
  
}