#D - DELETE
DeleteDataGroupTreatment <- function(data) {
  df.gr.treatment <<- df.gr.treatment[row.names(df.gr.treatment) != unname(data["id.gr"]), ]
  # update the ids
  if(nrow(df.gr.treatment) >0){
    row.names(df.gr.treatment) <<- c(1:nrow(df.gr.treatment))   
  }
  
  
}