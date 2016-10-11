# Get table metadata
GetTableMetadataMedication <- function() {
  fields <- c(  id.med = "Id" 
                , cost.med ="Cost"
                , label.med = "Label" 
                , frequency.med = "Freq/Year" 
                , week.med = "Weekly Schedule"
                , year.med = "Yearly Schedule"
  )
  
  
  result <- list(fields = fields)
  return (result)
}