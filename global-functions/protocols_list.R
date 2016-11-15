require(XML)
require(reshape2)

#-----------------------------------------------------------------------------------------
# Add the new protocol stored in protocol.list to the protocols.list (a list of lists) 
# for future comparison 
#-----------------------------------------------------------------------------------------
AddProtocolToList <- function(protocol.list) {
  
  if (exists("protocols.list")) {
   protocols.list <<- list(protocols.list, protocol.list)
  } else {
    protocols.list <<- protocol.list
  }
}


#-----------------------------------------------------------------------------------------
# Cast Data from all the current's protocol data objects to a list of objects
#'
#-----------------------------------------------------------------------------------------
CastProtocolData<- function(protocol.name, explicit.cost, implicit.cost
                            , med.cost, prof.cost, parent.cost ) {
  
  protocol.data<- list(
                  protocol.name = protocol.name
                , ind.component.data = ReadDataIndivTreatment()
                , grp.component.data = ReadDataGroupTreatment()
                , med.component.data = ReadDataMedication()
                , ind.component.summary = GetSummaryByIndivTreatment()
                , grp.component.summary  = GetSummaryByGroupTreatment()
                , med.component.summary  = GetSummaryByMedication()
                , person.summary  = GetSummaryByPerson()
                , parent.summary  = GetSummaryByParent()
                , total.explicit.cost = explicit.cost  
                , total.implicit.cost = implicit.cost
                , total.combined.cost = explicit.cost + implicit.cost
                , total.cost.medications = med.cost
                , total.cost.professional.time = prof.cost
                , total.cost.parent.time = parent.cost  
                
               ) 
 
  return(protocol.data) 
}

############################################################################
# Save the Active Protocol to an RMD file
# First Save all the information to a list of objects and then save to XML
###########################################################################
SaveProtocolToRDS <- function(file, protocol.name, explicit.cost, implicit.cost
                             , med.cost, prof.cost, parent.cost ){
  
   
   # Cast Protocol data
   protocol.data <- CastProtocolData(protocol.name, explicit.cost, implicit.cost
                                        , med.cost, prof.cost, parent.cost )
   
   
   saveRDS(protocol.data, file)
}   



##################################################################################
# Upload a protocol previouly saved in an RDS File and save it to the Protocol LIst
##################################################################################
UploadProtocolFromRDSToList <- function(protocol.RDSfile){
  
  if (is.null(protocol.RDSfile))
    return(NULL)
  
  protocol.data <- readRDS(protocol.RDSfile$datapath)
  
  return(protocol.data)   

}




