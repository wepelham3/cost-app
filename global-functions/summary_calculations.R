##########################################################################
# I. Get Summary 1 - explicit cost, implicit cost, combined cost
##########################################################################

GetSummary1 <- function(explicit.costs, implicit.costs) {
  total.label <- c(" Total explicit cost...",
                   "+ Total implicit cost...",
                   "= Total combined cost...")
  cost <- c(explicit.costs, implicit.costs, explicit.costs + implicit.costs)
  df.summary <- data.frame(total.label, cost)
  return(df.summary)
  
}

##########################################################################
# II. Get Summary 2 - medication costs, professional costs, parent costs
##########################################################################

GetSummary2 <- function(med.costs, prof.costs, parent.costs) {
  total.label <- c("Total cost of medications...",
                   "Total cost of professional time...",
                   "Total cost of parent time...")
  cost <- c(med.costs, prof.costs, parent.costs)
  df.summary <- data.frame(total.label, cost)
  return(df.summary)
  
}

##########################################################################
# III. Get Summary by Individual Treatment
##########################################################################
GetSummaryByIndivTreatment <- function() {
  
  if (exists("df.ind.treatment") && nrow(df.ind.treatment) > 0){
    
    individual.component <- paste(df.ind.treatment$label.ind,", ",df.ind.treatment$frequency.ind,"x/yr, "
                                  ,df.ind.treatment$duration.ind,"min each", sep="")
    
    cost <- df.ind.treatment$cost
    
    df.summary.ind <- data.frame(individual.component, cost)
    
  } 
  
  else {
    
    df.summary.ind  <- data.frame(  individual.component = "-"
                                    , cost = 0 )
    
  }
  
  return(df.summary.ind)
  
} # close GetSummaryByIndivTreatment


##########################################################################
# IV. Get Summary by Group Treatment
##########################################################################
GetSummaryByGroupTreatment <- function() {
  
  if (exists("df.gr.treatment") && nrow(df.gr.treatment) > 0){
    
    group.component <- paste(df.gr.treatment$label.gr,", ",df.gr.treatment$frequency.gr,"x/yr, "
                             ,df.gr.treatment$duration.gr,"min each", sep="")
    
    #cost <- paste("$", df.gr.treatment$cost)
    
    cost <- df.gr.treatment$cost
    
    df.summary.gr <- data.frame(group.component, cost)
    
  } 
  else {
    
    df.summary.gr <- data.frame(  group.component = "-"
                                  , cost = 0 )
    
  }
  
  
  return(df.summary.gr)
  
} # close GetSummaryByGroupTreatment


##########################################################################
# V. Get Summary by Person
##########################################################################

GetSummaryByPerson <- function(num.persons = 5){
  
  
  list.persons = list()
  
  k <- 1
  
  
  if (exists("df.ind.treatment") && nrow(df.ind.treatment) > 0 ) {
    
    for (i in 1:nrow(df.ind.treatment)){
      
      data <- df.ind.treatment[i, ];
      
      for (j in 1:num.persons){ 
        
        person = as.character(data[paste("person", j, ".ind", sep="")])
        
        if(person != ""){
          cost_by_person <- data.frame(person = person
                                       , cost = CalculateCostByPersonIndivTreatment(data, j)
                                       , stringsAsFactors = TRUE 
          )
          
          list.persons[[k]] <- cost_by_person
          
          k <- k + 1
        }  
      }  
      
    }   
    
  }  
  
  if (exists("df.gr.treatment") && nrow(df.gr.treatment) > 0 )  {
    
    for (i in 1:nrow(df.gr.treatment)){
      
      data <- df.gr.treatment[i, ];
      
      for (j in 1:num.persons){ 
        
        person = as.character(data[paste("person", j, ".gr", sep="")])
        
        if(person != ""){
          
          cost_by_person <- data.frame(person = person
                                       , cost = CalculateCostByPersonGroupTreatment(data, j)
                                       , stringsAsFactors = TRUE 
          )
          
          list.persons[[k]] <- cost_by_person
          
          k <- k + 1
        }  
      }  
      
    }   
    
  }  
  
  if(k > 1) {
    
    df.persons <- do.call(rbind, list.persons)
    df.persons <- ddply(df.persons, .(person), summarise, cost=sum(cost))
    df.persons$cost = round(df.persons$cost, 2)
    
  }
  
  else {
    
    df.persons <- data.frame(  person = "-"
                               , cost = 0)
    
  }
  
  return(df.persons)   
  
} # close GetSummaryByPerson

##########################################################################
# V. Get Summary by Parent
##########################################################################

GetSummaryByParent <- function(num.persons = 5){
  
  
  list.persons = list()
  
  k <- 1
  
  
  if (exists("df.ind.treatment") &&  nrow(df.ind.treatment) > 0)  {
    
    for (i in 1:nrow(df.ind.treatment)){
      
      data <- df.ind.treatment[i, ];
      
      for (j in 1:num.persons){ 
        
        person = as.character(data[paste("person", j, ".ind", sep="")])
        
        #if(person == "Parent"){
          
        if(length(grep("parent", tolower(person)) > 0)) {
          cost_by_person <- data.frame(person = person
                                       , cost = CalculateCostByPersonIndivTreatment(data, j)
                                       , stringsAsFactors = TRUE 
          )
          
          list.persons[[k]] <- cost_by_person
          
          k <- k + 1
        }  
      }  
      
    }   
    
  }  
  
  if (exists("df.gr.treatment") && nrow(df.gr.treatment) > 0) {
    
    for (i in 1:nrow(df.gr.treatment)){
      
      data <- df.gr.treatment[i, ];
      
      for (j in 1:num.persons){ 
        
        person = as.character(data[paste("person", j, ".gr", sep="")])
        
        #if(person == "Parent"){
        if(length(grep("parent", tolower(person)) > 0)) {
           cost_by_person <- data.frame(person = person
                                       , cost = CalculateCostByPersonGroupTreatment(data, j)
                                       , stringsAsFactors = TRUE 
          )
          
          list.persons[[k]] <- cost_by_person
          
          k <- k + 1
        }  
      }  
      
    }   
    
  }  
  
  if(k > 1) {
    
    df.persons <- do.call(rbind, list.persons)
    df.persons <- ddply(df.persons, .(person), summarise, cost=sum(cost))
    df.persons$cost = round(df.persons$cost, 2)
    
  }
  
  else {
    
    df.persons <- data.frame( person = character()
                              , cost = integer()
    )
    
  }
  
  
  return(df.persons)   
  
} # close GetSummaryByParent


##########################################################################
# VI. Get Summary by Medication
##########################################################################

GetSummaryByMedication <- function() {
  
  if (exists("df.med") &&  nrow(df.med) > 0) {
    
    med.component <- paste(df.med$label.med,", ",df.med$frequency.med,"x/day, ", df.med$week.med
                           , ", ", df.med$year.med, sep="")
    
    #  cost <- paste("$", df.med$cost)
    
    cost <- df.med$cost
    df.summary.med <- data.frame(med.component, cost)
    
  } 
  
  else
  {
    
    df.summary.med <- data.frame(  medication = "-"
                                   , cost = 0)
    
  }
  
  
  
  return(df.summary.med)   
  
} # close GetSummaryByMedication

##########################################################################
# VI. Output Summary
##########################################################################

OutputSummary <- function(df.summary, column.names) {
  
  df.summary.out <- as.data.frame(df.summary)
  df.summary.out$cost <- comma(df.summary.out$cost)
  df.summary.out$cost <- paste0("$", df.summary.out$cost)
  names(df.summary.out) <- column.names
  
  return(df.summary.out)
}  