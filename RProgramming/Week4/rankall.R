rankall<- function(outcome,num="best"){
  outcometable <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.string="")

  if(outcome=="heart attack"){varcol <- 11}
  else if(outcome=="heart failure"){ varcol <- 17}
  else if(outcome=="pneumonia"){varcol <- 23}
  else {stop("invalid outcome")}
  
  states <- unique(outcometable[,c("State")]);
  resultset <- NULL
  
  for(i in states){
  outcomestate <- outcometable[outcometable$State==i,c(2,varcol)]
  colnames(outcomestate)[2]<- "Rate"
  
  outcomestate[,c(2)]<- as.numeric(outcomestate[,c(2)])
  outcomestate <-outcomestate[order(outcomestate[,c(2)],outcomestate[,c(1)],na.last=NA),]
  outcomestate$Rank <- c(1:nrow(outcomestate))
  if(num=="best"){
    outcome <-outcomestate[outcomestate$Rank==1,1]
  }
  else if(num=="worst"){
    outcome <-outcomestate[outcomestate$Rank==nrow(outcomestate),1]
  }
  
  else if(num>nrow(outcomestate)){
    outcome <- NA}
  else {
    outcome <- outcomestate[outcomestate$Rank==num,1]
  }
  
  if(is.null(resultset)){
    resultset <- data.frame(hospital=c(outcome),state=i,stringsAsFactors=F)
  }
  else{
    resultset<- rbind(resultset,c(outcome,i))
  }
  
  }
  
  resultset[order(resultset[,2]),]

  
}