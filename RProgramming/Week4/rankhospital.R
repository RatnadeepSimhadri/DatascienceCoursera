rankhospital<- function(state,outcome,num="best"){
  outcometable <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.string="")
  if(! state %in% outcometable[,c("State")]){stop("invalid state")}
  if(outcome=="heart attack"){varcol <- 11}
  else if(outcome=="heart failure"){ varcol <- 17}
  else if(outcome=="pneumonia"){varcol <- 23}
  else {stop("invalid outcome")}
  
  
  outcomestate <- outcometable[outcometable$State==state,c(2,varcol)]
  colnames(outcomestate)[2]<- "Rate"
  
  outcomestate[,c(2)]<- as.numeric(outcomestate[,c(2)])
  outcomestate <-outcomestate[order(outcomestate[,c(2)],outcomestate[,c(1)],na.last=NA),]
  outcomestate$Rank <- c(1:nrow(outcomestate))
  if(num=="best"){
    outcomestate[outcomestate$Rank==1,1]
  }
  else if(num=="worst"){
    outcomestate[outcomestate$Rank==nrow(outcomestate),1]
  }
  
  else if(num>nrow(outcomestate)){NA}
  else {
    outcomestate[outcomestate$Rank==num,1]
  }
  
}