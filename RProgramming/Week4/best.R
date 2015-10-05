best<- function(state,outcome){
  outcometable <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.string="")
  if(! state %in% outcometable[,c("State")]){stop("invalid state")}
  if(outcome=="heart attack"){varcol <- 13}
  else if(outcome=="heart failure"){ varcol <- 19}
  else if(outcome=="pneumonia"){varcol <- 25}
  else {stop("invalid outcome")}
  
  
  outcomestate <- outcometable[outcometable$State==state,c(2,varcol)]
  
  outcomestate[,c(2)]<- as.numeric(outcomestate[,c(2)])
  outcomestate <-outcomestate[order(outcomestate[,c(2)]),]
  outcomestate[1,1]
}