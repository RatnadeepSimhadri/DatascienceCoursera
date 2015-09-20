complete <- function(directory,id=1:332){

dataset <- NULL
countset <- NULL

for( i in id){

	if(i < 10){
	filename <- paste(directory,"/","00",i,".csv",sep="")
	}

	else if ( i >=10 & i <=99){
	filename <- paste(directory,"/","0",i,".csv",sep="")
	}
	else{
	filename <- paste(directory,"/",i,".csv",sep="")
	}

	dataset <- read.csv(file=filename)
	dataset <- dataset [!is.na(dataset $sulfate) & !is.na(dataset $nitrate),]

	if (is.null(countset)){
	countset <- data.frame(id=i,nobs=nrow(dataset))
	}
	else {
	countset <- rbind(countset,c(i,nrow(dataset)))
	}

}
countset

}