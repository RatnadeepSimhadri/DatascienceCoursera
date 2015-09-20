pollutantmean <- function(directory,pollutant,id=1:332){

dataset <- NULL

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

	if(is.null(dataset)){
	dataset <- read.csv(file=filename)
	}
	else{
	datasettemp <- read.csv(file=filename)
	dataset <- rbind(dataset,datasettemp)
	}

}
mean(dataset[,pollutant],na.rm=TRUE)
}