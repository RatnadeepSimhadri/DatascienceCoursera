corr <- function(directory, threshold = 0) {

countset <- complete(directory)

countset <- countset[countset$nobs > threshold,]
corvec <-c()
class(corvec)<- "numeric"

	for( i in countset$id){

		if(i < 10){
		filename <- paste(directory,"/","00",i,".csv",sep="")
		}

		else if ( i >=10 & i <=99){
		filename <- paste(directory,"/","0",i,".csv",sep="")
		}
		else{
		filename <- paste(directory,"/",i,".csv",sep="")
		}

		data <- read.csv(filename)
		data <- data [!is.na(data$sulfate) & !is.na(data$nitrate),]
		corvec[length(corvec)+1] <- cor(data$sulfate,data$nitrate)
	}

corvec

}