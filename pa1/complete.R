complete <- function(directory, id = 1:332) {
	
	filename <- function(i) {
		tries <- c(toString(i), paste("0",i,sep=""), paste("00",i,sep=""))
		result<-character(0)
		for(try in tries) {
			exist <- list.files(directory, pattern=paste("^",try, ".csv", sep=""), full.names=TRUE)
			if (length(exist) == 1) {
				result <- exist
				break
			}
		}
		result
	}
	
	completecase <- function(fn) {
		tmpds <- read.csv(fn)
		cc <- complete.cases(tmpds)
		length(cc[cc==T])
	}
	
	dat<-data.frame()
	for(i in id){
		fn <- filename(i)
		if(length(fn) == 1) {
			dat<-rbind(dat, c(i,completecase(fn)))
		}			
	}
	colnames(dat)<-c("id","nobs")
	dat
}
