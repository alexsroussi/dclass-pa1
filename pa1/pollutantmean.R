pollutantmean <- function(directory, pollutant, id = 1:332) {
	
	readallfiles <- function(){
		v<-numeric()
		for(i in id){
			fn <- filename(i)
			if(length(fn) == 1) {
				v<-c(v, readfile(fn))
			}			
		}
		v
	}
	
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
	
	readfile <- function(fn) {
		tmpds <- read.csv(fn)
		v<-tmpds[,pollutant]
		v[!is.na(v)]
	}
	
	mean(readallfiles())
}
