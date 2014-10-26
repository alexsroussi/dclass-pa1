corr <- function(directory, threshold = 0) {
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
	
	complete <- function(id = 1:332) {
	
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
	
	idsoverthreshold <- function(){
		comp <- complete()
		comp[comp$nobs>=threshold,"id"]
	}
	
	ids<-idsoverthreshold()
	
	v<-numeric()
	for(id in ids){
		fn<-filename(id)
		dat<-read.csv(fn)
		sulf<-dat[,"sulfate"]
		nitr<-dat[,"nitrate"]
		
		v<-c(v,cor(nitr,sulf,use="na.or.complete"))
	}
	v[!is.na(v)]
}
