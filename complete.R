complete <- function(directory, id){
  
  idvec<-0
  ccvec<-0
  
  for (n in id){
    ##now we correct file id numbers to ensure they are
    ##in the file range and have the proper prefixes 
    if(n<10){
      id<-paste("00",n,sep="")
    } else if(n<100){
      id<-paste("0",n,sep="")
    } else if(n<333){
      id<-n
    } else {
      stop("incorrect monitor id - enter something between 1 and 332")  
    }
    
    fn<-sprintf("%s/%s.csv",directory,id)
    monitor_var <- read.csv(fn, header=TRUE)
    idvec<-c(idvec,n)
    ccount<-sum(complete.cases(monitor_var))
    ccvec<-c(ccvec,ccount)
    
  }
  mydf<-data.frame(idvec[-1],ccvec[-1])
  names(mydf)<-c("id","nobs")
  ##print(c(idvec,ccvec))
  return(mydf)
  
}