corr <- function(directory, threshold=0){
  
  idvec<-0
  
  for (n in 1:332){
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
    
    df<-complete(directory,n)
    if(df$nobs>threshold){
      fn<-sprintf("%s/%s.csv",directory,id)
      monitor_var <- read.csv(fn, header=TRUE)
      xtp<-round(cor(monitor_var[,2],monitor_var[,3],use="complete.obs"),digits=5)
      idvec<-c(idvec,xtp)
    }
  }
  
  ##print(c(idvec,ccvec))
  return(idvec[-1])
  
}