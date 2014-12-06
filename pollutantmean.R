pollutantmean <- function(directory, pollutant, id=1:332) {
  ##first let's clean the arguments. 
  ##To start we'll check if the pollutant argument is 
  ##one of the two existing pollutants 
  if(pollutant!="sulfate" && pollutant!="nitrate"){
    stop("incorrect pollutant - use either sulfate or nitrate")
  }
  
  ##initialize the count variables
  sum1<-0
  count1<-0
  
  ##the for loop reads each file sequentially
  for (n in id){
    ##now we correct file id numbers to ensure they are
    ##in the file range and have the proper prefixes 
    if(n<10){
      n<-paste("00",n,sep="")
    } else if(n<100){
      n<-paste("0",n,sep="")
    } else if(n<333){
      n<-n
    } else {
      stop("incorrect monitor id - enter something between 1 and 332")  
    }
    fn<-sprintf("%s/%s.csv",directory,n)
    monitor_var <- read.csv(fn, header=TRUE)
    sum1<-sum1+(sum(monitor_var[,pollutant],na.rm=TRUE))
    count1<-count1+((nrow(monitor_var)-(sum(is.na(monitor_var[,pollutant])))))
  }
  ##rounding to three decimal places to match sample output
  my_mean<-round((sum1/count1),digits=3)
  return(my_mean)

}