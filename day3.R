#R Advent day3
setwd("C:/Users/E.A.Rosenthal/Dropbox/Advent-code")

library(data.table)
library(tidyr)
day3.dt <- fread("day3-input.txt", header=FALSE,keepLeadingZeros=TRUE,
                 colClasses = "character"  )
str(day3.dt)
char.length <- nchar(day3.dt[1,])
#for some reason, the last column gets lost and the first column is empty
# so I added in a column
split.col.names <- (paste0("col",seq(0:char.length)))
#separate out, but fill on left because leading zeros are discared
split.df <- as.data.frame(day3.dt) %>%
    separate(V1,sep="",
             into=split.col.names)
split.dt <- data.table(split.df)
str(split.dt)
split.dt[,(split.col.names):=lapply(.SD,as.numeric),.SDcols=split.col.names]
str(split.dt)
split.dt[,col1:=NULL]


gamma <- (paste0(round(colSums(split.dt)/length(day3.dt$V1)),collapse=""))
epsilon <- (paste0(round(1-colSums(split.dt)/length(day3.dt$V1)),collapse=""))

gamma <- round(colSums(split.dt)/length(day3.dt$V1))
epsilon <- round(1-colSums(split.dt)/length(day3.dt$V1))


bin.mult <- 2^c((length(gamma)-1):0) #need to put in reverse direction
gamma.dec <- sum(bin.mult*gamma)
epsilon.dec <- sum(bin.mult*epsilon)

gamma.dec * epsilon.dec

#part 2 (I am still working on this part)
subset.dt <- split.dt[][split.dt$col2==gamma[1]]
for(i in 2:length(split.dt))
  {
  j <- i+1
  col.name <- paste0("col",j)
  gamma.calc <- round(colSums(subset.dt)/length(subset.dt$col2))
  subset.dt[,IND:=ifelse(.SD==gamma.calc[i],1,0),.SDcols=col.name]
  subset.dt <- subset.dt[][subset.dt$IND==1]
  if(length(subset.dt$col2)==1)
  {
    #i <- length(split.dt)+1
    output <- subset.dt
    output[,IND:=i]
  }
}
output
output[,IND:=NULL]
o2.rate <- output

subset.dt <- split.dt[][split.dt$col2==epsilon[1]]
for(i in 2:length(split.dt))
#  i<- 3
{
  j <- i+1
  col.name <- paste0("col",j)
  epsilon.calc <- round(1-colSums(subset.dt)/length(subset.dt$col2))
  subset.dt[,IND:=ifelse(.SD==epsilon.calc[i],1,0),.SDcols=col.name]
  subset.dt <- subset.dt[][subset.dt$IND==1]
  if(length(subset.dt$col2)==1)
  {
    #i <- length(split.dt)+1
    output <- subset.dt
    output[,IND:=i]
  }
}
output
output[,IND:=NULL]
co2.rate <- output

o2.dec <- sum(bin.mult*o2.rate)
co2.dec <- sum(bin.mult*co2.rate)

o2.dec*co2.dec
