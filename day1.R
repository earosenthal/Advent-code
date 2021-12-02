# R advent day 1
# use the diff function
#read in the data
setwd("C:/Users/E.A.Rosenthal/Dropbox/Advent-code")
library(data.table)
day1.dt <- fread("day1-input.txt", header=FALSE)

diffs.lag1 <- diff(day1.dt$V1,lag=1)
length(diffs.lag1[diffs.lag1>0])

#window of size 3. Can I do matrix multiplication.
#or is it easy enough to just make a loop?
window.sums <- rep(0,length(day1.dt$V1)-2)
for(i in 1:(length(day1.dt$V1)-2))
{
  window.sums[i] <- day1.dt$V1[i] + day1.dt$V1[i+1] + day1.dt$V1[i+2]
}

diffs.lag2 <- diff(window.sums,lag=1)
length(diffs.lag2[diffs.lag2>0])


x<- c(1,3,4,9)
diff(x, lag=1)
diff(x, lag=2)
diff(x, differences=2)


