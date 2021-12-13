# R advent day 4
# I don't get the correct answer with this code and I am not sure why
library(matrixStats)
#read in the data
setwd("C:/Users/E.A.Rosenthal/Dropbox/Advent-code")

bingo.calls <- readLines("day4-input.txt", n=1)
bingo.calls <- as.numeric(unlist(strsplit(bingo.calls, ",")))
bingo.calls

#get the boards as an array
boards <- scan("day4-input.txt",skip=2,
               blank.lines.skip = TRUE)
head(boards)
boards.array <- array(boards, dim=c(5,5,length(boards/25)))
boards.array[,,1]
boards.array[,,2] #the boards are transposed from what is in the input

indicator.array <- array(rep(0,length(boards)),
                            dim=c(5,5,length(boards)/25))
colSums(indicator.array[,,1])
rowSums(indicator.array[,,1])


# create a function to check for a call and where it is in a board
call.look <- function(x.array,bingo.call)
{
  i.row = j.col=0
  #if(!is.na(anyValue(x.array,bingo.call)))
  if(!is.na(anyValue(x.array,bingo.call)) &
            anyValue(x.array,bingo.call)==TRUE)
  {
    i.row <- which(rowAnys(x.array,value=bingo.call))
    j.col <- which(colAnys(x.array,value=bingo.call))

  }
  return(c(i.row,j.col))
}
call.look(x.array=boards.array[,,1], bingo.call=17)
call.look(x.array=boards.array[,,1], bingo.call=48)


#function to get rowsum and rowcol and check for total of 5
is.winner <- function(x.array)
{
  win <- FALSE
  sum1 <- rowSums(x.array)
  sum2 <- colSums(x.array)
  if(any(sum1==5) || any(sum2==5))
  {
    win <- TRUE
  }
  return(win)
}
is.winner(indicator.array[,,1])
check <- matrix(c(rep(0,20), rep(1,5)),nrow=5,ncol=5)
is.winner(check)

indicator.array <- array(rep(0,length(boards)),
                         dim=c(5,5,length(boards)/25))
winner <- NA
for(i in 1:length(bingo.calls))
{
  bingo.call <- bingo.calls[i]
  for (j in 1:(length(boards)/25))
  #for (j in 1:10)
    {
    spot <- call.look(boards.array[,,j],bingo.call)
    if( !is.na(spot[1]) & (spot[1]!=0))
      {
      indicator.array[spot[1],spot[2],j] <- 1
      if(is.winner(indicator.array[,,j]))
      {
        winner <- c(i,j)
        print(winner)
        break
      }
    }
  }
  if(length(winner) > 1)
  {
    break
  }
}
winner

keep.board <- boards.array[,,winner[2]]
last.call <- bingo.calls[winner[1]]
unmarked <- sum(keep.board * (1-indicator.array[,,winner[2]]))
score <- last.call * unmarked
score

# check all the indicator arrays and see if there is another one
# that is a winner that I could have missed
check.boards <- rep(NA,length(boards)/25)
for (j in 1:(length(boards)/25))
{
  check.boards[j] <- is.winner(indicator.array[,,j])
}
summary(check.boards)
# there is only one winner
