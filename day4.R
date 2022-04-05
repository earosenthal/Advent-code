# R advent day 4
# Originally did not work because in the functions, I need to use the variable 
# names (e.g., anyValue(x=x.array,value=bingo.call) instead of 
# anyValue(x.array,bingo.call))
# 
library(matrixStats)
#read in the data
setwd("C:/Users/E.A.Rosenthal/Dropbox/Advent-code")

bingo.calls <- readLines("day4-input.txt", n=1)
bingo.calls <- as.numeric(unlist(strsplit(bingo.calls, ",")))
bingo.calls

#get the boards as an array
boards <- scan("day4-input.txt",skip=2,
               blank.lines.skip = TRUE)
head(boards,25)
boards.array <- array(boards, dim=c(5,5,length(boards)/25))
boards.array[,,1]
boards.array[,,2] #the boards are transposed from what is in the input


indicator.array <- array(rep(0,length(boards)),
                            dim=c(5,5,length(boards)/25))
colSums(indicator.array[,,1])
rowSums(indicator.array[,,1])
indicator.array[,,25]

# create a function to check for a call and where it is in a board
call.look <- function(x.array,bingo.call)
{
  i.row = j.col=0
  if(is.na(anyValue(x=x.array,value=bingo.call)))
     {
       i.row=j.row=-1
  }
  #if(!is.na(anyValue(x.array,bingo.call)))
  if(!is.na(anyValue(x=x.array,value=bingo.call)) &
            anyValue(x=x.array,value=bingo.call)==TRUE)
  {
    i.row <- which(rowAnys(x=x.array,value=bingo.call))
    j.col <- which(colAnys(x=x.array,value=bingo.call))

  }
  return(c(i.row,j.col))
}
call.look(x.array=boards.array[,,1], bingo.call=17)
call.look(x.array=boards.array[,,1], bingo.call=48)


#function to get rowsum and rowcol and check for total of 5
is.winner <- function(x.array)
{
  win <- FALSE
  sum1 <- rowSums(x=x.array)
  sum2 <- colSums(x=x.array)
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
    spot <- call.look(x.array=boards.array[,,j],bingo.call=bingo.call)
#    if(!is.na(spot[1]) & spot[1]==-1)
#    { #can I figure out why I would get missing values?
#      winner <- c(i,j)
#      break
#    }
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


# which board is last
indicator.array <- array(rep(0,length(boards)),
                         dim=c(5,5,length(boards)/25))
did.win <- rep(0,length(boards)/25)
winner <- NA
for(i in 1:length(bingo.calls))
{
  bingo.call <- bingo.calls[i]
  for (j in 1:(length(boards)/25))
    #for (j in 1:10)
  {
    spot <- call.look(x.array=boards.array[,,j],bingo.call=bingo.call)

    if( !is.na(spot[1]) & (spot[1]!=0))
    {
      indicator.array[spot[1],spot[2],j] <- 1
      if(did.win[j]==1)
      {
        next
      }
      if(is.winner(indicator.array[,,j]))
      {
        winner <- c(i,j)
        did.win[j] <- 1
        #print(winner)
        #break
      }
    }
  }
  if(sum(did.win)==length(did.win))
     {
    break
      winner
  }
}
winner
loser <- winner

keep.board <- boards.array[,,winner[2]]
last.call <- bingo.calls[winner[1]]
unmarked <- sum(keep.board * (1-indicator.array[,,winner[2]]))
score <- last.call * unmarked
score



