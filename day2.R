# R advent day 2
#read in the data
setwd("C:/Users/E.A.Rosenthal/Dropbox/Advent-code")
library(data.table)
library(Hmisc)
day2.dt <- fread("day2-input.txt", header=FALSE)
setnames(day2.dt, c("course","move"))
describe(day2.dt)
horiz <- day2.dt$move[day2.dt$course=="forward"]
up <- -1*day2.dt$move[day2.dt$course=="up"]
down <- day2.dt$move[day2.dt$course=="down"]

horizontal <- sum(horiz)
depth <- sum(up) + sum(down)

horizontal*depth

# when order matters and have to calculate aim
attach(day2.dt)
depth <- 0
horizontal <- 0
aim <- 0
for(i in 1:length(course))
{
  if(course[i]=="up")
  {
    aim <- aim - move[i]
  }
  else if(course[i] == "down")
  {
    aim <- aim + move[i] 
  }
  else
  {
    horizontal <- horizontal + move[i]
    depth <- depth + aim*move[i]
  }
}
horizontal * depth