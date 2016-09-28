#Christian Acosta R Homework Week 10

collegedata=read.csv(file="C:/Users/caa5000/Documents/GitHub/ds710assignment9/CollegeDataEvenCleaner.csv")
head(collegedata)
summary(collegedata)
attach(collegedata)

#Use length and which to determine # schools that have 
#per student instructional expenditure higher than out-of-state tuition.
#Out.of.state.tuition & 
length(which(Instructional.expenditure.per.student>Out.of.state.tuition)) #457
match("Instructional.expenditure.per.student", names(collegedata)) #col 34
match("Out.of.state.tuition", names(collegedata)) #col 23

nrow(collegedata) #1302 rows.
collegedata
#Use control flow to answer same question above. 
simpleExpenseFrame <- data.frame(collegedata[23], collegedata[34]) #lower amt of data and make it easier.
simpleExpenseFrame
noNAsimpleExpenseFrame <- na.omit(simpleExpenseFrame)#use na.omit
noNAsimpleExpenseFrame #Contains only the rows with full information. 
length(noNAsimpleExpenseFrame)
head(noNAsimpleExpenseFrame)
OutTuition <- noNAsimpleExpenseFrame[,1] #assign var to first col new matrix.
OutTuition
instructExpense <- noNAsimpleExpenseFrame[,2] #assign var to second col new matrix.
instructExpense
numcounter <- 0 

#much simplified data frame - not sure if this was what homework intended
#but should put timer to test vs. which!
instructorExpensiveStuff <- function()
{numcounter <- 0 
  for (i in 1:length(instructExpense)) #Iterate over entire array.
  {
    if(instructExpense[i] > OutTuition[i])
    {
      numcounter = numcounter+1
    }
    
  }
  numcounter #457
}
?system.time()

#install.packages("microbenchmark")
library("microbenchmark")
#Use system.time to compare running times. 
timeWhichTest <- function()
{
  length(which(Instructional.expenditure.per.student>Out.of.state.tuition))
}
system.time(replicate(10000, timeWhichTest())) #User:.06, System:0?, Elapsed:.06
system.time(replicate(10000, instructorExpensiveStuff())) #user:9.89, system:0, elapsed:9.93
#Which statement MUCH faster. 
microbenchmark(timeWhichTest(), instructorExpensiveStuff())


#Perform micro-benchmarking on median running times of 3 different mean methods.
#1. use apply() and build in function mean() to calculate mean of each column.
#sapply(collegedata, mean, na.rm=TRUE) #applies mean function to each column in matrix.
collegeNum <- collegedata[,sapply(collegedata, is.numeric)] #convert to numeric.
apply(collegeNum, 2, FUN=mean, na.rm=TRUE)

#2. Use apply, write a function mymean() that takes in sum of all non-NA values and 
# in column divides by # of non-missing values.
?mean
mymean <- function(x) #take in sum of each column no NAs. 
{
  #x is sum, y is counts. 
  sum(x, na.rm=TRUE)/length(which(!is.na(x)))
}

apply(collegeNum, 2, mymean) #Not sure why is.numeric wrapping college data wouldn't work. 

?apply

#3. Use a for loop to iterate over numeric columns, and a nested for loop to iterate
#over values within the column. 
avgLoop <- function(collegeNum) #take in dataset.
{
  numCols <- ncol(collegeNum)
  numCols #34
  numRows <- nrow(collegeNum)
  numRows #1302
  colMeans = numeric(34) #this works confirmed. 
  for (c in 1:34) #iterate over each column. numCols=34.
  {
    colTotal = 0 #used to sum total for columns. 
    notna = 0 #count the number that ARE NOT NA.
    for (r in 1:1302) #iterate over each row. 1302 total
    {
      if (!is.na(collegeNum[r,c])) #If NOT na, add the value to running total. 
        {  
          colTotal <- colTotal + collegeNum[r,c] #add the current index so long as not NA
          notna <- notna+1
        }
    }
    colMeans[c] <- colTotal/notna #set colMeans to total of
    #1302 iterations, divided by length of the column which is NOT NA.

  }
  colMeans
}

loopedMeans <- avgLoop(collegeNum)
loopedMeans #first try only looping over once. 

#microbenchmark the three different things...
microbenchmark(apply(collegeNum, 2, FUN=mean, na.rm=TRUE), mymean(collegeNum), avgLoop(collegeNum))
microbenchmark(apply(collegeNum, 2, FUN=mean, na.rm=TRUE), mymean(collegeNum))
microbenchmark(avgLoop(collegeNum))
system.time(replicate(1, avgLoop(collegeNum)))
