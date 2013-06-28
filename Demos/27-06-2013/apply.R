require(ggplot2)
require(plyr)

n <- 1000 # sample size
sample.chisq <- rchisq(n=n,df=5)
sample.studentst <- rnorm(n=n)/sqrt(sample.chisq/5) #5
sample.normal <- rnorm(n=n,mean=17,sd=6)
sample.exp <- rexp(n=n,rate=1/10)

sample.data <- data.frame(ChiSq = sample.chisq, StudentsT=sample.studentst, Normal=sample.normal, Exp=sample.exp)

## check out SD's of columns
apply(sample.data,MARGIN=2,FUN=sd)
###################################
##################################
###################################
## this can stand-in for a for loop
###################################
sapply(X=1:10,FUN=function(t){print(t)})

## roll 2 dice, over and over
roll2Dice <- function() {
  d1 <- sample(1:6,size=1)
  d2 <- sample(1:6,size=1)
  return(c(d1,d2))  
}

roll2Dice()

simulationStudy <- replicate(n=1000,expr={
  d1 <- sample(1:6,size=1);
  d2 <- sample(1:6,size=1);
  c(d1,d2)
  })

checkForCraps <- function(d1,d2) {
  summ <- sum(c(d1,d2))
  if (summ == 7 || summ == 11) {
    return(T)
  } else {
    return(F)
  }
}

craps <- apply(X=simulationStudy,MARGIN=2,FUN=function(col){checkForCraps(col[1],col[2])})

mean(craps)

