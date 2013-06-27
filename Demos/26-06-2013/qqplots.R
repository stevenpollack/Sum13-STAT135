require(ggplot2)
require(plyr)

n <- 1000 # sample size
sample.chisq <- rchisq(n=n,df=5)
sample.studentst <- rnorm(n=n)/sqrt(sample.chisq/5) #5
sample.normal <- rnorm(n=n,mean=17,sd=6)
sample.exp <- rexp(n=n,rate=1/10)

sample.data <- data.frame(ChiSq = sample.chisq, StudentsT=sample.studentst, Normal=sample.normal, Exp=sample.exp)

head(sample.data)

standardize <- function(sample) {
  return((sample-mean(sample))/sd(sample))
}

standardized.data <- data.frame(apply(X=sample.data,MARGIN=2,FUN=standardize))
standardized.data$x <- seq(-10,10,length.out=n)
standardized.data <- transform(standardized.data,dNorm=dnorm(x=x),pNorm=pnorm(x))

head(standardized.data)

## StudentT vs. Normal -- QQ
ggplot(data=standardized.data) + stat_qq(aes(sample=StudentsT)) + geom_abline(slope=1,intercept=0,color='red') + labs(title="Q-Q plot of Student's T (df=5) vs. Std. Normal")

## StudentT vs. Normal -- densities
ggplot(data=standardized.data) + geom_ribbon(aes(x=x,ymin=0,ymax=dNorm),fill='blue',alpha=0.5) + stat_density(adjust=1.5,aes(x=StudentsT),fill='red',alpha=0.5) + xlim(-3.75,3.75)  + labs(title="kde of Student's T (df = 5, red) over a density of Std. Normal (blue)")

## StudentT vs. Normal -- CDFs
ggplot(data=standardized.data) + stat_ecdf(aes(x=StudentsT),color='blue',alpha=0.5) + geom_line(aes(x=x,y=pNorm),color='red',alpha=0.5) + xlim(-5,5) + labs(title="eCDF of Student's T (df = 5) vs. CDF of Standard Normal")

## ChiSq vs. Normal -- QQ
ggplot(data=standardized.data) + stat_qq(aes(sample=ChiSq)) + geom_abline(slope=1,intercept=0,color='red') + labs(title=expression(paste("Q-Q plot of ", chi[nu=5]^2, " vs. Std. Normal",sep="")),x="Std. Normal Quantiles",y=expression(paste(chi[nu=5]^2, " Quantiles")))

## Chi-Squared vs. Normal -- densities
ggplot(data=standardized.data) + geom_ribbon(aes(x=x,ymin=0,ymax=dNorm),fill='blue',alpha=0.5) + stat_density(adjust=1.5,aes(x=ChiSq),fill='red',alpha=0.5) + xlim(-3.75,3.75)  + labs(title=expression(paste("kde of ", chi[nu=5]^2, " (red) over a density of Std. Normal (blue)", sep="")))

## ChiSq (df=5) vs. Normal -- CDFs
ggplot(data=standardized.data) + stat_ecdf(aes(x=ChiSq),color='blue',alpha=0.5) + geom_line(aes(x=x,y=pNorm),color='red',alpha=0.5) + xlim(-5,5) + labs(title=expression(paste("eCDF of ", chi[nu=5]^2, " over CDF of Standard Normal",sep="")))

### ignore!
#testdf <- ldply(.data=sample.data,.fun=function(col){standardize(col)})
#head(t(testdf))
#str(t(testdf))