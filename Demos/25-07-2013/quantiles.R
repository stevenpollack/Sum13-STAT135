D <- c(1,7,52,13,75)
quantile(D,probs=seq(from=0,to=1,length.out=101),type=1)

quantile(c(1,7,13),type=1)
summary(c(1,7,13))

library(ggplot2)
require(quantreg)

set.seed(1234)
some.data <- sample(1:500,size=10,replace=T)

### look at order statistics
sort(some.data)

df <- data.frame(Num=1:10,X=some.data)

# visualize the data
ggplot() + geom_point(data=df, aes(x=X,y=0),color='red')

# look at ECDF
ggplot()  + geom_vline(xintercept=df$X,color='red', alpha=0.5, lty=2) + geom_point(data=df, aes(x=X,y=0),color='red') + stat_ecdf(data=df,aes(x=X)) + labs(list(y=expression(F[n](x)),x="Data",title="Empirical CDF"))

sort(some.data)
quantile(x=some.data,probs=seq(from=0,to=1,length.out=101),type=1)
### quantiles come from Inverse CDF, so we should
### look into ?quantile to choose how quantiles are found

### type=3 takes nearest (even) order statistic as quantile
### type=1 uses inverse ECDF
probs <- seq(from=0,to=1,length.out=11)[-1]
df2 <- data.frame(probs=probs, quants = quantile(some.data,probs=probs,type=1))

### compare order stats to quantiles.

ggplot() + geom_line(data=df2,aes(x=probs,y=quants)) + geom_point(data=df2,aes(x=probs,y=quants),color='red')
### default is type=7
quantile(some.data,probs=seq(from=0,to=1,length.out=11))

ggplot() + stat_boxplot(data=df,aes(x=1,y=X)) + geom_point(data=df, aes(x=1,y=X),color='red')