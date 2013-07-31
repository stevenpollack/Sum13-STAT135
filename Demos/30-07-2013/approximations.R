### investigate relationship between Normal and Chi-Square
set.seed(1234)

### 1. We're told that X ~ Chisq(1) is just X = Z^2, where Z ~ N(0,1), let's test that.
library(ggplot2)
normal.sample <- rnorm(n=1000)
normal.squared <- normal.sample^2
x.sequence <- seq(from=0,to=max(normal.squared),length.out=1000)
chi.sq.density <- dchisq(x=x.sequence,df=1)

### store everything in a dataframe
df <- data.frame(X=x.sequence,N=normal.sample,N2=normal.squared,X2.density=chi.sq.density)

### verify normal
ggplot(data=df) + geom_histogram(aes(x=N),binwidth=0.2)

### check out normal.squared
N2.plot <- ggplot(data=df) + geom_histogram(aes(x=N2,y=..density..),binwidth=0.025)
show(N2.plot)

### now overlay chisq(1) density
N2.plot <- N2.plot + geom_line(aes(x=X,y=X2.density),color='red',size=1)
show(N2.plot)
### zoom in
show(N2.plot + xlim(c(0,3)))

### 2. We're told that for (independent) X1, X2, X3 ~ Chisq(1), Y=X1+X2+X3 ~ Chisq(3). Let's investigate!

X1 <- rchisq(n=1000,df=1)
X2 <- rchisq(n=1000,df=1)
X3 <- rchisq(n=1000,df=1)
Y <- X1+X2+X3

df2 <- data.frame(X=seq(from=0,to=max(Y),length.out=1000),Y=Y)
df2 <- within(df2, {density=dchisq(x=X,df=3)})

ggplot(data=df2) + geom_histogram(aes(x=Y,y=..density..),binwidth=0.33) + geom_line(aes(x=X,y=density),color='red',size=1)

### 3. We can approximate X ~ Poisson(lambda) with Y ~ N(lambda, sqrt(lambda))!

lambda.small <- 1
lambda.medium <- 25
lambda.large <- 100
lambdas <- c(lambda.small,lambda.medium,lambda.large)

### check out density of Poisson for various lambdas, as well as normal approximation.

### 4. Check out result #3 on board

W1 <- rchisq(n=1000,df=4)
W2 <- rchisq(n=1000,df=8)
W3 <- rchisq(n=1000,df=5)
Y2 <- W1+W2+W3 # should have df=17

df3 <- data.frame(X=seq(from=0,to=max(Y2),length.out=1000),Y=Y2)
df3 <- within(df3, {density=dchisq(x=X,df=17)})

ggplot(data=df3) + geom_histogram(aes(x=Y,y=..density..),binwidth=2.5) + geom_line(aes(x=X,y=density),color='red',size=1)