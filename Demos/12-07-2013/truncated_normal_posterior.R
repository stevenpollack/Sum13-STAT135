library(msm)
library(ggplot2)
set.seed(1234)

### generate mu's and X's conditional on the mu's
alpha <- 0; beta <- 100; sigma2.0 <- 1;
num.of.data.points <- 250;
M <- runif(n=num.of.data.points,min=alpha,max=beta);
### for each M = mu, generate n iid X, such that 
### X | M = mu ~ N(mu, sigma2.0)
X <- sapply(X=M,FUN=function(mu){rnorm(n=num.of.data.points,mean=mu,sd=sigma2.0)});

### organize likelihood and prior for this particular example
likelihood <- function(mu,data) {
  f <- dnorm(x=data,mean=mu,sd=sqrt(sigma2.0))
  prod(f)
}

prior.density <- function(mu,alpha,beta) { dunif(x=mu,min=alpha,max=beta) }

### for a fixed collection of data, X[,1], calculate
### empirical posterior.density
x <- seq(from=alpha,to=beta,length.out=10000)
emp.posterior.density <- sapply(X=x,FUN=function(mu){likelihood(mu,X[,1])}) * prior.density(x,alpha,beta)

dx <- x[2] - x[1]
normalization.constant <- sum(emp.posterior.density*dx)

### create data frame to hold densities (for plotting)
df <- data.frame(x=x, EPD=(emp.posterior.density/normalization.constant))

### calculate posterior hyper-parameters, and
### theoretical posterior.density
mu.star <- mean(X[,1])
sigma2.star <- (sigma2.0)/length(X[,1])
a <- alpha
b <- beta

theoretical.posterior.density <- dtnorm(x=x,mean=mu.star,sd=sqrt(sigma2.star),lower=a,upper=b)

### add theoretical posterior to data frame
df$TPD <- theoretical.posterior.density

### plot overlap, note that mean(X[,1]) = 11.45
ggplot(data=df) + geom_line(aes(x=x,y=EPD),color='red',alpha=0.5,linetype=1) + geom_line(aes(x=x,y=TPD),color='blue',alpha=0.5,linetype=2) + ylab("posterior density") + labs(title="Emperical Posterior (Red) and Theoretical Posterior (Blue, dashed)") # + xlim(c(10,15)) 


