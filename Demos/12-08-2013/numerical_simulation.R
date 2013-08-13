theta0 <- 1
alpha <- 0.05
n <- 10
  
genTestStat <- function(n,theta0) {
  X <- rexp(n,theta0)
  Xbar <- mean(X)
  return(Xbar*exp(-theta0*Xbar))
}

### generate a ton of test statistics
### to get an idea of sample distribution

sampling.dist <- replicate(n=1E5,expr={genTestStat(n,theta0)})
eCDF <- ecdf(x=sampling.dist)

### look at CDF to get an idea of where 
### c.alpha might be

plot(eCDF)

determineCAlpha <- function(eCDF,alpha) {
  f <- function(x,alpha) { eCDF(x) - alpha }
  uniroot(f, c(0,1),alpha=alpha)$root
}

c.alpha <- determineCAlpha(eCDF,alpha)

### sanity check:
eCDF(c.alpha) # approximate 0.05