### 9.33

###              |  pre | post
### deaths in CA |  922 | 997
### deaths in AS |  418 | 434

### check out deaths in CA for significance
p.theta <- c(0.5,0.5)
O <- c(922,997)
n <- sum(O)
E <- n*p.theta

GLRT <- 2*sum(O*log(O/E)) # 2.932
X.squared <- sum((O-E)^2/E) # 2.931

df <- 1-0
pchisq(q=GLRT,df=df,lower.tail=F) # 0.0868

### check out deaths in Asian for significance

O2 <- c(418,434)
n2 <- sum(O2)
E2 <- n2*p.theta
GLRT2 <- 2*sum(O2*log(O2/E2)) # 0.30
X.squared2 <- sum((O2-E2)^2/E2) # 0.30

df <- 1-0
pchisq(q=GLRT2,df=df,lower.tail=F) # 0.5835

#####################
### 9.37

### load in number of deaths per month
O <- c(1668,1407,1370,1309,1341,1338,1406,1446,1332,1363,1410,1526)

### assume that each month is equally likely:
p.0 <- rep(x=1/12,times=12)

n <- sum(O) # 16,916
E <- n*p.0 # ~ 1411 / mo.

GLRT <- 2*sum(O*log(O/E)) # 76.50
X.squared <- sum((O-E)^2/E) # 79.01

df <- length(O)-1 # no free parameters in p(\theta)

# p-value of GLRT 
pchisq(q=GLRT,df=df,lower.tail=F) # 6.98e-12 (basically 0)

# p-value of X.squared
pchisq(q=X.squared,df=df,lower.tail=F) # 2.29e-12 (basically 0)

### essentially, you'd be lead to believe the null isn't right...
### check out (O-E):
plot(1:12,O-E)
abline(a=0,b=0,col='red')
### greatest deviations from model happens in December and January, but also in April and May (rainy months)... So model isn't really taking seasons into account (obviously).

### check out data, graphically
plot(1:12,O)

######################
### build helper function
######################

calcGLRT.and.X2 <- function(O,p.0,df=NULL) {
  ### assuming null hypothesis completely specifices p.0
  n <- sum(O)
  E <- n*p.0
  GLRT <- 2*sum(O*log(O/E))
  X2 <- sum((O-E)^2/E)
  df <- if (is.null(df)) {
        length(O)-1
        } else {
          df
        }
  p.value1 <- pchisq(q=GLRT,df=df,lower.tail=F)
  p.value2 <- pchisq(q=X2,df=df,lower.tail=F)
  return(list(GLRT=GLRT,GLRT.p.value=p.value1, X.squared=X2, X2.p.value=p.value2) )
}


#####################
### 9.43.a

O <- c(9207,8743)
p.0 <- c(0.5,0.5)
calcGLRT.and.X2(O,p.0)
# GLRT and X.squared are ~ 11.99 with p-values ~ 0.0005
# so we'd most likely reject the null-hypothesis.

### 9.43.b

O <- c(100,524,1080,1126,655,105)
### if all coins were fair 
p.0 <- dbinom(x=0:5,size=5,prob=0.5)
calcGLRT.and.X2(O,p.0)
# GLRT ~ 20.89, X.squared ~ 21.57 => reject

### 9.43.c


### using GLRT
df <- 1
p.hat <- x/N
GLRT.stat <-  -2*N*sum(p.hat*log(p.0/p.hat)) # 90.44

### check out Pearson's chi-squared stat
X.squared <- sum((x-N*p.0)^2/(N*p.0)) # 90.37

### check prob that we experience a more extreme
### statistic than GLRT.stat
pchisq(q=GLRT.stat,df=df,lower.tail=F) # 1.9e-21