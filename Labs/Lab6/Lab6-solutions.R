### helper functions
bootstrapStat <- function(population, stats, B=1E5, no.parallel=F, verbose=F) {
  # stats can be a vector of function names. not set up to handle complicated functions. you'll have to right wrappers for multivariate functions
  
  # default to parallel computation
  require(parallel)
  num.of.cores <- if (no.parallel) {1} else {detectCores()}
  
  # routine to be run in parallel
  calcStatsOnOneSample <- function(dummy.var) {
    bs.pop <- sample(x=population,size=length(population),replace=T)
    sapply(X=stats,FUN=function(stat) {do.call(stat,list(bs.pop))})
  }
  
  # measure computation time of Bootstrap procedure
  computation.time <- system.time(expr={
    bs.sampling.dists <- mcmapply(dummy.var=1:B,
                                  mc.cores=num.of.cores,
                                  FUN=calcStatsOnOneSample)
  })
  
  if (verbose) { # report time if verbose
    writeLines(paste(B, "bootstrap statistics generated on", num.of.cores, "cores and spanned", computation.time["elapsed"], "seconds."))
  }

  # shape output to have sampling distributions along columns
  out <- array(data=t(bs.sampling.dists),dim=c(B,length(stats)))
  colnames(out) <- stats

  return(out)
}

twoSampleBootstrapDiffStat <- function(sample1,sample2, stats, B=1E5, no.parallel=F, verbose=F) {
  # stats can be a vector of function names. not set up to handle complicated functions. you'll have to right wrappers for multivariate functions. We are bootstrapping estimators of the form stat(sample1) - stat(sample2).
  
  twoSampleDiff <- function(stat,sample1,sample2) {
    do.call(stat,list(sample1)) - do.call(stat,list(sample2))
  }
  
  # default to parallel computation
  require(parallel)
  num.of.cores <- if (no.parallel) {1} else {detectCores()}
  
  # routine to be run in parallel
  calcStatsOnOneSample <- function(dummy.var) {
    bs.sample1 <- sample(x=sample1,size=length(sample1),replace=T)
    bs.sample2 <- sample(x=sample2,size=length(sample2),replace=T)
    sapply(X=stats,FUN=function(stat) {twoSampleDiff(stat,bs.sample1,bs.sample2)})
  }
  
  # measure computation time of Bootstrap procedure
  computation.time <- system.time(expr={
    bs.sampling.dists <- mcmapply(dummy.var=1:B,
                                  mc.cores=num.of.cores,
                                  FUN=calcStatsOnOneSample)
  })
  
  if (verbose) { # report time if verbose
    writeLines(paste(B, "bootstrap statistics generated on", num.of.cores, "cores and spanned", computation.time["elapsed"], "seconds."))
  }
  
  # shape output to have sampling distributions along columns
  out <- array(data=t(bs.sampling.dists),dim=c(B,length(stats)))
  colnames(out) <- stats
  
  return(out)
}


### 39.

### load in data
.fault.rates.test <- c(676,206,230,256,280,433,337,466,497,512,794,428,452,512)
.fault.rates.control <- c(88,570,605,617,653,2913,924,286,1098,982,2346,321,615,519)

fault.rates <- data.frame(Test=.fault.rates.test,Control=.fault.rates.control)

# take difference = Test - Control (so that we want to see diff < 0)
fault.rates <- transform(fault.rates, Differences=Test-Control)

### 39.a Plot the differences versus the control rate and summarize what you see
require(ggplot2)
plot.39.a <- ggplot(data=fault.rates) + geom_point(aes(y=Differences,x=Control)) + labs(y="Test - Control"); plot.39.a

### there seems to be a strong linear relationship between Differences and Control; Correlaton is -0.9775
cor1 <- with(fault.rates,expr={cor(Differences,Control)}); cor1

### 39.b Calculate the mean difference, its standard deviation and a confidence interval.

differences.stats <- with(fault.rates, {
  n <- length(Control);
  list(mean=mean(Differences),
       "standard deviation"=sqrt((n-1)/n^2)*sd(Differences))
  })

differences.stats

### rather than rely on test stat's with buried assumptions,
### we'll use the bootstrap to build a 95% CI
mean.and.median.bs.smpl.dist <- bootstrapStat(population=fault.rates$Differences,stats=c("mean","median"),verbose=T)

differences.CI <- data.frame(Statistic="mean",Method="bootstrap",value=differences.stats$mean, lwr=0, upr=0,stringsAsFactors=F)

differences.CI[1,4:5] <- 2*as.numeric(differences.CI[1,3]) - quantile(x=mean.and.median.bs.smpl.dist[,1],probs=c(0.975,0.025))


### we could compare this to an interval made from a t-test,
### but before we do, we should check to see if our Differences
### are approximately normal.

# standardize the differences for a QQ-plot
fault.rates <- transform(fault.rates,Stdized.Differences=(Differences-differences.stats[["mean"]])/differences.stats[["standard deviation"]])

qq.plot.39b <- ggplot(data=fault.rates) + stat_qq(aes(sample=Stdized.Differences)) + geom_abline(slope=1,intercept=0,color='red') + labs(list(x="Standard Normal Quantiles",y="Quantiles of Standardized Differences")); qq.plot.39b

hist.plot.39b <- ggplot(data=fault.rates) + geom_histogram(aes(x=Differences, y=..density..),binwidth=250) + geom_density(aes(x=Differences), fill='purple', alpha=0.5, adjust=2, color='white') + labs(y="Density Estimate",title="Histogram and Density estimate of Difference"); hist.plot.39b

### not very normal. We should be cautious when using the t-test derived on page 446...

fault.rate.t.test <- with(fault.rates,expr={
  ### and will give t-based CI for mean of the differences
  t.test(x=Test, y=Control, alternative="two.sided", paired=TRUE, var.equal=FALSE, conf.level=0.95)
});
differences.CI[2,] <- c("mean","t-test",fault.rate.t.test$estimate,fault.rate.t.test$conf.int[1:2])

### not only is the t-Test innapropriate, but it yields a wider CI than the bootstrap.

### 39.c Calculate the median difference and a confidence interval and compare to the previous result.

differences.stats$median <- median(fault.rates$Differences); differences.stats$median

### use exact CI from section 10.4.2, page 395:
### find j such that 1-2*pbinom()
critical.indx <- with(data=fault.rates,expr={
  CI.level <- 0.95
  n <- length(Differences)
  coverage.prob <- 1-2*pbinom(q=0:n,size=n,prob=0.5)
  # coverage.prob is monotonically decreasing
  # so take the smallest index less the level
  # then subtract one since binom is 0 indexed
  min(which(coverage.prob < CI.level))-1
})

fault.rates.median.exact.CI <- with(data=fault.rates,expr={
  j <- critical.indx
  n <- length(Differences)
  sort(Differences)[c(j,n-j+1)]
})

fault.rates.median.exact.CI.coverage.probability <- 1-2*pbinom(critical.indx,size=14,prob=0.5)

differences.CI[3,] <- c("median","exact",NA,fault.rates.median.exact.CI)

fault.rates.median.exact.CI # fairly wide, but coverage is 94%.

### recycle bs.smpl.dist from above
fault.rates.median.bs.CI <- 2*differences.stats$median - quantile(x=mean.and.median.bs.smpl.dist[,2],probs=c(0.975,0.025)); 

differences.CI[4,] <- c("median", "bootstrap", differences.stats$median, fault.rates.median.bs.CI)


ggplot(data=differences.CI) + geom_linerange(aes(x=as.factor(Method),ymin=as.numeric(lwr),ymax=as.numeric(upr))) + geom_hline(yintercept=0,lty=2) + geom_hline(aes(yintercept=as.numeric(value),color=Method)) + facet_grid(.~Statistic,scales="free_x") + labs(x="Method",title="95% Confidence intervals of mean of difference from various methods \nSolid lines represents point estimates")

### this confidence interval is way tighter than the mean's CI's from the bootstrap and t-Test.

### d. It's definitely inappropriate to do t-test as our data is quite clearly non-normal. The good news is that both the t-test and wilcox.test reject, so there's that...

### 40

.weight.gain.field <- c(22.8,10.2,20.8,27.0,19.2,9.0,14.2,19.8,14.5,14.8)
.weight.gain.control <- c(23.5,31.0,19.5,26.2,26.5,25.2,24.5,23.8,27.8,22.0)
weight.gain <- data.frame(Control=.weight.gain.control,Field=.weight.gain.field)

### 40.a display the data graphically with parallel dotplots
require(reshape2)
dot.plot.40a <- ggplot(data=melt(weight.gain,measure.vars=c("Control","Field"))) + geom_point(aes(x=value,y=variable),color='red') + labs(list(y="Magnetic Field",x="Weight gain (g)")) + scale_y_discrete(breaks=c("Field","Control"),labels=c("Present","Absent")); dot.plot.40a

density.plot.40a <- ggplot(data=melt(weight.gain,measure.vars=c("Control","Field"))) + geom_density(aes(x=value,fill=variable),color='white',adjust=1.5,alpha=0.45) + labs(x="Weight gain (g)",title="Density estimates for the two groups of mice") + scale_fill_discrete(name="Magnetic \nField Status",breaks=c("Field","Control"),labels=c("Present","Absent"));

### data within the two samples looks normal "enough"

### 40.b find a 95% confidence interval for the difference of the mean weight gains

### we can use the bootstrap for this

weight.gain.diff.mean <- with(weight.gain, mean(Field)-mean(Control))

two.sample.bs.mean.and.median.smpling.dist <- with(weight.gain, twoSampleBootstrapDiffStat(Field,Control,c("mean","median"),verbose=T));

weight.gain.mean.CI <- 2*weight.gain.diff.mean - quantile(x=two.sample.bs.mean.and.median.smpling.dist[,1],probs=c(0.975,0.025)); weight.gain.mean.CI

weight.gain.CI <- data.frame(Statistic="mean",Method="bootstrap",value=weight.gain.diff.mean, lwr=weight.gain.mean.CI[1], upr=weight.gain.mean.CI[2],stringsAsFactors=F)


### while the samples are supposedly independent, we should check the correlation to see if there are any efficiency gains for treating this like a paired-sample.
weight.gain.cor <- with(data=weight.gain, cor(x=Control,y=Field)); weight.gain.cor

### the correlation is negative and significantly different from zero, so we're best to consider \bar{X} - \bar{Y} as our estimator of \mu_{X}-\mu_{Y}; let's use a t.test with the degree of freedom approximation on page 428 (since we shouldn't assume variances are equal)

weight.gain.t.test <- with(data=weight.gain, t.test(x=Field, y=Control, var.equal=FALSE,alternative="two.sided",conf.level=0.95))

### this yields a confidence interval:

weight.gain.t.test$conf.int[1:2]
weight.gain.CI[2,] <- c("mean","t-test",diff(weight.gain.t.test$estimate[2:1]),weight.gain.t.test$conf.int[1],weight.gain.t.test$conf.int[2])
### 40.c use a t test to asses the statistcal significane of the observed difference. What is the p-value?

weight.gain.t.test$p.value

### p value is slightly below 0.2%.

### 40.d Repeat using a nonparametric test

### we'll use the Mann-Whitney test, which can be called via wilcox.test, see ?wilcox.test.
weight.gain.mann.whitney.test <- with(data=weight.gain, wilcox.test(x=Field,y=Control,alternative="two.sided",paired=FALSE))

weight.gain.mann.whitney.test

### 40.e what is the difference of the median weight gains?
weight.gain.diff.median <- with(weight.gain, median(Field)-median(Control))

### 40.f use the bootstrap to estimate the standard error of the difference of median weight gains

weight.gain.median.sd.error <- sd(two.sample.bs.mean.and.median.smpling.dist[,2]); weight.gain.median.sd.error

### 40.g Form a confidence interval for the difference of median weight gains based on the boostrap approximation to the sampling distribution

weight.gain.median.CI <- 2*weight.gain.diff.median - quantile(x=two.sample.bs.mean.and.median.smpling.dist[,2],probs=c(0.975,0.025)); weight.gain.median.CI

### just for fun
weight.gain.CI[3,] <- c("median","bootstrap",weight.gain.diff.median,weight.gain.median.CI[1],weight.gain.median.CI[2])

facet_labeller <- function(variable,value) {
  o <- list("0"="Mean","1"="Median")
  return(o[value])
}

ggplot(data=weight.gain.CI)  + geom_linerange(aes(x=as.factor(Method),ymin=as.numeric(lwr),ymax=as.numeric(upr))) + geom_hline(aes(yintercept=as.numeric(value),color=Statistic)) + geom_hline(yintercept=0,lty=2) + facet_grid(.~Statistic,scales="free_x") + labs(x="",title="Confidence intervals and point estimates for difference statistics\nDashed line at y=0, colored lines at relevant point estimates")



save.image("Labs/Lab6/solutions.Rdata")