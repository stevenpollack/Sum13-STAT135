library(ggplot2) # for plotting
library(reshape2) # incase I need to melt.

### 46 --- bodytemp.txt
### a) For both males and females, make scatterplots of heart rate versus body
### temperature. Comment on the relationship or lack thereof.

### load bodytemp.txt
bodytemp.data <- read.table(file='./Labs/Lab4/bodytemp.txt',header=T)

bodytemp.data <- within(bodytemp.data,genderFactor<-factor(gender,labels=c("Male","Female")))

ggplot(data=bodytemp.data,aes(y=rate,x=temperature)) + geom_point() + facet_wrap(~genderFactor) + labs(list(x="Temperature (Fahrenheit)",y="Heart Rate (bpm)"))

### there doesn't seem to be much in the way of a relationship between heart rate and body temperature

### b) Quantify the strengths of the relationships by calculating
### Pearson and rank correlation coefficients.

male.indices <- with(bodytemp.data, {which(gender == 1)})
female.indices <- -male.indices

### calculate various correlation coefficients
male.cors <- with(bodytemp.data, {
  list(pearson=cor(temperature[male.indices],rate[male.indices]),spearman=cor(temperature[male.indices],rate[male.indices],method="spearman")) 
})
 
print(male.cors)

female.cors <- with(bodytemp.data, {
  list(pearson=cor(temperature[female.indices],rate[female.indices]),spearman=cor(temperature[female.indices],rate[female.indices],method="spearman")) 
})

print(female.cors)

### both pairs of correlation coefficients are pretty abysmal; this indicates that there isn't much of a relationship between heart rate and body temperature.

### c) Does the relationship for males appear to be the same as that for females? Ex amine this question graphically, by making a scatterplot showing both females and males and identifying females and males by different plotting symbols.

ggplot(data=bodytemp.data,aes(y=rate,x=temperature,color=genderFactor,shape=genderFactor)) + geom_point() + labs(list(shape="Gender",color="Gender",x="Temperature (Fahrenheit)",y="Heart Rate (bpm)"))

### both male and female sexes seem to exhibit the same pattern of behaviour

##################################################################
##################################################################

### 47

### load data
oldfaithful.data <- read.table(file='Labs/Lab4/oldfaithful.txt',header=T)

### a) Use histograms of durations and time intervals as well as other graphical methods to examine the fidelity of Old Faithful, and summarize your findings.

ggplot(data=oldfaithful.data,aes(x=DURATION)) + geom_histogram(binwidth=0.1) + geom_point(aes(y=0),color='red') + labs(list(x="Eruption duration (minutes)"))

ggplot(data=oldfaithful.data,aes(x=DURATION)) + stat_ecdf() + labs(list(x="Eruption duration (minutes)",y=expression(F[n](x)),title="Emprical CDF of Eruption duration"))

ggplot(data=oldfaithful.data,aes(x=INTERVAL)) + geom_histogram(binwidth=2) + geom_point(aes(y=0),color='red') + labs(list(x="Time between eruptions (minutes)"))

ggplot(data=oldfaithful.data,aes(x=INTERVAL)) + stat_ecdf() + labs(list(x="Time between eruptions (minutes)",y=expression(F[n](x)),title="Emprical CDF of Eruption intervals"))

### It appears that we're likely to see eruptions lasting (either) between 1 minutes and 2.5 minutes OR 3.5 minutes and 4.5 minutes. Also, eruptions intervals also seem to be somewhat bi-modal with a predominate likelihood to see intervals between 70 and 85 minutes, but it's also lilely to see intervals between 50 and 60 minutes.

### b) Is there a relationship between the durations of eruptions and the time intervals between them?

ggplot(data=oldfaithful.data,aes(x=INTERVAL,y=DURATION)) + geom_point()

### it appears as if there's a (positive) direct relationship between eruption interval and duration. Indeed, the Pearson corr is ~ 0.86.

print(with(oldfaithful.data,cor(INTERVAL,DURATION))) # 0.8584

### 48.
lottery.data <- read.table(file="Labs/Lab4/1970lottery.txt",header=T)

### a) Plot draft number versus day number. Do you see any trend?

ggplot(data=lottery.data) + geom_point(aes(x=Day_of_year,y=Draft_No)) + labs(list(x="Day of the year",y="Draft number"))

### I don't see a trend.

### b) Calculate the Pearson and rank correlation coefficients. What do they suggest?

correlation.coefs <- with(lottery.data, {
  list(pearson=cor(Day_of_year,Draft_No),spearman=cor(Day_of_year,Draft_No,method="spearman"))
})

print(correlation.coefs)

### the negative correlations indicate that as Day of Year increases, Draft Number should decrease. But it should be noted that the magnitude of the coefficients, overall, are fairly small...

### c) Is the correlation statistically significant? One way to assess this is via a permutation test. Randomly permute the draft numbers and find the correlation of this random permutation with the day numbers. Do this 100 times and see how many of the resulting correlation coefficients exceed the one observed in the data. If you are not satisfied with 100 times, do it 1,000 times.

isPermutationLarger <- function(calculatedCorrelation) {
  draft_no <- sample(1:366)
  pearson.cor <- cor(draft_no,1:366)
  spearman.cor <- cor(draft_no,1:366,method="spearman")
  #print(abs(as.numeric(calculatedCorrelation)))
  #print(abs(c(pearson.cor, spearman.cor)))
  abs(as.numeric(calculatedCorrelation)) < abs(c(pearson.cor, spearman.cor))
}

simulation <- replicate(n=1000,expr={isPermutationLarger(correlation.coefs)})

print(rowMeans(simulation)) # both means are effectively 0

### turns out what we're seeing is INCREDIBLY significant! In fact, the chance we see something more extreme is ~ 0.

### d) Make parallel boxplots of the draft numbers by month. Do you see any pattern?
melted.lottery <- melt(lottery.data,measure.vars="Draft_No",id.vars="Month")
melted.lottery <- within(melted.lottery,{
  Month2 <- factor(x=Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=T)
})
ggplot(data=melted.lottery,aes(x=Month2,y=value)) + geom_boxplot() + labs(list(x="Month",y="Draft Number"))

### the trend is fairly alarming: as we progress through the year, the median of the draft numbers decreases, (as does the distance between the 25th and 75th percentiles). 