## hit CTRL + ENTER to run lines of code through console
random.sample <- runif(n=100,min=80,max=200)
summary(random.sample)
ggplot(data=data.frame(wizzle=random.sample)) + geom_histogram(aes(x=wizzle),binwidth=12,color='red',fill='blue') + labs(list(title="histogram title",x="x-axis title"))