### load findClosestChiSquareModel routie
source('FindClosestChiSquareModel.R')

analyzeGridOfTriplets <- function(I=3:6,J=3:6,K=3:6,visualize.results=F){
  ### grid up [3,6]^3 and do 500 data points on each section
  triplets <- expand.grid(I,J,K)
  
  ### explore best fitting model over various triplets
  closestChiSquares <- apply(X=triplets,
                             MARGIN=1,
                             FUN=function(triplet){
                               I <- triplet[1]; J <- triplet[2]
                               K <- triplet[3]
                               findClosestChiSquare(I,J,K)
                             })
  
  ### compare model chosen with sup-norm vs. expected model
  model.comparison.df <- within(data=triplets,expr={
    closest.df <- closestChiSquares
    expected.df <- (I-1)*(J-1)*K
    distance <- expected.df - closest.df
  })
  
  if (visualize.results) {
    require(ggplot2)
    
    distance.heat.map <- ggplot(data=model.comparison.df) + geom_tile(aes(fill=distance,x=I-3, y=rep(c(32:29, 26:23, 20:17, 14:11),each=4)))
    
    #show(distance.heat.map)
    ggsave(filename="triplet-analysis.png",plot=distance.heat.map)
  }
  
  save.image(file="triplet-analysis.Rdata")
  return(model.comparison.df)
}

