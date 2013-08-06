### load findClosestChiSquareModel routie
source('FindClosestChiSquareModel.R')

analyzeGridOfTriplets <- function(I=3:6,J=3:6,K=3:6,num.of.tables=500,min.obs.per.table=1e+05,min.cell.count=10,verbose=F,visualize.results=T,save.results=T){
  ### grid up [3,6]^3 and do 500 data points on each section
  .triplets <- expand.grid(I,J,K)
  
  ### explore best fitting model over various triplets
  .closestChiSquares <- apply(X=.triplets,
                             MARGIN=1,
                             FUN=function(triplet){
                               I <- triplet[1]; J <- triplet[2]
                               K <- triplet[3]
                               findClosestChiSquare(I,J,K,num.of.tables,min.obs.per.table,min.cell.count,verbose,visualize.results,save.results)
                             })
  
  ### compare model chosen with sup-norm vs. expected model
  model.comparison.df <- within(data=.triplets,expr={
    closest.df <- .closestChiSquares
    expected.df <- (I-1)*(J-1)*K
    distance <- expected.df - closest.df
  })
  
  # save all variables that don't start with .
  save.filename <- paste("triplet-analysis-",gsub(pattern="\\s",replacement="-",x=date()),".Rdata", sep="")
  writeLines(paste("Triplet Analysis complete. Saving results in ", save.filename, "...", sep=""))
  save(list=ls(),file=save.filename)
  
  # visualize results, if need be.
  if (visualize.results) { # only works on default grid.
    require(ggplot2)
    
    # need to work out a dynamic way to generate y
    distance.heat.map <- ggplot(data=model.comparison.df) + geom_tile(aes(fill=distance,x=Var1-3, y=rep(c(32:29, 26:23, 20:17, 14:11),each=4)))
    
    show(distance.heat.map)
    ggsave(filename="triplet-analysis.png",plot=distance.heat.map)
  }
  
  return(model.comparison.df)
}

analyzeGridOfTriplets()
