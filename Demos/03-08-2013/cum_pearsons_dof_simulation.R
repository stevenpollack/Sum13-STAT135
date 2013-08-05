### set up simulation parameters
random.table.size <- sample(x=3:6,size=3,replace=T)
I <- random.table.size[1]; J <- random.table.size[2]; K <- random.table.size[3]
expected.dof <- (I-1)*(J-1)*K

### set up log-file
log.filename <- paste(gsub(pattern="\\s",replacement="-",x=date()),".out",sep="")
cat(paste("Starting Log:", date(), "\n"),file=log.filename,append=T)
cat(paste("Preparing a simulation for a", I, "x", J, "x", K, "contingency table...\nThis should yield a test statistic with", expected.dof, "degrees of freedom...\n"),file=log.filename,append=T)

### simulation helper functions
generateOnePoint <- function(I,J,K) {
  ### scheme is simple:
  ### --Z: discrete uniform from [1,K]
  ### --X: floored normal whose SD depends on Z,
  ###      X is forced to lie in [0,I-1]
  ### --Y: truncated, floored poisson with rate=Z,
  ###      Y is not to exceed J
  
  Z <- sample(x=1:K,size=1)
  
  X.raw <- floor(rnorm(mean=I/2,sd=sqrt(Z),n=1))
  ### X should be in [0,I-1]
  X <- if ( X.raw < 0 ) { 0 } else if ( X.raw > I-1 ) { I-1 } else { X.raw }
  
  Y.raw <- floor(1.5*rpois(lambda=Z,n=1))
  ### Y should be in [0,J], truncate if need be
  Y <- if (Y.raw > J) {J} else {Y.raw}
  
  ### package these variables into a triplet
  return(c(X,Y,Z))
}
positivity.check <- function(contingency.table, threshold=0, verbose=F) {
  ### check to see if input contingency table has minimum cell count
  ### so as to not break any positivity assumptions for Chi-Square test
  ### returns TRUE if the check is good
  if (min(contingency.table) <= threshold) {
    if (verbose) {writeLines("Minimum cell count is below specified threshold of",threshold)}
    return(F)
  } else {
    if (verbose) {writeLines("All cell counts above",threshold)}
    return(T)
  }
}
generateContingencyTable <- function(I,J,K,num.of.observations=100000,threshold=0) {
  ### set a high number of observations to assure asymptotics
  ### as well as no positivity issues
  
  genTable <- function(I,J,K,n.obs=num.of.observations) {
    simulation.data <- replicate(n=n.obs,expr={generateOnePoint(I,J,K)})
    simulation.df <- data.frame(X=simulation.data[1,],Y=simulation.data[2,],Z=simulation.data[3,])
    return(table(simulation.df))
  }
  
  ### Check that generated table passes positivity check
  repeat {
    simulated.contingency.table <- genTable(I,J,K)
    if ( positivity.check(simulated.contingency.table,threshold) ) { # table has no issues
      break
    }
  }
  
  return(simulated.contingency.table) 
}
calculatePearsonsX2 <- function(two.way.table) {
  ### this function calculates X2 for an IxJ table
  row.margins <- rowSums(two.way.table)
  col.margins <- colSums(two.way.table)
  total.counts <- sum(two.way.table)
  expected.counts <- row.margins %o% col.margins / total.counts
  X2 <- sum( (two.way.table - expected.counts)^2 / expected.counts )
  return(X2)
}
calculateCumulativePearsons <- function(three.way.table) {
  # calculate X2 for each level of Z
  X2 <- apply(X=three.way.table,MARGIN=3,FUN=calculatePearsonsX2)
  # Cumulative Pearsons is sum of X2's
  return(sum(X2))
}

### run simulation
require(parallel) # I know this may be redundant

num.of.cores <- detectCores()
simulation.length <- 175

cat(paste("Commencing a", simulation.length, "point simulation, on", num.of.cores, "cores...\n"),file=log.filename,append=T)

sim.time <- system.time(expr={
  simulation.data <- data.frame(results=unlist(mclapply(X=1:simulation.length,
                                      mc.cores=num.of.cores,
                                      FUN=function(dummy.var) {
  simulated.contingency.table <- generateContingencyTable(I,J,K,threshold=10)
  calculateCumulativePearsons(simulated.contingency.table)
}
                                                         )
                                                )
                                 )
  })

### finish up and report stats
image.filename <- paste("dof_sim", gsub(pattern=".out",replacement=".Rdata",x=log.filename), sep="-")

cat(paste(simulation.length, "point simulation required", round(sim.time["elapsed"]/60,digits=2), "minutes...\nSaving image to", image.filename),file=log.filename,append=T)

save.image(file=image.filename)

