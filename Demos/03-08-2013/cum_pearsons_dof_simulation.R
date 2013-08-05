### set up simulation parameters
random.table.size <- sample(x=3:6,size=3,replace=T)
I <- random.table.size[1]; J <- random.table.size[2]; K <- random.table.size[3]
# I <- 4; J <- 6; K <-5;
expected.dof <- (I-1)*(J-1)*K

### set up log-file
log.filename <- paste(gsub(pattern="\\s",replacement="-",x=date()),".out",sep="")
cat(paste("Starting Log:", date(), "\n"),file=log.filename,append=T)
cat(paste("Preparing a simulation for a", I, "x", J, "x", K, "contingency table...\nThis should yield a test statistic with", expected.dof, "degrees of freedom...\n"),file=log.filename,append=T)

### simulation helper functions
generateOnePoint <- function(I,J,K) {
  ### scheme is simple:
  ### --Z: discrete uniform from [1,K]
  ### --X: truncated Geo(p=Z/k+1)
  ### --Y: truncated, Pois(r=Z)
  
  Z <- sample(x=1:K,size=1)
  X <- floor(I*rbeta(n=1,shape1=2+Z/(K+1),shape2=2))
  Y <- floor(J*rbeta(n=1,shape1=2,shape2=2+Z/(K+1)))
  
  ### package these variables into a triplet
  return(c(X,Y,Z))
}
positivity.check <- function(contingency.table, threshold=0, verbose=F) {
  ### check to see if input contingency table has minimum cell count
  ### so as to not break any positivity assumptions for Chi-Square test
  ### returns TRUE if the check is good
  if (min(contingency.table) <= threshold) {
    if (verbose) {writeLines(paste("Minimum cell count is below specified threshold of",threshold))}
    return(F)
  } else {
    if (verbose) {writeLines(paste("All cell counts above",threshold))}
    return(T)
  }
}
generateContingencyTable <- function(I,J,K,num.of.observations=100000,threshold=10) {
  ### set a high number of observations to assure asymptotics
  ### as well as no positivity issues
  
  genTable <- function(I,J,K,n.obs=num.of.observations) {
    simulation.data <- replicate(n=n.obs,expr={generateOnePoint(I,J,K)})
    simulation.df <- data.frame(X=simulation.data[1,],Y=simulation.data[2,],Z=simulation.data[3,])
    return(table(simulation.df))
  }
  
  ## Check that generated table passes positivity check
  simulated.contingency.table <- genTable(I,J,K)
  repeat {
    if ( positivity.check(simulated.contingency.table,threshold,verbose=FALSE) ) { # table has no issues
      break
    } else {
      simulated.contingency.table <- simulated.contingency.table + genTable(I,J,K)
    }
  }
  
  return(simulated.contingency.table) 
}

checkForIndependence <- function(table,x,y,z) {
  total <- sum(table)
  joint.prob <- table[x,y,z]
  factor.prob <- sum(table[x,,z])*sum(table[,y,z])/sum(table[,,z])
  list(lhs=joint.prob,rhs=factor.prob,diff=abs(joint.prob-factor.prob)/total)
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
require(parallel)

num.of.cores <- detectCores()
simulation.length <- 175

cat(paste("Commencing a", simulation.length, "point simulation, on", num.of.cores, "cores...\n"),file=log.filename,append=T)

sim.time <- system.time(expr={
  cumulative.Pearsons <- mclapply(X=1:simulation.length,
                                  mc.cores=num.of.cores,
                                  FUN=function(dummy.var){
                                    ### generate random table, then find its X^2 stat
                                    random.table <- generateContingencyTable(I,J,K)
                                    calculateCumulativePearsons(random.table)
           })
  simulation.data <- data.frame(results=unlist(cumulative.Pearsons))
  })

### finish up and report stats
sim.specs <- paste(I,J,K,simulation.length,sep="-")
image.filename <- paste("dof_sim", sim.specs, gsub(pattern=".out",replacement=".Rdata",x=log.filename), sep="-")

cat(paste(simulation.length, "point simulation required", round(sim.time["elapsed"]/60,digits=2), "minutes...\nSaving image to", image.filename, "\n"),file=log.filename,append=T)

save.image(file=image.filename)

