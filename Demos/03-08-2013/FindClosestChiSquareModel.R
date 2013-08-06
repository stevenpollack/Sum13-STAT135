### visualization routines
visualizeECDFvsDoF <- function(simulation.data, dof.range, I, J, K) {
  require(ggplot2)
  require(reshape2)
  
  ### visualize data within +/- 1 of expected DoF's
  sim.study <- within(data=simulation.data,expr={
    X <- seq(from=min(results),
             to=max(results),
             length.out=length(results))
    })
  
  sim.study <- with(data=sim.study, expr={
    exp.CDF <- ecdf(x=results)
    potential.CDFs <- sapply(X=dof.range,
                             FUN=function(dof){pchisq(X,df=dof)})
    data.frame(cbind(sim.study,potential.CDFs,exp.CDF(X)))
  })
  
  colnames(sim.study) <- c("results", "X", as.character(dof.range), "Experimental.CDF") 
  
  ### melt data for visualization of ecdf's
  s3 <- melt(data=sim.study,id.vars=c("results","X","Experimental.CDF"))

  ecdf.plot <- ggplot(data=s3) +
    geom_line(aes(x=X,y=value,color=variable,lty=rep(x="Theoretical",times=length(X))),alpha=1) +
    geom_line(aes(x=X,y=Experimental.CDF,lty="Experimental"),color='purple', size=0.75) +
    scale_color_discrete(guide=guide_legend(title="Degrees of\nFreedom")) + 
    scale_linetype_manual(values=c(3,1), breaks=c("Theoretical","Experimental"), name="Data Source", labels=c("Theory","Simulation")) +
    labs(list(x="", y=expression(P(X <= x)), title=substitute(expr={ paste("CDF for ", chi^{2}, "(", nu, " = ", dfs, " ) and ECDF of Data", sep="")}, list(dfs=paste(dof.range,collapse=", "))))) 
  
  ### display plot
  show(ecdf.plot)
  
  ### save plot
  num.of.tables <- dim(simulation.data)[1]
  sim.specs <- paste(I,J,K,num.of.tables,sep="-")
  ggsave(filename=paste(sim.specs, "ecdf_cdf_overlay.png", sep="-"), plot=ecdf.plot)
}
visualizeModelDistances <- function(dfs, model.distances, I, J, K, num.of.tables) {
  require(ggplot2)
  
  expected.dof <- (I-1)*(J-1)*K
  
  error.plot <- ggplot(data=data.frame(df=dfs,max.error=model.distances)) +
    geom_vline(xintercept=expected.dof,color='red') +
    geom_point(aes(x=df,y=max.error,shape="regular"), show_guide=F) +  
    scale_shape_manual(values=c(1,2)) +
    labs(list(x=expression(paste("Degrees of Freedom (",nu, ")",sep="")),
              y=expression(plain(max)[x %in% plain(Data)]*group("|",F[n](x)-F[nu](x),"|")),
              title=paste("Max Error between eCDF and various Chi-Squared CDF's \n Red line at (I-1)(J-1)K =", (I-1)*(J-1)*K, "DoF")))
  
  # display plot
  show(error.plot)
  
  # save plot
  sim.specs <- paste(I,J,K,num.of.tables,sep="-")
  ggsave(filename=paste(sim.specs,"error_vs_dof.png", sep="-"), plot=error.plot)
}

### actual simulation code
findClosestChiSquare <- function(I,J,K, num.of.tables=500, min.obs.per.table=100000, min.cell.count=10, verbose=F, visualize.results=F, save.results=T) {
  ### 
  expected.dof <- as.numeric((I-1)*(J-1)*K)
  
  writeLines(paste("Preparing a simulation for a", I, "x", J, "x", K, "contingency table...\nThis should yield a test statistic with", expected.dof, "degrees of freedom..."))
  
  ### simulation helper functions, build
  ### inside function to not pollute workspace
  .generateOnePoint <- function(I,J,K) {
    ### scheme is simple:
    
    Z <- sample(x=1:K,size=1)
    X <- floor(I*rbeta(n=1,shape1=2+Z/(K+1),shape2=2))
    Y <- floor(J*rbeta(n=1,shape1=2,shape2=2+Z/(K+1)))
    
    ### package these variables into a triplet
    return(c(X,Y,Z))
  }
  .positivityCheck <- function(contingency.table) {
    ### check to see if input contingency table has minimum cell count
    ### so as to not break any positivity assumptions for Chi-Square test
    ### returns TRUE if the check is good
    if (min(contingency.table) <= min.cell.count) {
      if (verbose) {
        writeLines(paste("Minimum cell count is below specified min.cell.count of",min.cell.count))
      }
      return(F)
    } else {
      if (verbose) {
        writeLines(paste("All cell counts above",min.cell.count))
      }
      return(T)
    }
  }
  .generateContingencyTable <- function(I,J,K) {
    ### set a high number of observations to assure asymptotics
    ### as well as no positivity issues
    
    .genTable <- function(I,J,K) {
      simulation.data <- replicate(n=min.obs.per.table,expr={.generateOnePoint(I,J,K)})
      simulation.df <- data.frame(X=simulation.data[1,],Y=simulation.data[2,],Z=simulation.data[3,])
      return(table(simulation.df))
    }
    
    ## Check that generated table passes positivity check
    simulated.contingency.table <- .genTable(I,J,K)
    repeat {
      if ( .positivityCheck(simulated.contingency.table) ) { # table has no issues
        break
      } else {
        simulated.contingency.table <- simulated.contingency.table + .genTable(I,J,K)
      }
    }
    
    return(simulated.contingency.table) 
  }
  .calculatePearsonsX2 <- function(two.way.table) {
    ### this function calculates X2 for an IxJ table
    row.margins <- rowSums(two.way.table)
    col.margins <- colSums(two.way.table)
    total.counts <- sum(two.way.table)
    expected.counts <- row.margins %o% col.margins / total.counts
    X2 <- sum( (two.way.table - expected.counts)^2 / expected.counts )
    return(X2)
  }
  .calculateCumulativePearsons <- function(three.way.table) {
    # calculate X2 for each level of Z
    X2 <- apply(X=three.way.table,MARGIN=3,FUN=.calculatePearsonsX2)
    # Cumulative Pearsons is sum of X2's
    return(sum(X2))
  }
  
  ### run simulation
  require(parallel)
  num.of.cores <- detectCores()
  
  writeLines(paste("Commencing a", num.of.tables, "point simulation, on", num.of.cores, "cores..."))
  
  sim.time <- system.time(expr={
    .cumulative.Pearsons <- mclapply(X=1:num.of.tables,
                                    mc.cores=num.of.cores,
                                    FUN=function(dummy.var){
                                      ### generate random table, then find its X^2 stat
                                      random.table <- .generateContingencyTable(I,J,K)
                                      .calculateCumulativePearsons(random.table)
                                    })
    emp.dist <- data.frame(results=unlist(.cumulative.Pearsons))
  })
  
  ### report simulation stats (wall-clock time)
  writeLines(paste(num.of.tables, " point simulation required ", sim.time["elapsed"] %/% 60, ":", round(sim.time["elapsed"] %% 60,digits=2), " wall-clock time", sep=""))
  
  ### determine which Chi-Square CDF best-fits eCDF
  closest.model <- with(data=emp.dist, expr={
    ### "best-fit" is determined through use of sup-norm
    .supNorm <- function(nu, eCDF, q) {
      return(max(abs(eCDF(q)-pchisq(q,df=nu) )))
    }
    
    candidate.dfs <- 1:(3*expected.dof) # search beyond 3 times predicted
    Fn <- ecdf(results) # ecdf is a closure, see help file

    distances <- sapply(X=candidate.dfs, FUN=function(df){.supNorm(df,Fn,results)})
    
    if (visualize.results) {
      visualizeModelDistances(candidate.dfs, distances, I, J, K, num.of.tables)
    }
    
    which.min(distances)
  })
  
  ### save data
  sim.specs <- paste(I,J,K,num.of.tables,sep="-")
  
  if (save.results) { # save all variables without . in front
    image.filename <- paste(sim.specs,gsub(pattern="\\s",replacement="-",x=date()),".Rdata",sep="")
    
    writeLines(paste("Saving workspace image to ", image.filename, "...", sep=""))
    
    save(list=ls(),file=image.filename)
  }
  
  if (visualize.results) {
    visualizeECDFvsDoF(emp.dist,expected.dof+(-1:1),I,J,K)
  }
  
  ### finally, return closest.Model
  return(closest.model)
}
