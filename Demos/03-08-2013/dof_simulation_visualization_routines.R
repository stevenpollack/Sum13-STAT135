### load simulation results
load(file='Demos/03-08-2013/dof_sim-Mon-Aug--5-00:27:52-2013.Rdata')

compareECDF <- function(sim.results,expected.dof) {
  ### helper function to analyze eCDF against
  ### CDF of Chi-square(nu=expected.dof)
  ### sup-norm is taken between the two fxns
  differenceFxn <- function(x) {
    Fn <- ecdf(sim.results)
    abs(Fn(x)-pchisq(q=x,df=expected.dof))
  } 
  max(differenceFxn(sim.results))
}

### analyze results
require(ggplot2)
require(reshape2)

### visualize data within +/- 1 of expected DoF's
sim.study <- within(data=simulation.data, expr={
  X <- seq(from=min(simulation.data),to=max(simulation.data),length.out=simulation.length)})

sim.study <- with(data=sim.study, expr={
  exp.CDF <- ecdf(x=results)
  potential.CDFs <- sapply(X=expected.dof+(-1:1),FUN=function(dof){pchisq(X,df=dof)})
  data.frame(cbind(sim.study,potential.CDFs,exp.CDF(X)))
})

colnames(sim.study) <- c("results", "X", as.character(expected.dof+(-1):1), "Experimental.CDF") 

### melt data for visualization of ecdf's
s3 <- melt(data=sim.study,id.vars=c("results","X","Experimental.CDF"))

ecdf.plot <- ggplot(data=s3) +
  geom_line(aes(x=X,
                y=value,
                color=variable,
                lty=rep(x="Theoretical",times=dim(s3)[1])),
            alpha=1) +
  geom_line(aes(x=X,
                y=Experimental.CDF,
                lty="Experimental"),
            color='purple',
            size=0.75) +
  scale_color_discrete(guide=guide_legend(title="Degrees of\nFreedom")) +
  scale_linetype_manual(values=c(3,1),
                        breaks=c("Theoretical","Experimental"),
                        name="Data Source",
                        labels=c("Theory","Simulation")) +
  labs(list(x="",
            y=expression(P(X <= x)),
            title=substitute(expr={
              paste("CDF for ", chi^{2}, "(", nu, " = ", dfs, " ) and ECDF of Data", sep="")
            },
                             list(dfs=paste(expected.dof+(-1):1,collapse=", "))))) 

show(ecdf.plot)

### check out deviation of eCDF from various other Chi-squared CDF's

ss2 <- with(data=simulation.data,expr={
  df <- 1:(3*expected.dof)
  max.error <- sapply(X=df,FUN=function(df){compareECDF(results,df)})
  data.frame(df=df, max.error=max.error)
})

error.plot <- ggplot(data=ss2) +
  geom_vline(xintercept=expected.dof,color='red') +
  geom_point(aes(x=df,y=max.error,shape="regular"), show_guide=F) +  
  scale_shape_manual(values=c(1,2)) +
  labs(list(x=expression(paste("Degrees of Freedom (",nu, ")",sep="")),
            y=expression(plain(max)[x %in% plain(Data)]*group("|",F[n](x)-F[nu](x),"|")),
            title=paste("Max Error between eCDF and various Chi-Squared CDF's \n Red line at (I-1)(J-1)K DoF")))

show(error.plot)

ggplot(data=simulation.data)+ stat_qq(aes(sample=results),distribution=qchisq,dparams=list(df=expected.dof)) + geom_abline(intercept=0,slope=1,color='red')

### save the plots with simulation specs in prefix
ggsave(plot=ecdf.plot,filename=paste(sim.specs,"ecdf_cdf_overlay.png",sep="-"))
ggsave(plot=error.plot,filename=paste(sim.specs,"error_vs_dof.png",sep="-"))


