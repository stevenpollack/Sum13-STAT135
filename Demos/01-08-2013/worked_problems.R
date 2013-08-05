prob.14 <- array(data=c(71,305,217,652,305,869,180,259),dim=c(2,2,2),dimnames=list("DoI"=c("Hi","Low"),"Age"=c("<45",">=45"),"HS"=c("none","some")))

calcX2ForATable <- function(table) {
  rowMargins <- rowSums(table)
  colMargins <- colSums(table)
  totalCount <- sum(table)
  expectedCounts <- rowMargins %o% colMargins / totalCount
  observedCounts <- table
  X2 <- sum((observedCounts - expectedCounts)^2 / expectedCounts)
  return(X2)
}

### for prob 14.a -- figure out conditional independence wrt HS

X2s <- apply(X=prob.14,MARGIN=3,FUN=calcX2ForATable)
X2 <- sum(X2s) ### it has (I-1)(J-1)K dof's
p.value.a <- pchisq(q=X2,df=2,lower.tail=F)

### for prob14.b -- figure this out wrt AGE (margin=2)

X2s.1 <- apply(X=prob.14,MARGIN=2,FUN=calcX2ForATable)
X2.1 <- sum(X2s.1) ### it has (I-1)(J-1)K dof's
p.value.b <- pchisq(q=X2.1,df=2,lower.tail=F)

### prob 19

prob.19 <- array(data=c(12,4,5,9),dim=c(2,2),dimnames=list("Anxiety"=c("Hi","Low"),"Wait"=c("Together","Alone")))
prob.19.rowMargins <- rowSums(prob.19)
prob.19.colMargins <- colSums(prob.19)

### quick way to check out table and margins
rbind(cbind(prob.19,prob.19.rowMargins),c(prob.19.colMargins,sum(prob.19)))

### lets check out null-distribution of N11
dhyper(k=16,n=13,m=17,x=0:17)

### check out CDF under null
phyper(k=16,n=13,m=17,q=0:12)

phyper(k=16,n=13,m=17,q=10:17,lower.tail=F)

rejectionRegion <- function(lwr,upr) {
  a<-phyper(k=16,n=13,m=17,q=lwr)
  b<-phyper(k=16,n=13,m=17,q=upr,lower.tail=F)
  return(a+b)
}

right.tail <- phyper(q=prob.19[1,1],m=17,n=13,k=16,lower.tail=F); right.tail