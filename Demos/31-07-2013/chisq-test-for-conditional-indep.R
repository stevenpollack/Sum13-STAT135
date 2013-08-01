### let's look at some 3-way contingency tables
### https://onlinecourses.science.psu.edu/stat504/book/export/html/102

death.data <- c(19,0,132,9,11,6,52,97)
death.data.table <- array(data=death.data,dim=c(2,2,2))
dimnames(death.data.table) <- list("Victims Race"=c("White","Black"),"Death Penalty"=c("Yes","No"),"Defendant's Race"=c("White","Black"))

death.data.table

s.low <- matrix(data=c(11,43,42,169),nrow=2,byrow=T)
s.med <- matrix(data=c(14,104,20,132),nrow=2,byrow=T)
s.high <- matrix(data=c(8,196,2,59),nrow=2,byrow=T)
boyscout.data <- array(data=c(s.low,s.med,s.high),dim=c(2,2,3),dimnames=list("Boy Scout"=c("Yes","No"),"Delinquent"=c("Yes","No"),"Socioeconomic Status"=c("Low","Medium","High")))

boyscout.data

### Assuming the courts are racist, let's condition on Defendant's race, and see if Victims Race and Death Penalty are independent.

### so let's look at each 2-way contingency table, separately, and perform a chi-sq test to see if the variables are independent.

calcX2 <- function(table,verbose=F){
  rightMargins <- rowSums(table)
  bottomMargins <- colSums(table)
  total <- sum(table)
  expectedCounts <- outer(X=rightMargins,Y=bottomMargins)/total
  X2 <- sum((table-expectedCounts)^2/expectedCounts)
  if (verbose) {
    print(rbind(cbind(table,rightMargins),c(bottomMargins,total)))
  }
  return(X2)
}

X2 <- apply(X=death.data.table,MARGIN=3,FUN=calcX2,verbose=T)

print(X2)

# each contingency table is 2x2, therefore df=1
pchisq(X2,df=1,lower.tail=F)

# total dfs is (I-1)(J-1)(K-1)
pchisq(sum(X2),df=1,lower.tail=F)

###

boyscout.X2 <- apply(X=boyscout.data,MARGIN=3,FUN=calcX2,verbose=T)

pchisq(boyscout.X2,df=1,lower.tail=F)

### slice according to whether you're a boyscout or not

boyscout.X2.socioecon <- apply(X=boyscout.data,MARGIN=1,FUN=calcX2,verbose=T)

pchisq(boyscout.X2.socioecon,df=2,lower.tail=F)

### slice according to delinquency.

boyscout.X2.delinq <- apply(X=boyscout.data,MARGIN=2,FUN=calcX2,verbose=T)

pchisq(boyscout.X2.delinq,df=2,lower.tail=F)