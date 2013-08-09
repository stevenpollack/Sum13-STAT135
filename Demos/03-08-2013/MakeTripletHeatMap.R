load("~/grizzlybear/dof_simulation/triplet-analysis-Tue-Aug--6-19:49:51-2013.Rdata")

colnames(model.comparison.df)[1:3] <- c("I","J","K")
model.comparison.df <- within(model.comparison.df,expr={
  expected.df <- (I-1)*(J-1)*K
  distance <- expected.df - closest.df
})

y <- sapply(X=K,FUN=function(z) {rep(seq(from=z,to=2*z,length.out=length(J)),each=length(I))})

### break apart [0,1] into |K| groups of size |K| with a unit space in between, then 
k <- length(K); j <- length(J)
y <- rep(seq(from=0,to=1,length.out=k^2+k-1)[-seq(from=k+1,to=k^2+k+1,by=k+1)],each=j)

require(ggplot2)


# testGrob <- lapply(X=paste("J=",J,sep=""),FUN=function(label){grobTree(textGrob(label=label,name="side",just="left"))})
# 
# tGrob <- grobTree(textGrob(label=paste("J=", rev(J), sep="",collapse="\n"), x=unit(1, "strwidth", "J=3"), y=unit(4, "strheight", "J=3"), just=c("left","top"),name="shit",gp=gpar(col='grey40')))

distance.heat.map <- ggplot(data=model.comparison.df) +
  geom_tile(aes(fill=as.factor(distance), x=I, y=y),color='white') +
  labs(list(x="",y="",title=expression( paste( (I-1)*(J-1)*K-plain(DoF),"(best fitting model)", sep="") ))) +
  scale_fill_discrete(breaks=c(1,0,-1),labels=c(1,0,-1),name="Difference",h.start=50,c=50,direction=-1) +
  scale_y_continuous(breaks=seq(from=0,to=1,length.out=2*k+1)[2*(1:k)],labels=paste("K=",K,sep="")) +
  coord_cartesian(ylim=c(-0.05,1.05),xlim=c(min(I)-0.55,max(I)+0.55)) +
  scale_x_discrete(breaks=I,labels=paste("I=",I,sep="")) #+
  #annotation_custom(grob=tGrob,xmin=max(I)+0.4,xmax=max(I)+4,ymin=0.06,ymax=0.06)

distance.heat.map
ggsave(filename="triplet-heat-map.png",plot=distance.heat.map)
# g <- ggplotGrob(distance.heat.map)
# g$layout$clip[g$layout$name=="panel"] <- "off"
# grid.draw(g)
# 
# 
# plegend <- ggplot() +
#   #geom_blank() +
#   #geom_segment(data=d2, aes(x=2, xend=0, y=y, yend=y), 
#                #arrow=arrow(length=unit(2,"mm"), type="closed")) +
#   geom_text(aes(x=2.5,y=y[c(1,5,9,13)],label=paste("J=",J,sep="")), hjust=0) +
#   scale_x_continuous(expand=c(0,0)) +
#   guides(colour="none")+
#   theme_minimal() + theme(line=element_blank(),
#                           text=element_blank(),
#                           panel.background=element_rect(fill="grey95",
#                                                         linetype=2))
# 
# 
# # extract the panel only, we don't need the rest
# gl <- gtable_filter(ggplotGrob(plegend), "panel")
# 
# # add a cell next to the main plot panel, and insert gl there
# g <- ggplotGrob(distance.heat.map)
# index <- subset(g$layout, name == "panel")
# g <- gtable_add_cols(g, unit(1, "strwidth", "line # 1") + unit(1, "cm"))
# g <- gtable_add_grob(g, gl, t = index$t, l=ncol(g), 
#                      b=index$b, r=ncol(g))
# grid.newpage()
# grid.draw(g)
