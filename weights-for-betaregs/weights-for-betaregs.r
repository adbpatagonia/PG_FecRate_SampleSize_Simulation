setwd('C:/Buren_files/PhD/Harp Seals/fecundity_rates/2013 update/analyses/montecarlo-samplesize/weights-for-betaregs')
montcresults <- read.csv('montcresults.csv',header=T)
conflevel <- 0.05                       # confidence level for the confidence intervals in the plots
 results <- aggregate (fecrate~sampmontc, FUN = function(x) { c( mean=mean(x), quantile=quantile(x,probs = c(conflevel/2, 1-conflevel/2)),var=var(x), cv=sd(x)/mean(x))},data=montcresults)
results <- cbind(results[,1], as.data.frame(results[,2]))
names(results) <- c( 'resample.level','mean.fecrate','ll.fecrate', 'ul.fecrate','var.fecrate','cv.fecrate')
results$weight <- with(results,1-(var.fecrate/max(var.fecrate)))

with(results,plot(resample.level,weight,type='o',pch=16,cex=.5,xaxp=c(0,150,6),xlim=c(0,150)))
with(subset(results,resample.level>6),subplot(plot(resample.level,weight,type='o',pch=16,cex=.5,xlab='',ylab='',bty='n',main='',xlim=c(0,150),ylim=c(0.2,1),cex.axis=.8,yaxp=c(.2,1,3),xaxp=c(0,150,3)),x=c(40,150),y=c(0.1,0.8)))
#abline(v=9,lty=2)

with(results,plot(resample.level,mean.fecrate,type='o',pch=16,cex=.5,xaxp=c(0,150,6),xlim=c(0,150),ylim=c(0,1)))
with(results,lines(resample.level,ll.fecrate,type='l',pch=16,cex=.5,xaxp=c(0,150,6),xlim=c(0,150)))
with(results,lines(resample.level,ul.fecrate,type='l',pch=16,cex=.5,xaxp=c(0,150,6),xlim=c(0,150)))

with(results,plot(resample.level,var.fecrate,type='o',pch=16,cex=.5,xaxp=c(0,150,6),xlim=c(0,150),ylim=c(0,.1)))
with(results,plot(resample.level,cv.fecrate,type='o',pch=16,cex=.5,xaxp=c(0,150,6),xlim=c(0,150),ylim=c(0,1.5)))

#write.csv(results,'montecarlo-weightsforbetaregs.csv',row.names=F)


