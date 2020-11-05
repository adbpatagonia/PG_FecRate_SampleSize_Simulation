setwd('C:/Buren_files/PhD/Harp Seals/fecundity_rates/2013 update/analyses/montecarlo-samplesize/weights-for-betaregs')
montcresults <- na.omit(read.csv('montecarlo-disaggregated-results1.csv',header=T))
mont2 <-  na.omit(read.csv('montecarlo-disaggregated-results2.csv',header=T))
mont3 <-  na.omit(read.csv('montecarlo-disaggregated-results3.csv',header=T))
mont4 <-  na.omit(read.csv('montecarlo-disaggregated-results_june2014.csv',header=T))
montcresults <- rbind(montcresults,mont2,mont3,mont4)

conflevel <- 0.05                       # confidence level for the confidence intervals in the plots
 results <- aggregate (fecrate~year+sampmontc, FUN = function(x) { c( mean=mean(x), quantile=quantile(x,probs = c(conflevel/2, 1-conflevel/2)),var=var(x), cv=sd(x)/mean(x))},data=montcresults)
results <- cbind(results[,1:2], as.data.frame(results[,3]))
names(results) <- c( 'year','resample.level','mean.fecrate','ll.fecrate', 'ul.fecrate','var.fecrate','cv.fecrate')

    plotdata <- subset(results,year==2011)
    with(plotdata,plot(resample.level,var.fecrate,type= 'o',pch=16,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))
        plotdata <- subset(results,year==2010)
    with(plotdata,lines(resample.level,var.fecrate,type= 'o',pch=16,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5,col='blue'))
    
    results11 <- subset(results,year==2011)
    results10 <- subset(results,year==2010)
    
    results11$weight <- with(results11,1-(var.fecrate/max(var.fecrate)))
  with(results11,plot(resample.level,weight,type= 'o',pch=16,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))
      results10$weight <- with(results10,1-(var.fecrate/max(var.fecrate)))
  with(results10,lines(resample.level,weight,type= 'o',pch=16,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5,col='blue'))
montc <- subset(montcresults,year==2011)
#write.csv (montc,'montcresults.csv',row.names=F)