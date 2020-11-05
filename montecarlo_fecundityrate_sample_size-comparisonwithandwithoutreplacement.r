

results <- read.csv('montecarlo-fecundity-sample-size-replacement.csv',header=T)
results$replacement <- 'Y'
results.woreplac <- read.csv('montecarlo-fecundity-sample-size.csv',header=T)
results.woreplac$replacement <- 'N'

allresults <- rbind(results,results.woreplac)


jpeg(filename = "ppt_with&withoutreplacement-samplesize-var-1000iteration.jpg", width = 1000, height = 1000, units = "px", pointsize = 17, quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo"), antialias = c("default"))
  par(mfrow=c(2,2))
  for (y in c(2006,2009,2010,2011)){
    plotdata <- subset(allresults,year==y)
    with(subset(plotdata,replacement=='N'),plot(resample.level,var.fecrate,type= 'o',pch=16,main=y,xaxp=c(10,100,9),ylim=c(0,0.03),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))
    with(subset(plotdata,replacement=='Y'),lines(resample.level,var.fecrate,type= 'o',pch=16,main=y,xaxp=c(10,100,9),col='red',cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))
        }
    legend(60,0.03,legend=c('with replacement','without replacement'),col=c('red','black'),lty=1,lwd=2,pch=16,cex=1,bty='n')        
    dev.off()
    
jpeg(filename = "ppt_bootstrap-samplesize-var-1000iteration.jpg", width = 1000, height = 1000, units = "px", pointsize = 17, quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo"), antialias = c("default"))
  par(mfrow=c(2,2))
  for (y in c(2006,2009,2010,2011)){
    plotdata <- subset(allresults,year==y)
    with(subset(plotdata,replacement=='Y'),plot(resample.level,var.fecrate,type= 'o',pch=16,main=y,xaxp=c(10,100,9),ylim=c(0,0.03),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))

        }
    dev.off()

    
jpeg(filename = "ppt_bootstrap-samplesize-2006-1000iteration.jpg", width = 1000, height = 500, units = "px", pointsize = 17, quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo"), antialias = c("default"))
  par(mfrow=c(1,2))
  for (y in c(2006)){
    plotdata <- subset(allresults,year==y & replacement=='Y')
    with(plotdata,plot(resample.level,mean.fecrate,type= 'o',ylim=c(0,0.8),pch=16,main=y,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))
    with(plotdata,lines(resample.level,ll.fecrate,lty=2))
    with(plotdata,lines(resample.level,ul.fecrate,lty=2))
    abline(h=fecrate$fecrate[which(fecrate$year==y)], col='red')
    with(plotdata,plot(resample.level,var.fecrate,type= 'o',pch=16,main=y,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))
    }
dev.off()