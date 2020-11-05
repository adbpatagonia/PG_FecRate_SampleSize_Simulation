
rm(list=c(ls(all=TRUE)))    # remove all variables from workspace 
s1 <- Sys.time()
require(stringr)
op <- par(no.readonly = TRUE) # the whole list of settable par's

############ BEGIN USER INPUT #####################
inpath<-'C:/Buren_files/PhD/Harp Seals/fecundity_rates/2013 update/analyses/montecarlo-samplesize' # folder where input file is
infile<-'ovar-mat.csv'     # name of input file

nmontc <- 1000                        # number of monte carlo iterations
sampmontc <- seq(from=10,to=100,by=5)   # resample levels. It is now a sequence from 20 to 100, in increments of 5 seals
conflevel <- 0.05                       # confidence level for the confidence intervals in the plots
############ END USER INPUT #####################


 setwd(inpath)
ovar <-read.csv(infile,header=T)
ovar <- subset(ovar, Month>9 | Month<3)
ovar <- ovar[,c('ID','maturity')]
ovar$year <- as.numeric(substr(ovar$ID, 1, 4))


ovarsummary <- aggregate(ovar$ID, by=list(ovar$year, ovar$maturity), FUN=length)
names(ovarsummary) <- c( 'year','maturity','n')

fecrate <- aggregate(n~year,FUN=sum,data=subset(ovarsummary,maturity>1))
names(fecrate) <- c('year','mature')
pregnant <- as.vector(aggregate(n~year,FUN=sum,data=subset(ovarsummary,maturity==2))[2])
fecrate <- cbind(fecrate,pregnant)
names(fecrate) <- c('year','mature','pregnant')
fecrate$fecrate <- with(fecrate,pregnant/mature)


montcresults <- data.frame(matrix(data=numeric(), ncol=4, nrow=nmontc*length(sampmontc)*length(unique(ovar$year)),dimnames = list(NULL,c("year", "sampmontc", "iteration","fecrate"))))
i <- 0
for (y in unique(ovar$year)){
  for (smontc in sampmontc){
    for (j in 1:nmontc){
      i <- i +1
      dat <- subset(ovar, year==y)
      montcdat <- data.frame(ID=sample(dat$ID,smontc,replace=FALSE))
      montcdat <- merge(montcdat,dat)
      montcdatsummary <- aggregate(montcdat$ID, by=list(montcdat$maturity), FUN=length)
      names(montcdatsummary) <- c( 'maturity','n')
      montcmature <- sum(montcdatsummary$n[which(montcdatsummary$maturity>1)])
      montcpregnant <- ifelse(2 %in%  montcdatsummary$maturity,montcdatsummary$n[which(montcdatsummary$maturity==2)],0)   # if no pregnant seals were resampled then assign a zero value
      montcresults$year[i] <- y
      montcresults$sampmontc[i] <- smontc
      montcresults$iteration[i] <- j
      montcresults$fecrate[i] <- montcpregnant/ montcmature
      }
    }
  }

 
 results <- aggregate (fecrate~year+sampmontc, FUN = function(x) { c( mean=mean(x), quantile=quantile(x,probs = c(conflevel/2, 1-conflevel/2)),var=var(x), cv=sd(x)/mean(x))},data=montcresults)
results <- cbind(results[,1:2], as.data.frame(results[,3]))
names(results) <- c( 'year','resample.level','mean.fecrate','ll.fecrate', 'ul.fecrate','var.fecrate','cv.fecrate')   

write.csv(montcresults, 'montecarlo-disaggregated-results.csv',row.names=F)
write.csv(results, 'montecarlo-aggregated-results.csv',row.names=F)

ovar <-ovar[order(ovar$year),]


jpeg(filename = "montecarlo-samplesize-2006_2009-1000iteration.jpg", width = 692, height = 692, units = "px", pointsize = 15, quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo"), antialias = c("default"))
  par(mfrow=c(2,2))
  for (y in c(2006,2009)){
    plotdata <- subset(results,year==y)
    with(plotdata,plot(resample.level,mean.fecrate,type= 'o',ylim=c(0,0.8),pch=16,main=y,xaxp=c(10,100,9)))
    with(plotdata,lines(resample.level,ll.fecrate,lty=2))  
    with(plotdata,lines(resample.level,ul.fecrate,lty=2))    
    abline(h=fecrate$fecrate[which(fecrate$year==y)], col='red')
    with(plotdata,plot(resample.level,var.fecrate,type= 'o',pch=16,main=y,xaxp=c(10,100,9)))  
    } 
dev.off()

jpeg(filename = "montecarlo-samplesize-2010_2011-1000iteration.jpg", width = 692, height = 692, units = "px", pointsize = 15, quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo"), antialias = c("default"))
  par(mfrow=c(2,2))
  for (y in c(2010,2011)){
    plotdata <- subset(results,year==y)
    with(plotdata,plot(resample.level,mean.fecrate,type= 'o',ylim=c(0,0.8),pch=16,main=y,xaxp=c(10,100,9)))
    with(plotdata,lines(resample.level,ll.fecrate,lty=2))  
    with(plotdata,lines(resample.level,ul.fecrate,lty=2))    
    abline(h=fecrate$fecrate[which(fecrate$year==y)], col='red')
    with(plotdata,plot(resample.level,var.fecrate,type= 'o',pch=16,main=y,xaxp=c(10,100,9)))  
    }   
dev.off()



jpeg(filename = "ppt_montecarlo-samplesize-2006_2009-1000iteration.jpg.jpg", width = 1000, height = 1000, units = "px", pointsize = 17, quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo"), antialias = c("default"))
  par(mfrow=c(2,2))
  for (y in c(2006,2009)){
    plotdata <- subset(results,year==y)
    with(plotdata,plot(resample.level,mean.fecrate,type= 'o',ylim=c(0,0.8),pch=16,main=y,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))
    with(plotdata,lines(resample.level,ll.fecrate,lty=2))  
    with(plotdata,lines(resample.level,ul.fecrate,lty=2))    
    abline(h=fecrate$fecrate[which(fecrate$year==y)], col='red')
    with(plotdata,plot(resample.level,var.fecrate,type= 'o',pch=16,main=y,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))  
    } 
dev.off()

jpeg(filename = "ppt_montecarlo-samplesize-2010_2011-1000iteration.jpg.jpg", width = 1000, height = 1000, units = "px", pointsize = 17, quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo"), antialias = c("default"))
  par(mfrow=c(2,2))
  for (y in c(2010,2011)){
    plotdata <- subset(results,year==y)
    with(plotdata,plot(resample.level,mean.fecrate,type= 'o',ylim=c(0,0.8),pch=16,main=y,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))
    with(plotdata,lines(resample.level,ll.fecrate,lty=2))  
    with(plotdata,lines(resample.level,ul.fecrate,lty=2))    
    abline(h=fecrate$fecrate[which(fecrate$year==y)], col='red')
    with(plotdata,plot(resample.level,var.fecrate,type= 'o',pch=16,main=y,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))  
    }   
dev.off()



jpeg(filename = "montecarlo-samplesize-cv-1000iteration.jpg", width = 692, height = 692, units = "px", pointsize = 15, quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo"), antialias = c("default"))
  par(mfrow=c(2,2))
  for (y in c(2006,2009,2010,2011)){
    plotdata <- subset(results,year==y)    
    with(plotdata,plot(resample.level,cv.fecrate,type= 'o',pch=16,ylim=c(0,0.7),main=y,xaxp=c(10,100,9)))  
    } 
dev.off()

jpeg(filename = "ppt_montecarlo-samplesize-cv-1000iteration.jpg", width = 1000, height = 1000, units = "px", pointsize = 17, quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo"), antialias = c("default"))
  par(mfrow=c(2,2))
  for (y in c(2006,2009,2010,2011)){
    plotdata <- subset(results,year==y)    
    with(plotdata,plot(resample.level,cv.fecrate,type= 'o',pch=16,main=y,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))  
    } 
dev.off()


jpeg(filename = "ppt_montecarlo-samplesize-var-1000iteration.jpg", width = 1000, height = 1000, units = "px", pointsize = 17, quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo"), antialias = c("default"))
  par(mfrow=c(2,2))
  for (y in c(2006,2009,2010,2011)){
    plotdata <- subset(results,year==y)    
    with(plotdata,plot(resample.level,var.fecrate,type= 'o',pch=16,main=y,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))  
    } 
dev.off()

jpeg(filename = "ppt_montecarlo-samplesize-2006-1000iteration.jpg.jpg", width = 1000, height = 500, units = "px", pointsize = 17, quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo"), antialias = c("default"))
  par(mfrow=c(1,2))
  for (y in c(2006)){
    plotdata <- subset(results,year==y)
    with(plotdata,plot(resample.level,mean.fecrate,type= 'o',ylim=c(0,0.8),pch=16,main=y,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))
    with(plotdata,lines(resample.level,ll.fecrate,lty=2))  
    with(plotdata,lines(resample.level,ul.fecrate,lty=2))    
    abline(h=fecrate$fecrate[which(fecrate$year==y)], col='red')
    with(plotdata,plot(resample.level,var.fecrate,type= 'o',pch=16,main=y,xaxp=c(10,100,9),cex=1.3,lwd=3,cex.lab=1.5,cex.axis=1.5))  
    } 
dev.off()
   
s2 <- Sys.time()
elapsedtime <- s2-s1