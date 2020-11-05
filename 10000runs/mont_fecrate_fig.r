setwd('C:/Buren_files/PhD/Harp Seals/fecundity_rates/2013 update/analyses/montecarlo-samplesize/10000runs')

mont1 <- read.csv('montecarlo-disaggregated-allyears-results1.csv',header=T)
mont2 <- read.csv('montecarlo-disaggregated-allyears-results2.csv',header=T)
mont3 <- read.csv('montecarlo-disaggregated-allyears-results3.csv',header=T)
mont4 <- read.csv('montecarlo-disaggregated-allyears-results4.csv',header=T)
mont <- rbind(mont1,mont2,mont3,mont4)
rm(mont1,mont2,mont3,mont4)

conflevel <- 0.05                       # confidence level for the confidence intervals in the plots
 results <- aggregate (fecrate~year+sampmontc, FUN = function(x) { c( mean=mean(x), quantile=quantile(x,probs = c(conflevel/2, 1-conflevel/2)),var=var(x), cv=sd(x)/mean(x))},data=mont)
results <- cbind(results[,1:2], as.data.frame(results[,3]))
names(results) <- c( 'year','resample.level','mean.fecrate','ll.fecrate', 'ul.fecrate','var.fecrate','cv.fecrate')

inpath<-'C:/Buren_files/PhD/Harp Seals/fecundity_rates/2013 update/analyses/montecarlo-samplesize' # folder where input file is
infile<-'ovar-mat.csv'     # name of input file
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

 par(mfrow=c(2,2))
  for (y in c(2006,2009,2010,2011)){
    plotdata <- subset(results,year==y)
    with(plotdata,plot(resample.level,var.fecrate,type= 'o',pch=16,ylim=c(0,0.1),main=y,xaxp=c(10,120,11)))
    }


cols <-    c('red','orange','blue','purple')
cols <-    c('0xE41A1C','purple','0x4DAF4A','0x984EA3')
cols <-    c(rgb(228/256, 26/256, 28/256),rgb(55/256, 126/256, 184/256),rgb(77/256, 175/256, 74/256),rgb(152/256, 78/256, 163/256))
cols2 <-    c(rgb(27/256, 158/256, 119/256),rgb(217/256, 95/256, 2/256),rgb(117/256, 112/256, 179/256),rgb(231/256, 41/256, 138/256))

results <- subset(results,resample.level<101)
setwd('C:/Buren_files/PhD/Harp Seals/fecundity_rates/2013 update/analyses/montecarlo-samplesize/10000runs')
jpeg(filename = "montecarlo-fecrate_var.jpg", width = 692, height = 692, units = "px", pointsize = 15, quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo"), antialias = c("default"))
  par(mfrow=c(1,1))
  # par(bty='n')
  with(subset(results,year==2006),plot(resample.level,var.fecrate,type= 'o',xlab="Resample level",ylab="Var(Fecundity rate)",pch=16,ylim=c(0,0.07),yaxp=c(0,0.07,2),xaxp=c(10,120,11),col=cols[1]))
  j <- 1
  for (y in c(2009,2010,2011)){
    j <- j+1
    plotdata <- subset(results,year==y)
    with(plotdata,lines(resample.level,var.fecrate,type= 'o',pch=16,ylim=c(0,0.1),xaxp=c(10,120,11),col=cols[j]))
  }
  axis(1,labels=F,at=seq(5,115,5),col="#0000ff00",col.ticks='black',tck=-.008)
  axis(1,labels=F,at=seq(5,115,5),col='black',col.ticks="#0000ff00",tck=-.008)
  legend(85,0.07,legend=c('2006','2009','2010','2011'),col=cols,bty='n',cex=0.8,lty=1,pch=16)
dev.off()
