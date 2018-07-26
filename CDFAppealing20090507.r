rm(list=ls(all=T)) #clear workspace

NLA<-read.csv(url("http://willbmisled.com/lakes/nla_test.csv"))

par(mfrow=c(2,2))
#Plot PTL & NTL
  plot(ecdf(subset(NLA$PTL,NLA$APPEALNG==5)),ylab='Cumulative Frequency',xlim=c(0,400),col="blue",
               main='Appealing', xlab='PTL',sub="Blue=Most   Brown=Least")
  plot(ecdf(subset(NLA$PTL,NLA$APPEALNG==1)),ylab='Cumulative Frequency',xlim=c(0,400),col="brown",add=T)
  plot(ecdf(subset(NLA$PTL,NLA$PRISTINE==5)),ylab='Cumulative Frequency',xlim=c(0,400),col="blue",
               main='Pristine', xlab='PTL',sub="Blue=Most   Brown=Least")
  plot(ecdf(subset(NLA$PTL,NLA$PRISTINE==1)),ylab='Cumulative Frequency',xlim=c(0,400),col="brown",add=T)
  plot(ecdf(subset(NLA$NTL,NLA$APPEALNG==5)),ylab='Cumulative Frequency',xlim=c(0,4000),col="blue",
               main='Appealing', xlab='NTL',sub="Blue=Most   Brown=Least")
  plot(ecdf(subset(NLA$NTL,NLA$APPEALNG==1)),ylab='Cumulative Frequency',xlim=c(0,4000),col="brown",add=T)
  plot(ecdf(subset(NLA$NTL,NLA$PRISTINE==5)),ylab='Cumulative Frequency',xlim=c(0,4000),col="blue",
               main='Pristine', xlab='NTL',sub="Blue=Most   Brown=Least")
  plot(ecdf(subset(NLA$NTL,NLA$PRISTINE==1)),ylab='Cumulative Frequency',xlim=c(0,4000),col="brown",add=T)

#Plot CHLA & Secchi
  plot(ecdf(subset(NLA$CHLA,NLA$APPEALNG==5)),ylab='Cumulative Frequency',xlim=c(0,200),col="blue",
               main='Appealing', xlab='CHLA',sub="Blue=Most   Brown=Least")
  plot(ecdf(subset(NLA$CHLA,NLA$APPEALNG==1)),ylab='Cumulative Frequency',xlim=c(0,200),col="brown",add=T)
  plot(ecdf(subset(NLA$CHLA,NLA$PRISTINE==5)),ylab='Cumulative Frequency',xlim=c(0,200),col="blue",
               main='Pristine', xlab='CHLA',sub="Blue=Most   Brown=Least")
  plot(ecdf(subset(NLA$CHLA,NLA$PRISTINE==1)),ylab='Cumulative Frequency',xlim=c(0,200),col="brown",add=T)
  plot(ecdf(subset(NLA$SECMEAN,NLA$APPEALNG==5)),ylab='Cumulative Frequency',xlim=c(0,40),col="blue",
               main='Appealing', xlab='SECMEAN',sub="Blue=Most   Brown=Least")
  plot(ecdf(subset(NLA$SECMEAN,NLA$APPEALNG==1)),ylab='Cumulative Frequency',xlim=c(0,40),col="brown",add=T)
  plot(ecdf(subset(NLA$SECMEAN,NLA$PRISTINE==5)),ylab='Cumulative Frequency',xlim=c(0,40),col="blue",
               main='Pristine', xlab='SECMEAN',sub="Blue=Most   Brown=Least")
  plot(ecdf(subset(NLA$SECMEAN,NLA$PRISTINE==1)),ylab='Cumulative Frequency',xlim=c(0,40),col="brown",add=T)
 
        
#means & weighted means for Appealing and Pristing by reference class
x<-na.exclude(data.frame(CLASS=NLA$CLASS,APPEALING=NLA$APPEALNG,PRISTINE=NLA$PRISTINE,
                         WT=NLA$WGT_NLA,
                         WT_A=NLA$APPEALNG*NLA$WGT_NLA,WT_P=NLA$PRISTINE*NLA$WGT_NLA))
P_Mean=aggregate(x$PRISTINE,x['CLASS'],mean)
p=aggregate(x$WT_P,x['CLASS'],mean)
w=aggregate(x$WT,x['CLASS'],mean)
A_Mean=aggregate(x$APPEALING,x['CLASS'],mean)
a=aggregate(x$WT_A,x['CLASS'],mean)
Means<-data.frame(CLASS=p$CLASS,A_Mean=A_Mean$x,A_WtMean=(a$x/w$x),P_Mean=P_Mean$x,P_WtMean=(p$x/w$x))
Means
