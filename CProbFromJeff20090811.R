#CPROB code by Jeff Hollister.  See example at bottom.

#CPROB function
cprob<-function(xX,xY,xImpair,ProbComp = c("gt","lt"),Exceed = c("gte","lte"),
       ci=FALSE, alpha = 0.05, R=100,xW=rep(1,length(xX)))

      {#Functions used for bootstrap

      #RESAMPLE Function
      #Per suggestion on R Help Page for use of sample()
      resample <- function(x, size, ...)
       if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
       } else sample(x, size, ...)

      #FILLIN Function
      #Does a step interpolation on missing bootstrapped values
      #Missing bootstrapped values are generated becuase of repeat samples
      #Need to have a one-to-one match of x variable (i.e. what prob is
      #conditioned on). In other words, need to maintain the original values of
      #the x-variable in the bootstrapped samples.  Y-value is bootstrapped

      ###BEGIN FILLIN FUNCTION###
      fillin<-function(x,o,Exceed)
        {if (tolower(Exceed) == "lte"){
        xdf<-cbind(o,x)
        fillmatrix<-cbind(xdf[,1][xdf[,2]!=0],xdf[,2][xdf[,2]!=0])
        for(i in 1:length(fillmatrix[,1]))
        xdf[,2][xdf[,1]<fillmatrix[,1][i]&xdf[,2]==0]=fillmatrix[,2][i]
        xdf[,2][xdf[,1]>fillmatrix[,1][i]&xdf[,2]==0]=fillmatrix[,2][i]}

        if (tolower(Exceed) == "gte")
        {xdf<-cbind(order(o,decreasing=T),x[order(o,decreasing=T)])
        fillmatrix<-cbind(xdf[,1][xdf[,2]!=0],xdf[,2][xdf[,2]!=0])
        for(i in 1:length(fillmatrix[,1]))
        xdf[,2][xdf[,1]>fillmatrix[,1][i]&xdf[,2]==0]=fillmatrix[,2][i]
        xdf[,2][xdf[,1]<fillmatrix[,1][i]&xdf[,2]==0]=fillmatrix[,2][i]
        xdf<-cbind(xdf[,1][order(o,decreasing=T)],xdf[,2][order(o,decreasing=T)])}
        xdf
        }
      ###END FILLIN FUNCTION###


      #CP Function
      #Calculates Conditional Probability

      ###BEGIN CONDITIONAL PROBABILITY FUNCTION - WITH WEIGHTS###
      cpwt<-function(xX,xxY,xxImpair,xxProbComp = c("gt","lt"),
               xxW=rep(1,length(xX)),xxo)
        {
        #sets empty objects
        xxProb<-0
        xxNum<-0
        xxDenom<-0

        #Sets xImpair comparison - gt or lt
        if(tolower(xxProbComp) == "gt")
            {xxgtlt_w <- expression(xxW[i:length(xxo)][xxY[i:length(xxo)]>xxImpair])}
        if(tolower(xxProbComp) == "lt")
            {xxgtlt_w <- expression(xxW[i:length(xxo)][xxY[i:length(xxo)]<xxImpair])}

        #Calculates Conditional Probability
        for(i in xxo)
           {xxNum[i]<-sum(eval(xxgtlt_w))/sum(xxW)
            xxDenom[i]<-sum(xxW[i:length(xxo)])/sum(xxW)
            xxProb[i]<-xxNum[i]/xxDenom[i]
            }
            xxProb}
      ###END CONDITIONAL PROBABILITY FUNCTION###



      #Beginning of Bootstrapping
      #Removes NA's from analysis
      xX2 <- na.omit(data.frame(xX,xY,xW))[,1]
      xY2 <- na.omit(data.frame(xX,xY,xW))[,2]
      xW2 <- na.omit(data.frame(xX,xY,xW))[,3]
      xDF <- data.frame(xX2,xY2,xW2)

      #Sets Exceed Direction
      if(tolower(Exceed) == "gte")
          {o<-order(xDF[,1],xDF[,2],decreasing=F)
           xYSort<-xDF[,2][o]
           xXSort<-xDF[,1][o]
           xWSort<-xDF[,3][o]
           o<-order(xXSort)
           }
        if(tolower(Exceed) == "lte")
          {o<-order(xDF[,1],xDF[,2],decreasing=T)
           xYSort<-xDF[,2][o]
           xXSort<-xDF[,1][o]
           xWSort<-xDF[,3][o]
           o<-order(xXSort)
           }

      rawdata<-data.frame(xXSort,xYSort,xWSort)
      #o<-which(duplicated(rawdata[,1])==FALSE)

      #Creates Data Frame to store bootstraped CondProb
      bootcp<-data.frame(matrix(nrow=length(xXSort[which(duplicated(xXSort)==FALSE)]),ncol=R))

      #Sets up Bootstrap loop - Conditional deals with possiblility of having
      #Impariment metric values that are actually zero.

       for (i in 1:R)
        {if(sum(rawdata[,2]==0)>0)
         {bootdata1<-data.frame(rawdata[,1],(rawdata[,2]+1)*resample(c(0,1),
                               length(rawdata[,1]),replace=T),rawdata[,3])
         bootdata<-fillin(bootdata1[,2],c(1:length(bootdata1[,2])),Exceed)
         bootcp[,i]<-cpwt(bootdata1[,1],bootdata[,2]-1,xImpair,ProbComp,
                          bootdata1[,3],o)[which(duplicated(bootdata1[,1])==FALSE)]}
         else
         {bootdata1<-data.frame(rawdata[,1],(rawdata[,2])*resample(c(0,1),
                               length(rawdata[,1]),replace=T),rawdata[,3])
         bootdata<-fillin(bootdata1[,2],c(1:length(bootdata1[,2])),Exceed)
         bootcp[,i]<-cpwt(bootdata1[,1],bootdata[,2],xImpair,ProbComp,
                          bootdata1[,3],o)[which(duplicated(bootdata1[,1])==FALSE)]}
         }

      #Calculates Mean and Upper and Lower CI, Adds to output
      if (ci==T)
      {xOutput<-data.frame(xXSort[which(duplicated(xXSort)==FALSE)],cpwt(xXSort,
                           xYSort,xImpair,ProbComp,xWSort,o)[which(duplicated(xXSort)==FALSE)],
                            apply(bootcp,1,mean),apply(bootcp,1,quantile,
                            probs=alpha,na.rm=T),apply(bootcp,1,quantile,probs=1.0-alpha,na.rm=T))
      names(xOutput)<-c(substitute(xX),"Raw.Data.Probability","Bootstrap.Probability","Lower.CI",
                        "Upper.CI")}
      else
      {xOutput<-data.frame(xXSort[which(duplicated(xXSort)==FALSE)],cpwt(xXSort,
                           xYSort,xImpair,ProbComp,xWSort,o)[which(duplicated(xXSort)==FALSE)],
                            apply(bootcp,1,mean))
      names(xOutput)<-c(substitute(xX),"Raw.Data.Probability","Bootstrap.Probability")}
   
      xOutput}
    


#######################################################
#Figure 3 in the output is what you should get from running CProb.R with the dummydata.xls
#the call to cprob should look something like:

#xX=Stressor
#xY=REsponse
#XImpair=Stressor Threshold Value
#ProbComp=Impairment Cutoff Direction="gt"or "lt";is impairment gt threshold or lt threshold
#Exceed=Probability Direction="gte" or "lte"; direction of the stressor in which impairment is expected

#dummydata<- read.csv("M:/Net MyDocuments/PortableApps/R/scripts/CProb/DummyData.csv")  
#attach(dummydata)
#keep=cprob(percfines,epttaxa,9,"gt","gte",T)
