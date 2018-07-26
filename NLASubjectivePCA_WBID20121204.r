rm(list=ls(all=T)) #clear workspace
# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
#con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/WaterbodyDatabase/WaterBodyDataBase.mdb")
#con <- odbcConnectAccess("L:/Public/Milstead_Lakes/WaterbodyDatabase/WaterbodyDatabase.mdb")
con <- odbcConnectAccess("c:/bryan/epa/data/WaterbodyDatabase/WaterbodyDatabase.mdb")
NLA <- sqlQuery(con, "
SELECT tblJoinNLAID_WBID.WB_ID, NLA2007Sites_DesignInfo.SITE_ID, tblNLA_WaterQualityData.NTL, tblNLA_WaterQualityData.PTL, tblNLA_WaterQualityData.TURB, tblNLA_WaterQualityData.DO2_2M, tblNLA_WaterQualityData.CHLA, tblNLA_WaterQualityData.SECMEAN, tblNLA_WaterQualityData.CL, tblNLA_WaterQualityData.ANC, tblNLA_WaterQualityData.SO4, tblNLA_WaterQualityData.COLOR, tblNLA_Habitat_Super_Metrics.[RDis_IX], tblNLA_Habitat_Super_Metrics.[RVegQ], tblNLA_Habitat_Super_Metrics.LitCvrQ, tblNLA_Habitat_Super_Metrics.LitRipCvQ, NLA2007Sites_DesignInfo.LAKEAREA, NLA2007Sites_DesignInfo.DEPTHMAX, tblNLA_VisualAssessment.SWIMMABILITY, tblNLA_VisualAssessment.PRISTINE, tblNLA_VisualAssessment.APPEALING, tblNLA_VisualAssessment.BIOTIC_INTEGRITY, tblNLA_VisualAssessment.TROPHIC_STATE, tblNLA_VisualAssessment.RECREATIONAL_VALUE, tblNLA_Microcystin.Microcystin_ugl
FROM tblNLA_Microcystin INNER JOIN (tblNLA_VisualAssessment INNER JOIN (((tblJoinNLAID_WBID INNER JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) INNER JOIN tblNLA_WaterQualityData ON (NLA2007Sites_DesignInfo.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = tblNLA_WaterQualityData.SITE_ID)) INNER JOIN tblNLA_Habitat_Super_Metrics ON (tblNLA_WaterQualityData.VISIT_NO = tblNLA_Habitat_Super_Metrics.VISIT_NO) AND (tblNLA_WaterQualityData.SITE_ID = tblNLA_Habitat_Super_Metrics.SITE_ID)) ON (tblNLA_VisualAssessment.VISIT_NO = tblNLA_Habitat_Super_Metrics.VISIT_NO) AND (tblNLA_VisualAssessment.SITE_ID = tblNLA_Habitat_Super_Metrics.SITE_ID)) ON tblNLA_Microcystin.SITE_ID = tblNLA_VisualAssessment.SITE_ID;
")
close(con)
names(NLA)



#Decide on variable tranformations for PCA
  # Create Matrix of Variables to test for Skewness
  # Remove lines with missing values
VARS <- na.exclude(data.frame(NLA[,3:18]))
                   

#Test Skewness as a proxy for normality
library(e1071) #Test for skewness - expected skewness for normal=0
               #Ignore warning messages about NANs
logVARS<-log10(VARS+1)
loglogVARS<-log(log(VARS+1)+1)
sqrtVARS<-sqrt(VARS)
asinVARS<-asin(sqrtVARS)

s_raw<-lapply (VARS,skewness,na.rm=T)
s_log<-lapply (logVARS,skewness,na.rm=T)
s_loglog<-lapply (loglogVARS,skewness,na.rm=T)
s_sqrt<-lapply (sqrtVARS,skewness,na.rm=T)
s_asin<-lapply (asinVARS,skewness,na.rm=T)
skewness<-rbind(s_raw, s_log, s_loglog, s_sqrt, s_asin)
skewness

#Recommended transformation based on skewness
#raw=DO.EUP, 
#log=PTL, SECMEAN, CL, ANC, COLOR, RDis_IX,
#loglog=NTL, TURB, CHLA, SO4, LakeArea, DEPTHMAX,
#sqrt=RVegQ, LitCvrQ, LitRipCvQ


#*************************

#Generate matrix of transformed PCA variable and remove missing values
#Data transformed to minimize skew
#Ignore warnings of NAN's produced
   tVARS<- na.exclude(data.frame(SiteID=NLA$SITE_ID, DO=NLA$DO2_2M,
           P=log10(NLA$PTL+1),Secchi=log10(NLA$SECMEAN+1),CL=log10(NLA$CL+1),ANC=log10(NLA$ANC+1),
           Color=log10(NLA$COLOR+1),RDis=log10(NLA$RDis_IX+1),
           N=log(log(NLA$NTL+1)+1),Turb=log(log(NLA$TURB+1)+1),Chla=log(log(NLA$CHLA+1)+1),SO4=log(log(NLA$SO4+1)+1),
           Area=log(log(NLA$LAKEAREA+1)+1),Depth=log(log(NLA$DEPTHMAX+1)+1),
           RVeg=sqrt(NLA$RVegQ),LitCvr=sqrt(NLA$LitCvrQ),LitRip=sqrt(NLA$LitRipCvQ)))
          
   
                  
#Generate matrix of categorical and quantitative explanatory variables
   temp<- data.frame(SiteID=NLA$SITE_ID, APPEALING=NLA$APPEALING,
                      PRISTINE=NLA$PRISTINE,BIOTIC_INTEGRITY=NLA$BIOTIC_INTEGRITY,
                      RECREATIONAL_VALUE=NLA$RECREATIONAL_VALUE,SWIMMABILITY=NLA$SWIMMABILITY,
Appealing=ifelse(NLA$APPEALING==5,"Highest",
                 ifelse(NLA$APPEALING<5 & NLA$APPEALING>2," ",
                        ifelse(NLA$APPEALING<=2,"Lowest",NA))),
Pristine=ifelse(NLA$PRISTINE==5,"Pristine",
                 ifelse(NLA$PRISTINE<5 & NLA$PRISTINE>2," ",
                        ifelse(NLA$PRISTINE<=2,"Disturbed",NA))),
Integrity=ifelse(NLA$BIOTIC_INTEGRITY=="EXCELLENT","Highest",
                 ifelse(NLA$BIOTIC_INTEGRITY=="GOOD" | NLA$BIOTIC_INTEGRITY=="FAIR"," ",
                        ifelse(NLA$BIOTIC_INTEGRITY=="POOR","Lowest",NA))),
Recreation=ifelse(NLA$RECREATIONAL_VALUE=="EXCELLENT","Highest",
                 ifelse(NLA$RECREATIONAL_VALUE=="GOOD" | NLA$RECREATIONAL_VALUE=="FAIR"," ",
                        ifelse(NLA$RECREATIONAL_VALUE=="POOR","Lowest",NA))),
Swimmable=ifelse(NLA$SWIMMABILITY=="GOOD","Highest",
                 ifelse(NLA$SWIMMABILITY=="FAIR"," ",
                        ifelse(NLA$SWIMMABILITY=="UNSWIMMABLE","Lowest",NA))),
Microcystin=ifelse(NLA$Microcystin_ugl==0,"Zero",
   ifelse(NLA$Microcystin_ugl<quantile(NLA$Microcystin_ugl[NLA$Microcystin_ugl>0],.5),"Lower","Higher")))
                
                 


#Match Merge transformed PCA variables and explanatory variables
   tVARS<-merge(tVARS, temp, by.x="SiteID",by.y="SiteID", all= FALSE)
   
names(tVARS)


#Principal Components Analysis
  library(ade4) #ade4 package must be installed
  pca1 <- dudi.pca(df=tVARS[,2:17],center=TRUE,scale=TRUE,scannf=FALSE,nf=10)
  pca1$c1$CS1 #loadings-information how much each variable contribute to each component
  
          dotchart(pca1$cw) #numeric column weights-
          dotchart(pca1$lw) #numeric row weights
          barplot(pca1$eig[1:11]) #numeric eigenvalues-barchart of eigenvalues
          ngr <- sqrt(nrow(pca1$co))+1;  #modified array
               par(mfrow=c(ngr,ngr)); 
               for(i in 1:nrow(pca1$co)) 
               s.value(pca1$li,pca1$tab[,i],1,2, 
               sub=names(pca1$tab[i]), csub=2, 
               clegend=2, cgrid=2); par(mfrow=c(1,1))
          s.label(pca1$li,1,2) #row coordinates
          s.arrow(pca1$l1,1,2) #row normed scores
          s.label(pca1$co,1,2) #column coordinates
          s.arrow(pca1$c1,1,2) #column normed scores
          scatter(pca1,1,2) # panel of some the above
          score(pca1,1) #plots of rawdata against PCA1 plot(e.g, pca1$li$Axis1,VARS$logPTL)
          score(pca1,2) #plots of rawdata against PCA2 plot(e.g, pca1$li$Axis1,VARS$logPTL)
          s.corcircle(pca1$co,1,2)
          pca1$li$Axis1[1:10]# to see the PCA1 scores for first ten observation 
          pca1$eig/sum(pca1$eig)#percent of variance explained by each component
          pca1$c1 #loadings-information how much each variable contribute to each component
          cor(pca1$li, tVARS) #shows the correlations between original data and the loadings.

  
#Function to perform Between Group Analysis of PCA Results and Create Plots
PCA_Cat <- function(f,label,c1,c2,c3)
  {
    y<-factor(f)
    Out1 <- between(dudi = pca1, fac = y, scannf = FALSE, nf = 10)
    par(mfrow=c(2,1))
    #s.arrow(Out1$co,1,2, sub="column coordinates",csub=.75)
    s.arrow(Out1$c1,1,2,sub="class normed column coordinates",csub=.75)
    s.class(dfxy=Out1$ls, fac=y, xax=1, yax=2,
      add.plot=F,cstar=0, #col=rainbow(length(levels(y))),
      col=c(c1,c2,c3),
      sub = label, csub = .75)
      #return(Out1$c1)
      return(Out1$eig/sum(Out1$eig))#percent of variance explained by each component

    par(mfrow=c(1,1))
  }

#Plot Results
  #Turn window recording on
  #in Graphics window use PageUp and PageDown to scroll through results
  #scroll wheel will not work
    #windows(record=T)



    PCA_Cat(tVARS$Pristine,"Pristine Class","grey","red","green")
    PCA_Cat(tVARS$Appealing,"Appealing Class","grey","green","red")
    PCA_Cat(tVARS$Swimmable,"Swimmable Class","grey","green","red")
    PCA_Cat(tVARS$Integrity,"Integrity Class","grey","green","red")
    PCA_Cat(tVARS$Recreation,"Recreation Class","grey","green","red")
    PCA_Cat(tVARS$Microcystin,"Microcystin Level","red","blue","green")
    






