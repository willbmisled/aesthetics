rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
#con <- odbcConnectAccess("L:/Public/Milstead_Lakes/WaterbodyDatabase/WaterbodyDatabase.mdb")
get <- sqlQuery(con, "   
SELECT tblNLA_WaterQualityData.SITE_ID, tblNLAReferenceClassClusters20090318.CLUS,
tblNLAReferenceClassClusters20090318.CLASS, tblNLA_VisualAssessment.SWIMMABILITY, 
tblNLA_VisualAssessment.PRISTINE, tblNLA_VisualAssessment.APPEALNG, 
tblNLA_VisualAssessment.BIOTIC_INTEGRITY, tblNLA_VisualAssessment.TROPHIC_STATE, 
tblNLA_VisualAssessment.RECREATIONAL_VALUE, tblNLA_WaterQualityData.PH_LAB, 
tblNLA_WaterQualityData.COND, tblNLA_WaterQualityData.ANC, tblNLA_WaterQualityData.TURB, 
tblNLA_WaterQualityData.COLOR, tblNLA_WaterQualityData.TOC, tblNLA_WaterQualityData.DOC, 
tblNLA_WaterQualityData.NTL, tblNLA_WaterQualityData.PTL, tblNLA_WaterQualityData.NH4, 
tblNLA_WaterQualityData.NO3, tblNLA_WaterQualityData.NO3_NO2, tblNLA_WaterQualityData.SO4, 
tblNLA_WaterQualityData.CL, tblNLA_WaterQualityData.SIO2, tblNLA_WaterQualityData.CA, 
tblNLA_WaterQualityData.MG, tblNLA_WaterQualityData.NA, tblNLA_WaterQualityData.K, 
tblNLA_WaterQualityData.H, tblNLA_WaterQualityData.OH, tblNLA_WaterQualityData.NH4ION, 
tblNLA_WaterQualityData.CATSUM, tblNLA_WaterQualityData.ANSUM2, 
tblNLA_WaterQualityData.ANDEF2, tblNLA_WaterQualityData.SOBC, 
tblNLA_WaterQualityData.BALANCE2, tblNLA_WaterQualityData.ORGION, 
tblNLA_WaterQualityData.CONCAL2, tblNLA_WaterQualityData.CONDHO2, 
tblNLA_WaterQualityData.CHLA, tblNLA_WaterQualityData.SECMEAN, 
tblNLA_PhysicalHabitatMetrics.amfcEmergent, tblNLA_PhysicalHabitatMetrics.amfcFloating, 
tblNLA_PhysicalHabitatMetrics.amfcSubmergent, tblNLA_PhysicalHabitatMetrics.amfcAll, 
tblNLA_PhysicalHabitatMetrics.amiTotal, tblNLA_PhysicalHabitatMetrics.bfxHorizDist, 
tblNLA_PhysicalHabitatMetrics.bfxVertHeight, tblNLA_PhysicalHabitatMetrics.bsfcBedrock, 
tblNLA_PhysicalHabitatMetrics.bsfcSand, tblNLA_PhysicalHabitatMetrics.bsfcSilt, 
tblNLA_PhysicalHabitatMetrics.bsiStaVariety, tblNLA_PhysicalHabitatMetrics.bsiSiteVariety, 
tblNLA_PhysicalHabitatMetrics.bsxLdia, tblNLA_PhysicalHabitatMetrics.fcfcBoulders, 
tblNLA_PhysicalHabitatMetrics.fcfcBrush, tblNLA_PhysicalHabitatMetrics.fcfcLedges, 
tblNLA_PhysicalHabitatMetrics.fcfcLiveTrees, tblNLA_PhysicalHabitatMetrics.fcfcOverhang, 
tblNLA_PhysicalHabitatMetrics.fcfcSnag, tblNLA_PhysicalHabitatMetrics.fciNatural, 
tblNLA_PhysicalHabitatMetrics.hipwBuildings, tblNLA_PhysicalHabitatMetrics.hipwDocks, 
tblNLA_PhysicalHabitatMetrics.hipwWalls, tblNLA_PhysicalHabitatMetrics.hiiAll, 
tblNLA_PhysicalHabitatMetrics.hiiNonAg, tblNLA_PhysicalHabitatMetrics.hiiAg, 
tblNLA_PhysicalHabitatMetrics.hiiAllCirca, tblNLA_PhysicalHabitatMetrics.hifpAny, 
tblNLA_PhysicalHabitatMetrics.hifpAnyCirca, tblNLA_PhysicalHabitatMetrics.lzfpFilm, 
tblNLA_PhysicalHabitatMetrics.rvfpCanNone, tblNLA_PhysicalHabitatMetrics.rvfpCanBig, 
tblNLA_PhysicalHabitatMetrics.rvfcGndBare, tblNLA_PhysicalHabitatMetrics.rvfcGndInundated, 
tblNLA_PhysicalHabitatMetrics.rviTallWood, tblNLA_PhysicalHabitatMetrics.rviWoody, 
tblNLA_PhysicalHabitatMetrics.rviHerbs, tblNLA_PhysicalHabitatMetrics.rviTotalVeg, 
tblNLA_PhysicalHabitatMetrics.sixDepth, tblNLA_PhysicalHabitatMetrics.sivDepth, 
tblNLA_PhysicalHabitatMetrics.ssiStaVariety, tblNLA_PhysicalHabitatMetrics.ssiSiteVariety, 
tblNLA_PhysicalHabitatMetrics.ssxLdia, tblNLA_PhysicalHabitatMetrics.L_HorizDist, 
tblNLA_PhysicalHabitatMetrics.L_VertHeight, tblNLA_PhysicalHabitatMetrics.L_sixDepth, 
tblNLA_PhysicalHabitatMetrics.L_sivDepth, tblNLA_PhysicalHabitatMetrics.CVLitDepth, 
tblNLA_PhysicalHabitatMetrics.bsiStStaVariety, 
tblNLA_PhysicalHabitatMetrics.ssiStStaVariety, tblNLA_PhysicalHabitatMetrics.RDisIn, 
tblNLA_PhysicalHabitatMetrics.RDisInAg, tblNLA_PhysicalHabitatMetrics.RDisInNonAg, 
tblNLA_PhysicalHabitatMetrics.RDisInEx1a, tblNLA_PhysicalHabitatMetrics.SomeNatCvr, 
tblNLA_PhysicalHabitatMetrics.amfcFltEmg, tblNLA_PhysicalHabitatMetrics.RVegQ_1, 
tblNLA_PhysicalHabitatMetrics.RVegQ_2, tblNLA_PhysicalHabitatMetrics.RVegQ_3, 
tblNLA_PhysicalHabitatMetrics.RVegQ_5, tblNLA_PhysicalHabitatMetrics.RVegQ_5a, 
tblNLA_PhysicalHabitatMetrics.LITCVR_A, tblNLA_PhysicalHabitatMetrics.LITCVR_B, 
tblNLA_PhysicalHabitatMetrics.LITCVR_C, tblNLA_PhysicalHabitatMetrics.LITCVR_D, 
tblNLA_PhysicalHabitatMetrics.LRCVQ_1D, tblNLA_PhysicalHabitatMetrics.LRCVQ_2D, 
tblNLA_PhysicalHabitatMetrics.LRCVQ_5Da, tblNLA_PhysicalHabitatMetrics.LRCVQ_6Da
FROM (((((tblNLA_WaterQualityData LEFT JOIN NLA2007Sites_DesignInfo 
ON tblNLA_WaterQualityData.SITE_ID=NLA2007Sites_DesignInfo.SITE_ID) 
LEFT JOIN tblNLAReferenceClassClusters20090318 
ON tblNLA_WaterQualityData.SITE_ID=tblNLAReferenceClassClusters20090318.SITE_ID) 
LEFT JOIN tblNLA_PhysicalHabitatMetrics 
ON (tblNLA_WaterQualityData.SITE_ID=tblNLA_PhysicalHabitatMetrics.SITE_ID) 
AND (tblNLA_WaterQualityData.VISIT_NO=tblNLA_PhysicalHabitatMetrics.VISIT_NO)) 
LEFT JOIN tblNLA_AnalysisTeamData20090421 
ON (tblNLA_WaterQualityData.SITE_ID=tblNLA_AnalysisTeamData20090421.SITEID) 
AND (tblNLA_WaterQualityData.VISIT_NO=tblNLA_AnalysisTeamData20090421.VISITNO)) 
LEFT JOIN tblOmernikAggregatedEcoregions 
ON NLA2007Sites_DesignInfo.ECO_LEV_3=tblOmernikAggregatedEcoregions.ECO_L3) 
LEFT JOIN tblNLA_VisualAssessment 
ON (tblNLA_WaterQualityData.VISIT_NO=tblNLA_VisualAssessment.VISIT_NO) 
AND (tblNLA_WaterQualityData.SITE_ID=tblNLA_VisualAssessment.SITE_ID)
WHERE (((tblNLA_WaterQualityData.VISIT_NO)=1))
")
NLA<-data.frame(get)
close(con)

#Decide on variable tranformations for PCA
  # Create Matrix of Variables to test for Skewness
  # Remove lines with missing values
##VARS <- na.exclude(data.frame(NLA[,10:110],NPRatio=NLA$NTL/NLA$PTL))
                   

#Test Skewness as a proxy for normality
##library(e1071) #Test for skewness - expected skewness for normal=0
               #Ignore warning messages about NANs
##logVARS<-log10(VARS+1)
##loglogVARS<-log(log(VARS+1)+1)
##sqrtVARS<-sqrt(VARS)


##s_raw<-lapply (VARS,skewness,na.rm=T)
##s_log<-lapply (logVARS,skewness,na.rm=T)
##s_loglog<-lapply (loglogVARS,skewness,na.rm=T)
##s_sqrt<-lapply (sqrtVARS,skewness,na.rm=T)
##skewness<-rbind(s_raw, s_log, s_loglog, s_sqrt)
##skewness
##write.table(skewness, file='skewness.csv',row.names=T,sep=',')

#Transform variable to minimize skew.
#Ignore warnings of NAN's produced

 tVARS<- data.frame(
PH_LAB=NLA$PH_LAB,
log_COND=log(NLA$COND+1),
log_ANC=log(NLA$ANC+1),
loglog_TURB=log(log(NLA$TURB+1)+1),
log_COLOR=log(NLA$COLOR+1),
loglog_TOC=log(log(NLA$TOC+1)+1),
loglog_DOC=log(log(NLA$DOC+1)+1),
log_NTL=log(NLA$NTL+1),
log_PTL=log(NLA$PTL+1),
log_NPRatio=log((NLA$NTL/NLA$PTL)+1),
loglog_NH4=log(log(NLA$NH4+1)+1),
loglog_NO3=log(log(NLA$NO3+1)+1),
sqrt_NO3_NO2=sqrt(NLA$NO3_NO2),
loglog_SO4=log(log(NLA$SO4+1)+1),
log_CL=log(NLA$CL+1),
log_SIO2=log(NLA$SIO2+1),
log_CA=log(NLA$CA+1),
log_MG=log(NLA$MG+1),
loglog_NA.=log(log(NLA$NA.+1)+1),
loglog_K=log(log(NLA$K+1)+1),
loglog_H=log(log(NLA$H+1)+1),
loglog_OH=log(log(NLA$OH+1)+1),
loglog_NH4ION=log(log(NLA$NH4ION+1)+1),
log_CATSUM=log(NLA$CATSUM+1),
log_ANSUM2=log(NLA$ANSUM2+1),
log_ANDEF2=log(NLA$ANDEF2+1),
log_SOBC=log(NLA$SOBC+1),
loglog_ORGION=log(log(NLA$ORGION+1)+1),
log_CONCAL2=log(NLA$CONCAL2+1),
log_CONDHO2=log(NLA$CONDHO2+1),
loglog_CHLA=log(log(NLA$CHLA+1)+1),
loglog_SECMEAN=log(log(NLA$SECMEAN+1)+1),
sqrt_amfcEmergent=sqrt(NLA$amfcEmergent),
sqrt_amfcFloating=sqrt(NLA$amfcFloating),
sqrt_amfcSubmergent=sqrt(NLA$amfcSubmergent),
sqrt_amfcAll=sqrt(NLA$amfcAll),
sqrt_amiTotal=sqrt(NLA$amiTotal),
loglog_bfxHorizDist=log(log(NLA$bfxHorizDist+1)+1),
loglog_bfxVertHeight=log(log(NLA$bfxVertHeight+1)+1),
sqrt_bsfcBedrock=sqrt(NLA$bsfcBedrock),
sqrt_bsfcSand=sqrt(NLA$bsfcSand),
loglog_bsfcSilt=log(log(NLA$bsfcSilt+1)+1),
sqrt_bsiStaVariety=sqrt(NLA$bsiStaVariety),
bsiSiteVariety=NLA$bsiSiteVariety,
log_bsxLdia=log(NLA$bsxLdia+1),
sqrt_fcfcBoulders=sqrt(NLA$fcfcBoulders),
sqrt_fcfcBrush=sqrt(NLA$fcfcBrush),
sqrt_fcfcLedges=sqrt(NLA$fcfcLedges),
sqrt_fcfcLiveTrees=sqrt(NLA$fcfcLiveTrees),
sqrt_fcfcOverhang=sqrt(NLA$fcfcOverhang),
sqrt_fcfcSnag=sqrt(NLA$fcfcSnag),
loglog_fciNatural=log(log(NLA$fciNatural+1)+1),
sqrt_hipwBuildings=sqrt(NLA$hipwBuildings),
sqrt_hipwDocks=sqrt(NLA$hipwDocks),
sqrt_hipwWalls=sqrt(NLA$hipwWalls),
sqrt_hiiAll=sqrt(NLA$hiiAll),
loglog_hiiNonAg=log(log(NLA$hiiNonAg+1)+1),
sqrt_hiiAg=sqrt(NLA$hiiAg),
sqrt_hiiAllCirca=sqrt(NLA$hiiAllCirca),
hifpAny=NLA$hifpAny,
log_hifpAnyCirca=log(NLA$hifpAnyCirca+1),
sqrt_lzfpFilm=sqrt(NLA$lzfpFilm),
sqrt_rvfpCanNone=sqrt(NLA$rvfpCanNone),
rvfpCanBig=NLA$rvfpCanBig,
sqrt_rvfcGndBare=sqrt(NLA$rvfcGndBare),
sqrt_rvfcGndInundated=sqrt(NLA$rvfcGndInundated),
loglog_rviTallWood=log(log(NLA$rviTallWood+1)+1),
loglog_rviWoody=log(log(NLA$rviWoody+1)+1),
loglog_rviHerbs=log(log(NLA$rviHerbs+1)+1),
rviTotalVeg=NLA$rviTotalVeg,
loglog_sixDepth=log(log(NLA$sixDepth+1)+1),
loglog_sivDepth=log(log(NLA$sivDepth+1)+1),
ssiStaVariety=NLA$ssiStaVariety,
ssiSiteVariety=NLA$ssiSiteVariety,
sqrt_ssxLdia=sqrt(NLA$ssxLdia),
sqrt_L_HorizDist=sqrt(NLA$L_HorizDist),
sqrt_L_VertHeight=sqrt(NLA$L_VertHeight),
sqrt_L_sixDepth=sqrt(NLA$L_sixDepth),
loglog_CVLitDepth=log(log(NLA$CVLitDepth+1)+1),
loglog_bsiStStaVariety=log(log(NLA$bsiStStaVariety+1)+1),
sqrt_ssiStStaVariety=sqrt(NLA$ssiStStaVariety),
RDisIn=NLA$RDisIn,
sqrt_RDisInAg=sqrt(NLA$RDisInAg),
RDisInNonAg=NLA$RDisInNonAg,
RDisInEx1a=NLA$RDisInEx1a,
sqrt_SomeNatCvr=sqrt(NLA$SomeNatCvr),
sqrt_amfcFltEmg=sqrt(NLA$amfcFltEmg),
RVegQ_1=NLA$RVegQ_1,
sqrt_RVegQ_2=sqrt(NLA$RVegQ_2),
log_RVegQ_3=log(NLA$RVegQ_3+1),
RVegQ_5=NLA$RVegQ_5,
RVegQ_5a=NLA$RVegQ_5a,
loglog_LITCVR_A=log(log(NLA$LITCVR_A+1)+1),
sqrt_LITCVR_B=sqrt(NLA$LITCVR_B),
sqrt_LITCVR_C=sqrt(NLA$LITCVR_C),
sqrt_LITCVR_D=sqrt(NLA$LITCVR_D),
log_LRCVQ_1D=log(NLA$LRCVQ_1D+1),
sqrt_LRCVQ_2D=sqrt(NLA$LRCVQ_2D),
loglog_LRCVQ_5Da=log(log(NLA$LRCVQ_5Da+1)+1),
sqrt_LRCVQ_6Da=sqrt(NLA$LRCVQ_6Da),
SiteID=NLA$SITE_ID, 
Cluster=NLA$CLUS,
Class=NLA$CLASS,
Pristine=NLA$PRISTINE,
Appealing=NLA$APPEALNG,
Integrity=ifelse(NLA$BIOTIC_INTEGRITY=="POOR",1,ifelse(NLA$BIOTIC_INTEGRITY=="FAIR",2,
          ifelse(NLA$BIOTIC_INTEGRITY=="GOOD",3,
          ifelse(NLA$BIOTIC_INTEGRITY=="EXCELLENT",4,NA)))),
Recreation=ifelse(NLA$RECREATIONAL_VALUE=="POOR",1,ifelse(NLA$RECREATIONAL_VALUE=="FAIR",2,
          ifelse(NLA$RECREATIONAL_VALUE=="GOOD",3,
          ifelse(NLA$RECREATIONAL_VALUE=="EXCELLENT",4,NA)))), 
Swimmable=ifelse(NLA$SWIMMABILITY=="UNSWIMMABLE",1,ifelse(NLA$SWIMMABILITY=="FAIR",2,
          ifelse(NLA$SWIMMABILITY=="GOOD",3,NA))),
Trophic=ifelse(NLA$TROPHIC_STATE=="OLIGOTROPHIC",4,
        ifelse(NLA$TROPHIC_STATE=="MESOTROPHIC",3,
          ifelse(NLA$TROPHIC_STATE=="EUTROPHIC",2,
          ifelse(NLA$TROPHIC_STATE=="HYPEREUTROPHIC",1,NA))))           
 )

windows(record=T)  
par(mfrow=c(3,2)) 

#Use this function for boxplots with medians

PlotB <- function(f,label)
  {
   boxplot(f ~  tVARS$Appealing, notch=TRUE, varwidth=TRUE, 
           xlab="Aesthetic Condition (Unappealing to Appealing)", ylab=label)
   boxplot(f ~  tVARS$Pristine, notch=TRUE, varwidth=TRUE, 
           xlab="Human Influence Level (Disturbed to Pristine)", ylab=label)
   boxplot(f ~  tVARS$Recreation, notch=TRUE, varwidth=TRUE, 
           xlab="Recreation Potential (Poor-Fair-Good-Excellent)", ylab=label)
   boxplot(f ~  tVARS$Swimmable, notch=TRUE, varwidth=TRUE, 
           xlab="Swimming Value (Poor-Fair-Good)", ylab=label)
   boxplot(f ~  tVARS$Trophic, notch=TRUE, varwidth=TRUE, 
           xlab="Trophic Status (Hyper-Eu-Meso-Oligo)", ylab=label)
   boxplot(f ~  tVARS$Integrity, notch=TRUE, varwidth=TRUE, 
           xlab="Biotic Integrity (Poor-Fair-Good-Excellent)", ylab=label)
   }
   
#Use this function for simple plots with means and 95%CI  
#In most cases the error bars are too small to plot and this warning is given:
#"zero-length arrow is of indeterminate angle and so skipped"
#Ignore this
PlotB <- function(f,label)
  {   
   plotmeans(f ~  tVARS$Appealing,
            xlab="Aesthetic Condition (Unappealing to Appealing)",
            ylab=c(label,"Mean +/- 95% CI"),connect=F,pch=15,col="blue")
   plotmeans(f ~  tVARS$Pristine,
            xlab="Human Influence Level (Disturbed to Pristine)",
            ylab=c(label,"Mean +/- 95% CI"),connect=F,pch=15,col="blue")
   plotmeans(f ~  tVARS$Recreation,
            xlab="Recreation Potential (Poor-Fair-Good-Excellent)",
            ylab=c(label,"Mean +/- 95% CI"),connect=F,pch=15,col="blue")
   plotmeans(f ~  tVARS$Swimmable,
            xlab="Swimming Value (Poor-Fair-Good)",
            ylab=c(label,"Mean +/- 95% CI"),connect=F,pch=15,col="blue")
   plotmeans(f ~  tVARS$Trophic,
            xlab="Trophic Status (Hyper-Eu-Meso-Oligo)",
            ylab=c(label,"Mean +/- 95% CI"),connect=F,pch=15,col="blue")
   plotmeans(f ~  tVARS$Integrity,
            xlab="Biotic Integrity (Poor-Fair-Good-Excellent)",
            ylab=c(label,"Mean +/- 95% CI"),connect=F,pch=15,col="blue")
   }




PlotB(log10(NLA$SECMEAN+1),"log Secchi Transparency")
PlotB(tVARS$RDisInNonAg,"Human Structures")

windows(record=T)  
par(mfrow=c(3,2))
PlotB(tVARS$PH_LAB,"PH_LAB")
PlotB(tVARS$log_COND,"logCOND")
PlotB(tVARS$log_ANC,"logANC")
PlotB(tVARS$loglog_TURB,"loglogTURB")
PlotB(tVARS$log_COLOR,"logCOLOR")
PlotB(tVARS$loglog_TOC,"loglogTOC")
PlotB(tVARS$loglog_DOC,"loglogDOC")
PlotB(tVARS$log_NTL,"logNTL")
PlotB(tVARS$log_PTL,"logPTL")
PlotB(tVARS$log_NPRatio,"logNPRatio")
PlotB(tVARS$loglog_NH4,"loglogNH4")
PlotB(tVARS$loglog_NO3,"loglogNO3")
PlotB(tVARS$sqrt_NO3_NO2,"sqrtNO3_NO2")
PlotB(tVARS$loglog_SO4,"loglogSO4")
PlotB(tVARS$log_CL,"logCL")
PlotB(tVARS$log_SIO2,"logSIO2")
PlotB(tVARS$log_CA,"logCA")
PlotB(tVARS$log_MG,"logMG")
PlotB(tVARS$loglog_NA.,"loglogNA.")
PlotB(tVARS$loglog_K,"loglogK")
PlotB(tVARS$loglog_H,"loglogH")
PlotB(tVARS$loglog_OH,"loglogOH")
PlotB(tVARS$loglog_NH4ION,"loglogNH4ION")
PlotB(tVARS$log_CATSUM,"logCATSUM")
PlotB(tVARS$log_ANSUM2,"logANSUM2")
PlotB(tVARS$log_ANDEF2,"logANDEF2")
PlotB(tVARS$log_SOBC,"logSOBC")
PlotB(tVARS$loglog_ORGION,"loglogORGION")
PlotB(tVARS$log_CONCAL2,"logCONCAL2")
PlotB(tVARS$log_CONDHO2,"logCONDHO2")
PlotB(tVARS$loglog_CHLA,"loglogCHLA")
PlotB(tVARS$loglog_SECMEAN,"loglogSECMEAN")
PlotB(tVARS$sqrt_amfcEmergent,"sqrtamfcEmergent")
PlotB(tVARS$sqrt_amfcFloating,"sqrtamfcFloating")
PlotB(tVARS$sqrt_amfcSubmergent,"sqrtamfcSubmergent")
PlotB(tVARS$sqrt_amfcAll,"sqrtamfcAll")
PlotB(tVARS$sqrt_amiTotal,"sqrtamiTotal")
PlotB(tVARS$loglog_bfxHorizDist,"loglogbfxHorizDist")
PlotB(tVARS$loglog_bfxVertHeight,"loglogbfxVertHeight")
PlotB(tVARS$sqrt_bsfcBedrock,"sqrtbsfcBedrock")
PlotB(tVARS$sqrt_bsfcSand,"sqrtbsfcSand")
PlotB(tVARS$loglog_bsfcSilt,"loglogbsfcSilt")
PlotB(tVARS$sqrt_bsiStaVariety,"sqrtbsiStaVariety")
PlotB(tVARS$bsiSiteVariety,"bsiSiteVariety")
PlotB(tVARS$log_bsxLdia,"logbsxLdia")
PlotB(tVARS$sqrt_fcfcBoulders,"sqrtfcfcBoulders")
PlotB(tVARS$sqrt_fcfcBrush,"sqrtfcfcBrush")
PlotB(tVARS$sqrt_fcfcLedges,"sqrtfcfcLedges")
PlotB(tVARS$sqrt_fcfcLiveTrees,"sqrtfcfcLiveTrees")
PlotB(tVARS$sqrt_fcfcOverhang,"sqrtfcfcOverhang")
PlotB(tVARS$sqrt_fcfcSnag,"sqrtfcfcSnag")
PlotB(tVARS$loglog_fciNatural,"loglogfciNatural")
PlotB(tVARS$sqrt_hipwBuildings,"sqrthipwBuildings")
PlotB(tVARS$sqrt_hipwDocks,"sqrthipwDocks")
PlotB(tVARS$sqrt_hipwWalls,"sqrthipwWalls")
PlotB(tVARS$sqrt_hiiAll,"sqrthiiAll")
PlotB(tVARS$loglog_hiiNonAg,"logloghiiNonAg")
PlotB(tVARS$sqrt_hiiAg,"sqrthiiAg")
PlotB(tVARS$sqrt_hiiAllCirca,"sqrthiiAllCirca")
PlotB(tVARS$hifpAny,"hifpAny")
PlotB(tVARS$log_hifpAnyCirca,"loghifpAnyCirca")
PlotB(tVARS$sqrt_lzfpFilm,"sqrtlzfpFilm")
PlotB(tVARS$sqrt_rvfpCanNone,"sqrtrvfpCanNone")
PlotB(tVARS$rvfpCanBig,"rvfpCanBig")
PlotB(tVARS$sqrt_rvfcGndBare,"sqrtrvfcGndBare")
PlotB(tVARS$sqrt_rvfcGndInundated,"sqrtrvfcGndInundated")
PlotB(tVARS$loglog_rviTallWood,"loglogrviTallWood")
PlotB(tVARS$loglog_rviWoody,"loglogrviWoody")
PlotB(tVARS$loglog_rviHerbs,"loglogrviHerbs")
PlotB(tVARS$rviTotalVeg,"rviTotalVeg")
PlotB(tVARS$loglog_sixDepth,"loglogsixDepth")
PlotB(tVARS$loglog_sivDepth,"loglogsivDepth")
PlotB(tVARS$ssiStaVariety,"ssiStaVariety")
PlotB(tVARS$ssiSiteVariety,"ssiSiteVariety")
PlotB(tVARS$sqrt_ssxLdia,"sqrtssxLdia")
PlotB(tVARS$sqrt_L_HorizDist,"sqrtL_HorizDist")
PlotB(tVARS$sqrt_L_VertHeight,"sqrtL_VertHeight")
PlotB(tVARS$sqrt_L_sixDepth,"sqrtL_sixDepth")
PlotB(tVARS$loglog_CVLitDepth,"loglogCVLitDepth")
PlotB(tVARS$loglog_bsiStStaVariety,"loglogbsiStStaVariety")
PlotB(tVARS$sqrt_ssiStStaVariety,"sqrtssiStStaVariety")
PlotB(tVARS$RDisIn,"RDisIn")
PlotB(tVARS$sqrt_RDisInAg,"sqrtRDisInAg")
PlotB(tVARS$RDisInNonAg,"RDisInNonAg")
PlotB(tVARS$RDisInEx1a,"RDisInEx1a")
PlotB(tVARS$sqrt_SomeNatCvr,"sqrtSomeNatCvr")
PlotB(tVARS$sqrt_amfcFltEmg,"sqrtamfcFltEmg")
PlotB(tVARS$RVegQ_1,"RVegQ_1")
PlotB(tVARS$sqrt_RVegQ_2,"sqrtRVegQ_2")
PlotB(tVARS$log_RVegQ_3,"logRVegQ_3")
PlotB(tVARS$RVegQ_5,"RVegQ_5")
PlotB(tVARS$RVegQ_5a,"RVegQ_5a")
PlotB(tVARS$loglog_LITCVR_A,"loglogLITCVR_A")
PlotB(tVARS$sqrt_LITCVR_B,"sqrtLITCVR_B")
PlotB(tVARS$sqrt_LITCVR_C,"sqrtLITCVR_C")
PlotB(tVARS$sqrt_LITCVR_D,"sqrtLITCVR_D")
PlotB(tVARS$log_LRCVQ_1D,"logLRCVQ_1D")
PlotB(tVARS$sqrt_LRCVQ_2D,"sqrtLRCVQ_2D")
PlotB(tVARS$loglog_LRCVQ_5Da,"loglogLRCVQ_5Da")
PlotB(tVARS$sqrt_LRCVQ_6Da,"sqrtLRCVQ_6Da")







par(mfrow=c(2,2))
#Plot PTL & NTL -requires R version 2.9 or higher.
#if using earlier version remove the col="blue" and col="brown" options
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
 
        
#end of file