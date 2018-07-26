rm(list=ls(all=T)) #clear workspace

# get NLA and Sparrow Data
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "   
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, NLA2007Sites_DesignInfo.SITE_TYPE, MRB1_WBIDLakes.AlbersAreaM AS Area, tblGISLakeVolume.GISVol AS Volume, tblSparrowLoads.OutflowM3_yr AS Outflow, tblNLA_WaterQualityData.NTL, tblSparrowLoads.N_Load_kg_yr AS NInput, tblSparrowLoads.N_Output AS NOutput, tblSparrow_NP_Estimates.Est_TN, tblNLA_WaterQualityData.PTL, tblSparrowLoads.P_Load_kg_yr AS PInput, tblSparrowLoads.P_Output AS POutput, tblSparrow_NP_Estimates.Est_TP, tblNLA_WaterQualityData.CHLA AS ChlA, tblSparrow_NP_Estimates.Est_CHLA AS Est_ChlA, tblNLA_WaterQualityData.SECMEAN AS Secchi, tblSparrow_NP_Estimates.Est_SECMEAN AS Est_Secchi, tblNLA_Microcystin.Microcystin_ugl AS Microcystin, tblNLA_Microcystin.WHO_Category AS WHO, tblNLA_VisualAssessment.PRISTINE, tblNLA_VisualAssessment.APPEALNG, tblNLA_VisualAssessment.BIOTIC_INTEGRITY, tblNLA_VisualAssessment.TROPHIC_STATE, tblNLA_VisualAssessment.RECREATIONAL_VALUE, tblNLA_VisualAssessment.SWIMMABILITY, tblNLA_PhysicalHabitatMetrics.RDisIn, tblNLA_PhysicalHabitatMetrics.RDisInAg, tblNLA_PhysicalHabitatMetrics.RDisInNonAg, tblNLA_PhysicalHabitatMetrics.RDisInEx1a, tblNLA_ReferenceLakes_Bio.CLUSTER, tblNLA_ReferenceLakes_Bio.CLASS, qryCyanoCounts.SumOfABUND AS CyanoCount, qryEntero.Entero
FROM ((((((((((((MRB1_WBIDLakes INNER JOIN tblJoinNLAID_WBID ON MRB1_WBIDLakes.WB_ID = tblJoinNLAID_WBID.WB_ID) INNER JOIN tblNLA_AnalysisTeamData20090421 ON tblJoinNLAID_WBID.NLA_ID = tblNLA_AnalysisTeamData20090421.SITEID) INNER JOIN tblSparrowLoads ON MRB1_WBIDLakes.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN tblNLA_WaterQualityData ON (tblNLA_AnalysisTeamData20090421.VISITNO = tblNLA_WaterQualityData.VISIT_NO) AND (tblNLA_AnalysisTeamData20090421.SITEID = tblNLA_WaterQualityData.SITE_ID)) INNER JOIN tblGISLakeVolume ON MRB1_WBIDLakes.WB_ID = tblGISLakeVolume.WBID) INNER JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) INNER JOIN tblNLA_VisualAssessment ON (tblNLA_AnalysisTeamData20090421.VISITNO = tblNLA_VisualAssessment.VISIT_NO) AND (tblNLA_AnalysisTeamData20090421.SITEID = tblNLA_VisualAssessment.SITE_ID)) INNER JOIN tblNLA_Microcystin ON tblJoinNLAID_WBID.NLA_ID = tblNLA_Microcystin.SITE_ID) INNER JOIN tblNLA_PhysicalHabitatMetrics ON (tblNLA_AnalysisTeamData20090421.SITEID = tblNLA_PhysicalHabitatMetrics.SITE_ID) AND (tblNLA_AnalysisTeamData20090421.VISITNO = tblNLA_PhysicalHabitatMetrics.VISIT_NO)) INNER JOIN tblNLA_ReferenceLakes_Bio ON tblJoinNLAID_WBID.NLA_ID = tblNLA_ReferenceLakes_Bio.SITE_ID) INNER JOIN tblSparrow_NP_Estimates ON MRB1_WBIDLakes.WB_ID = tblSparrow_NP_Estimates.WB_ID) LEFT JOIN qryCyanoCounts ON tblJoinNLAID_WBID.NLA_ID = qryCyanoCounts.SITE_ID) LEFT JOIN qryEntero ON tblJoinNLAID_WBID.NLA_ID = qryEntero.SITE_ID
WHERE (((tblSparrowLoads.N_Percent)=1) AND ((tblNLA_AnalysisTeamData20090421.VISITNO)=1));
")
MRB1<-data.frame(get)
close(con)
attach(MRB1)
names(MRB1)


#Field Definitions:
  #WB_ID=unique lake identification number
  #NLA_ID=National Lake Assessment (NLA) Lake Identification Number
  #SITE_TYPE: probability survey samples=PROB_Lake; selecting only these reduces N from 133 to 100.
  #Area (m2): Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  #Volume (m3):volume for each lake calculated by Jeff Hollister from waterbody polygons and NLA MaxDepth
  #Outflow (m3/yr): Sum of CFS for all SPARROW waterbody outflows converted to m3/yr ([CFS_Output]*893593)
  #NTL (ug/l):  Total Nitrogen from the NLA
  #NInput (kg/yr): Sum of nitrogen loads from SPARROW for all upstream flowlines plus the incremental load 
  #                   for all flowlines within the waterbody.
  #NOutput (kg/yr): Sum of Nitrogen loads from SPARROW for all outflow flowlines of a waterbody.
  #Est_TN (ug/l):  Total Nitrogen estimated from the sparrow concentrations based regression equations (see Sparrow20090929.r)
  #PTL (ug/l):  Total Phosporus from the NLA
  #PInput (kg/yr): Sum of phosphorus loads from SPARROW for all upstream flowlines plus the incremental load 
  #                   for all flowlines within the waterbody.
  #POutput (kg/yr): Sum of Phosporus loads from SPARROW for all outflow flowlines of a waterbody.
  #Est_TP (ug/l):  Total Phosphours estimated from the sparrow concentrations based regression equations (see Sparrow20090929.r)
  #ChlA (ug/l):  Chorophyll A concentration in waterbody from NLA
  #Est_ChlA (ug/l):  Chorophyll A concentration estimated from the Est_TN & Est_TN  from sparrow concentrations 
  #                based on regression equations (see Sparrow20090929.r)
  #Secchi (m):  Secchi Disk Transparency from NLA
  #Est_Secchi (m): Secchi estimated from the Est_TN, Est_TN & Est_ChlA from sparrow concentrations 
  #                based on regression equations (see Sparrow20090929.r)
  #Microcystin (ug/l): NLA data on Microcystin detection.
  #WHO:  World Health Org. Category for Microcystin toxicity.
  #RDisIn: Riparian Disturbance Intensity Index (=RDisIn=1-(1/(1+hiiAll)))
  #RDisInAg: Riparian Agricultural Disturbance Intensity Index (=RDisInAg=1-(1/(1+hiiAg)))
  #RDisInNonAG: Riparian Non-Agricultural Disturbance Intensity Index (=1-(1/(1+hiiNonAg)))
  #RDisInEx1a: Riparian Disturbance Intensity and Extent Index 1a (=(RDisIn+hifpAnyCirca)/2) 
  #CLASS=Reference Class Assigned by the NLA analysis team
  #CLUSTER=groupings used to assign CLASS-most of the MRB1 site are in CLUSTER=B 
  #CyanoCount (cells/ml): Cyanobacteria counts
  #Entero  (CEQ/100 mL): Enterococci counts

#Data filters: 
  #for NLA data from first visit to the lake
  #for SPARROW data from waterbodies where the Input Load = Output Load for Nitrogen 
  #Consider selecting SITE_TYPE=PROB_Lake also; this will restrict analysis to probability samples only
                 #but will reduce sample size from 133 to 100.
  
#Calculated Fields
    TN=NTL/1000 #(mg/l)=Total Nitrogen from NLA 
    TP=PTL/1000 #(mg/l)=Total Phosphorus from NLA 
    Nin=NInput*1000000/Outflow #(ug/l) Nitrogen inflow load concentration from sparrow
    Nout=NOutput*1000000/Outflow #(ug/l) Nitrogen load concentration from sparrow
    Pin=PInput*1000000/Outflow #(ug/l) Phosphorus inflow load concentration from sparrow
    Pout=POutput*1000000/Outflow #(ug/l) Phosphorus load concentration from sparrow
    hrt=Volume/Outflow # (yr) Hydraulic retention time
    z=Volume/Area #(m) Mean Depth
    NPRatio=NTL/PTL #Nitrogen Phosphorus ratio (concentration ratio)
    #Appealing: Low=1 to High=5
      Appealing=MRB1$APPEALNG #rename
    #Pristine: Low=1 to High=5
      Pristine=MRB1$PRISTINE #rename
    #Integrity: Low=1 to High=4
      Integrity=ifelse(MRB1$BIOTIC_INTEGRITY=="POOR",1,ifelse(MRB1$BIOTIC_INTEGRITY=="FAIR",2,
          ifelse(MRB1$BIOTIC_INTEGRITY=="GOOD",3,
          ifelse(MRB1$BIOTIC_INTEGRITY=="EXCELLENT",4,NA))))
    #Recreation: Low=1 to High=4
      Recreation=ifelse(MRB1$RECREATIONAL_VALUE=="POOR",1,ifelse(MRB1$RECREATIONAL_VALUE=="FAIR",2,
          ifelse(MRB1$RECREATIONAL_VALUE=="GOOD",3,
          ifelse(MRB1$RECREATIONAL_VALUE=="EXCELLENT",4,NA))))
    #Swimmable: Low=1 to High=3
      Swimmable=ifelse(MRB1$SWIMMABILITY=="UNSWIMMABLE",1,ifelse(MRB1$SWIMMABILITY=="FAIR",2,
          ifelse(MRB1$SWIMMABILITY=="GOOD",3,NA)))
    #Trophic: Low=1 to High=4
      Trophic=ifelse(MRB1$TROPHIC_STATE=="OLIGOTROPHIC",4,
        ifelse(MRB1$TROPHIC_STATE=="MESOTROPHIC",3,
          ifelse(MRB1$TROPHIC_STATE=="EUTROPHIC",2,
          ifelse(MRB1$TROPHIC_STATE=="HYPEREUTROPHIC",1,NA))))
    #Assign Trophic State based on NLA thresholds for ChlA, TN, TP, and Secchi
      #1=Oligotrophic
      #2=Mesotrophic
      #3=Eutrophic
      #4=Hypertrophic

    tsChlA<-ifelse(is.na(ChlA),NA,
        ifelse(ChlA<2,4,
        ifelse(ChlA>=2 & ChlA<7,3,
        ifelse(ChlA>=7 & ChlA<=30,2,
        ifelse(ChlA>30,1,0)))))

    tsPTL<-ifelse(is.na(PTL),NA,
        ifelse(PTL<10,4,
        ifelse(PTL>=10 & PTL<25,3,
        ifelse(PTL>=25 & PTL<=50,2,
        ifelse(PTL>50,1,0)))))
        
    tsNTL<-ifelse(is.na(NTL),NA,
        ifelse(NTL<350,4,
        ifelse(NTL>=350 & NTL<750,3,
        ifelse(NTL>=750 & NTL<=1400,2,
        ifelse(NTL>1400,1,0)))))

    tsSecchi<-ifelse(is.na(Secchi),NA,
        ifelse(Secchi>4,4,
        ifelse(Secchi<=4 & Secchi>2.1,3,
        ifelse(Secchi<=2.1 & Secchi>=.7,2,
        ifelse(Secchi<.7,1,0)))))

#Plot with 95%CI of Apppeal, Recreation Value, Swimmability probabilities by stressor fifth quantiles 

        
par(fig=c(0,1,.4,1)) 
PPlot5(Nout,Appealing,4,NA,"P[Appeal Rating = Highest]","blue","blue","gray45","goldenrod1","goldenrod1")
par(fig=c(0,1,0,.6),new=T) 
PPlot5(Nout,Recreation,3,"Max Sparrow [N] outflow ug/l (Quintiles)","P[Recreation Rating = Highest]",
                          "blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,.4,1)) 
PPlot5(Nout,Swimmable,2,NA,"P[Swimmable Rating = Highest]","blue","gray45","gray45","gray45","goldenrod1")  
par(fig=c(0,1,0,.6),new=T) 
PPlot5(Nout,Microcystin,0,"Max Sparrow [N] outflow ug/l (Quintiles)","P[Microcystin Detected]",
                          "blue","blue","gray45","gray45","goldenrod1")               

par(fig=c(0,1,.4,1)) 
PPlot5(Nout,Pristine,4,NA,"P[Pristine Rating = Highest]","blue","blue","gray45","goldenrod1","goldenrod1")       
par(fig=c(0,1,0,.6),new=T) 
PPlot5(Nout,Integrity,3,"Max Sparrow [N] outflow ug/l (Quintiles)","P[Biotic Integrity Rating Highest]",
                          "blue","blue","gray45","goldenrod1","goldenrod1")               
par(mfrow=c(1,1))  

     


###########################
par(fig=c(0,1,.4,1)) 
PPlot5(Est_ChlA,Appealing,4,"Estimated Max [Chla] ug/l (Quintiles)","P[Appeal Rating = Highest]",
                          "blue","blue","gray45","goldenrod1","goldenrod1")
par(fig=c(0,1,.4,1)) 
PPlot5(Est_ChlA,Swimmable,2,"Estimated Max [Chla] ug/l (Quintiles)","P[Swimmable Rating = Highest]",
                          "blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,.4,1)) 
PPlot5(Est_ChlA,Microcystin,0,"Estimated Max [Chla] ug/l (Quintiles)","P[Microcystin Detected]",
                          "blue","blue","blue","gray45","goldenrod1")               




#two plots per page
par(fig=c(0,1,.4,1)) 
#app=
PPlot5(Est_ChlA,Appealing,4,NA,"P[Appeal Rating = Highest]","blue","blue","goldenrod1","goldenrod1","goldenrod1")
par(fig=c(0,1,0,.6),new=T) 
#rec=
PPlot5(Est_ChlA,Recreation,3,"Estimated Max [Chla] ug/l (Quintiles)","P[Recreation Rating = Highest]",
                          "blue","blue","goldenrod1","gray45","goldenrod1")  
                          
par(fig=c(0,1,.4,1)) 
#swi=
PPlot5(Est_ChlA,Swimmable,2,NA,"P[Swimmable Rating = Highest]","blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,0,.6),new=T) 
#mic=
PPlot5(Est_ChlA,Microcystin,0,"Estimated Max [Chla] ug/l (Quintiles)","P[Microcystin Detected]",
                          "blue","blue","gray45","gray45","goldenrod1")               

                          
par(fig=c(0,1,.4,1)) 
#pri=
PPlot5(Est_ChlA,Pristine,4,NA,"P[Pristine Rating = Highest]","blue","blue","gray45","goldenrod1","goldenrod1")       
par(fig=c(0,1,0,.6),new=T) 
#bio=
PPlot5(Est_ChlA,Integrity,3,"Estimated Max [ChlaA] ug/l (Quintiles)","P[Biotic Integrity Rating Highest]",
                          "blue","blue","gray45","goldenrod1","goldenrod1")  
                          
                          
thresh=c("appeal",blueMax=app[1,2],greyMax=NA,yellowMax=app[1,5],
       blueMean=mean(app[2,1:2]),greyMean=NA,yellowMean=mean(app[2,3:5]))  
a=c("recreation",blueMax=rec[1,2],greyMax=rec[1,3],yellowMax=rec[1,5],
       blueMean=mean(rec[2,1:2]),greyMean=mean(rec[2,3]),yellowMean=mean(rec[2,4:5])) 
thresh=rbind(thresh,a)  
a=c("swimmable",blueMax=swi[1,2],greyMax=swi[1,4],yellowMax=swi[1,5],
       blueMean=mean(swi[2,1:2]),greyMean=mean(swi[2,3:4]),yellowMean=mean(swi[2,5])) 
thresh=rbind(thresh,a)  
a=c("microcystin",blueMax=mic[1,2],greyMax=mic[1,4],yellowMax=mic[1,5],
       blueMean=mean(mic[2,1:2]),greyMean=mean(mic[2,3:4]),yellowMean=mean(mic[2,5])) 
thresh=rbind(thresh,a)  
a=c("pristine",blueMax=pri[1,2],greyMax=pri[1,3],yellowMax=pri[1,5],
       blueMean=mean(pri[2,1:2]),greyMean=mean(pri[2,3]),yellowMean=mean(pri[2,4:5]))
thresh=rbind(thresh,a) 
a=c("biotic",blueMax=bio[1,2],greyMax=bio[1,3],yellowMax=bio[1,5],
       blueMean=mean(bio[2,1:2]),greyMean=mean(bio[2,3]),yellowMean=mean(bio[2,4:5])) 
thresh=rbind(thresh,a) 

write.table(thresh, file='//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1_Chla_Subj_Thresholds.csv',row.names=T,sep=',')


#threee plots per page
par(fig=c(0,1,.49,.99)) 
PPlot5a(Est_ChlA,Appealing,4,NA,"Appeal Rating","blue","blue","goldenrod1","goldenrod1","goldenrod1")
par(fig=c(0,1,.25,.75),new=T) 
PPlot5a(Est_ChlA,Recreation,3,NA,"Recreation","blue","blue","gray45","goldenrod1","goldenrod1")  
par(fig=c(0,1,.01,.51),new=T)  
PPlot5(Est_ChlA,Swimmable,2,"Quintile Max Estimated [ChlaA] ug/l","Swimmable Rating","blue","blue","gray45","gray45","goldenrod1")  

par(fig=c(0,1,.49,.99))
PPlot5a(Est_ChlA,Pristine,4,NA,"Pristine Rating","blue","blue","gray45","goldenrod1","goldenrod1")       
par(fig=c(0,1,.25,.75),new=T) 
PPlot5a(Est_ChlA,Integrity,3,NA,"Biotic Integrity Rating","blue","blue","gray45","goldenrod1","goldenrod1")  
par(fig=c(0,1,.01,.51),new=T) 
PPlot5(Est_ChlA,Microcystin,0,"Quintile Max Estimated [ChlaA] ug/l","Microcystin Detected",
                          "blue","blue","gray45","gray45","goldenrod1")    
                          
par(fig=c(0,1,.49,.99)) 
PPlot5a(Est_ChlA,Appealing,4,NA,"Appeal Rating","blue","blue","goldenrod1","goldenrod1","goldenrod1")
par(fig=c(0,1,.25,.75),new=T) 
PPlot5a(Est_ChlA,Integrity,3,NA,"Biotic Integrity Rating","blue","blue","gray45","goldenrod1","goldenrod1")  
par(fig=c(0,1,.01,.51),new=T) 
PPlot5(Est_ChlA,Microcystin,0,"Quintile Max Estimated [ChlaA] ug/l","Microcystin Detected",
                          "blue","blue","gray45","gray45","goldenrod1")   
###NLA data
par(fig=c(0,1,.49,.99)) 
PPlot5a(NTL,Appealing,4,NA,"Appeal Rating","blue","blue","goldenrod1","goldenrod1","goldenrod1")
par(fig=c(0,1,.25,.75),new=T) 
PPlot5a(NTL,Recreation,3,NA,"Recreation","blue","blue","gray45","goldenrod1","goldenrod1")  
par(fig=c(0,1,.01,.51),new=T)  
PPlot5(NTL,Swimmable,2,"Quintile Max NLA [NTL] ug/l","Swimmable Rating","blue","blue","gray45","gray45","goldenrod1")  


par(fig=c(0,1,.49,.99)) 
PPlot5a(PTL,Appealing,4,NA,"Appeal Rating","blue","blue","goldenrod1","goldenrod1","goldenrod1")
par(fig=c(0,1,.25,.75),new=T) 
PPlot5a(PTL,Recreation,3,NA,"Recreation","blue","blue","gray45","goldenrod1","goldenrod1")  
par(fig=c(0,1,.01,.51),new=T)  
PPlot5(PTL,Swimmable,2,"Quintile Max NLA [PTL] ug/l","Swimmable Rating","blue","blue","gray45","gray45","goldenrod1")  


par(fig=c(0,1,.49,.99)) 
PPlot5a(ChlA,Appealing,4,NA,"Appeal Rating","blue","blue","goldenrod1","goldenrod1","goldenrod1")
par(fig=c(0,1,.25,.75),new=T) 
PPlot5a(ChlA,Recreation,3,NA,"Recreation","blue","blue","gray45","goldenrod1","goldenrod1")  
par(fig=c(0,1,.01,.51),new=T)  
PPlot5(ChlA,Swimmable,2,"Quintile Max NLA [ChlaA] ug/l","Swimmable Rating","blue","blue","gray45","gray45","goldenrod1")  

par(fig=c(0,1,.49,.99)) 
PPlot5a(Secchi,Appealing,4,NA,"Appeal Rating","blue","blue","goldenrod1","goldenrod1","goldenrod1")
par(fig=c(0,1,.25,.75),new=T) 
PPlot5a(Secchi,Recreation,3,NA,"Recreation","blue","blue","gray45","goldenrod1","goldenrod1")  
par(fig=c(0,1,.01,.51),new=T)  
PPlot5(Secchi,Swimmable,2,"Quintile Max NLA Secchi ug/l","Swimmable Rating","blue","blue","gray45","gray45","goldenrod1")  


par(fig=c(0,1,.49,.99)) 
PPlot5a(NTL,Appealing,4,NA,"Appeal Rating","blue","blue","goldenrod1","goldenrod1","goldenrod1")
par(fig=c(0,1,.25,.75),new=T) 
PPlot5a(NTL,Integrity,3,NA,"Biotic Integrity Rating","blue","blue","gray45","goldenrod1","goldenrod1")  
par(fig=c(0,1,.01,.51),new=T) 
PPlot5(NTL,Microcystin,0,"Quintile Max NLA [NTL] ug/l","Microcystin Detected",
                          "blue","blue","gray45","gray45","goldenrod1")  

par(fig=c(0,1,.49,.99)) 
PPlot5a(PTL,Appealing,4,NA,"Appeal Rating","blue","blue","goldenrod1","goldenrod1","goldenrod1")
par(fig=c(0,1,.25,.75),new=T) 
PPlot5a(PTL,Integrity,3,NA,"Biotic Integrity Rating","blue","blue","gray45","goldenrod1","goldenrod1")  
par(fig=c(0,1,.01,.51),new=T) 
PPlot5(PTL,Microcystin,0,"Quintile Max NLA [PTL] ug/l","Microcystin Detected",
                          "blue","blue","gray45","gray45","goldenrod1")  
 
                          
par(fig=c(0,1,.49,.99)) 
PPlot5a(ChlA,Appealing,4,NA,"Appeal Rating","blue","blue","goldenrod1","goldenrod1","goldenrod1")
par(fig=c(0,1,.25,.75),new=T) 
PPlot5a(ChlA,Integrity,3,NA,"Biotic Integrity Rating","blue","blue","gray45","goldenrod1","goldenrod1")  
par(fig=c(0,1,.01,.51),new=T) 
PPlot5(ChlA,Microcystin,0,"Quintile Max NLA [ChlaA] ug/l","Microcystin Detected",
                          "blue","blue","gray45","gray45","goldenrod1")  
                          
par(fig=c(0,1,.49,.99)) 
PPlot5a(Secchi,Appealing,4,NA,"Appeal Rating","blue","blue","goldenrod1","goldenrod1","goldenrod1")
par(fig=c(0,1,.25,.75),new=T) 
PPlot5a(Secchi,Integrity,3,NA,"Biotic Integrity Rating","blue","blue","gray45","goldenrod1","goldenrod1")  
par(fig=c(0,1,.01,.51),new=T) 
PPlot5(Secchi,Microcystin,0,"Quintile Max NLA Secchi ug/l","Microcystin Detected",
                          "blue","blue","gray45","gray45","goldenrod1")       
     
                          
                          
par(fig=c(0,1,.49,.99))
PPlot5a(Est_ChlA,Entero,0,NA,"Enterococci detected","blue","blue","gray45","goldenrod1","goldenrod1")       
par(fig=c(0,1,.25,.75),new=T) 
PPlot5a(Est_ChlA,CyanoCount,20000,NA,"CyanoCounts GT 20k","blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,.01,.51),new=T) 
PPlot5(Est_ChlA,Microcystin,0,"Quintile Max Estimated [ChlaA] ug/l","Microcystin Detected",
                          "blue","blue","gray45","gray45","goldenrod1")              
           
par(fig=c(0,1,.49,.99))
PPlot5a(Secchi,CyanoCount,20000,NA,"CyanoCounts GT 20k","blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,.25,.75),new=T) 
PPlot5a(Pout,CyanoCount,20000,NA,"CyanoCounts GT 20k","blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,.01,.51),new=T) 
PPlot5(Est_ChlA,CyanoCount,20000,"Quintile Max Estimated [ChlaA] ug/l","Microcystin Detected",
                          "blue","blue","gray45","gray45","goldenrod1")              
############  4 plots
par(fig=c(0,1,.55,1))
PPlot5a(Nout,CyanoCount,20000,NA,"Cyano","blue","blue","gray45","gray45","goldenrod1")  
     text(,.8,"NTL")  
par(fig=c(0,1,.37,.82),new=T) 
PPlot5a(Pout,CyanoCount,20000,NA,"Cyano","blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,.19,.64),new=T)
PPlot5a(Est_ChlA,CyanoCount,20000,NA,"Cyano","blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,.01,.46),new=T) 
PPlot5(Est_Secchi,CyanoCount,20000,"Quintile Max Estimated [ChlaA] ug/l","Cyano",
                          "blue","blue","gray45","gray45","goldenrod1") 
                          text(.2,.8,"NTL")  
                          
par(fig=c(0,1,.01,.46),new=T) 
PPlot5(Nout,CyanoCount,20000,"Quintile Max Estimated [ChlaA] ug/l","Cyano",
                          "blue","blue","gray45","gray45","goldenrod1") 
                          text(log10(1500),.5,"NTL")  

 
 
par(fig=c(0,1,.55,1))
PPlot5a(Nout,Pristine,4,NA,"Cyano","blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,.37,.82),new=T) 
PPlot5a(Pout,Pristine,4,NA,"Cyano","blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,.19,.64),new=T)
PPlot5a(Est_ChlA,Pristine,4,NA,"Cyano","blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,.01,.46),new=T) 
PPlot5(Est_Secchi,Pristine,4,"Quintile Max Estimated [ChlaA] ug/l","Cyano",
                          "blue","blue","gray45","gray45","goldenrod1")   
                        
par(fig=c(0,1,.55,1))
PPlot5a(NTL,Pristine,4,NA,"Cyano","blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,.37,.82),new=T) 
PPlot5a(PTL,Pristine,4,NA,"Cyano","blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,.19,.64),new=T)
PPlot5a(ChlA,Pristine,4,NA,"Cyano","blue","blue","gray45","gray45","goldenrod1")  
par(fig=c(0,1,.01,.46),new=T) 
PPlot5(Secchi,Pristine,4,"Quintile Max Estimated [ChlaA] ug/l","Cyano",
                          "blue","blue","gray45","gray45","goldenrod1")   

           


     
                          
                                    
par(mfrow=c(1,1))  
test=summary(lm(log10(CyanoCount)~log10(PTL)))
test
plot(log10(PTL),log10(CyanoCount),sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

par(mfrow=c(1,1))  
test=summary(lm(log10(CyanoCount)~log10(Nout)))
test
plot(log10(Nout),log10(CyanoCount),sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

par(mfrow=c(1,1))  
test=summary(lm(log10(CyanoCount)~log10(ChlA)))
test
plot(log10(ChlA),log10(CyanoCount),sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)



#########################

 






       
PPlot5(Nout,Appealing,4,"Max Sparrow [N] outflow ug/l (Quintiles)","P[Appeal Rating = Highest]")     
PPlot5(Nout,Recreation,3,"Max Sparrow [N] outflow ug/l (Quintiles)","P[Recreation Rating = Highest]")       
PPlot5(Nout,Swimmable,2,"Max Sparrow [N] outflow ug/l (Quintiles)","P[Swimmable Rating = Highest]")  
PPlot5(Nout,Microcystin,0,"Max Sparrow [N] outflow ug/l (Quintiles)","P[Microcystin Detected]")       
PPlot5(Nout,Pristine,4,"Max Sparrow [N] outflow ug/l (Quintiles)","P[Pristine Rating = Highest]")       
PPlot5(Nout,Integrity,3,"Max Sparrow [N] outflow ug/l (Quintiles)","P[Biotic Integrity Rating Highest]")       

PPlot5(Pout,Microcystin,0,"Sparrow [P] outflow ug/l (Quintiles)","P[Microcystin Detected]")          
PPlot5(Pout,Appealing,4,"Sparrow [P] outflow ug/l (Quintiles)","P[Appeal Rating = Highest]")        
PPlot5(Pout,Recreation,3,"Sparrow [P] outflow ug/l (Quintiles)","P[Recreation Rating = Highest]")        
PPlot5(Pout,Swimmable,2,"Sparrow [P] outflow ug/l (Quintiles)","P[Swimmable Rating = Highest]")        
PPlot5(Pout,Pristine,4,"Sparrow [P] outflow ug/l (Quintiles)","P[Pristine Rating = Highest]")        
PPlot5(Pout,Integrity,3,"Sparrow [P] outflow ug/l (Quintiles)","P[Integrity Rating Highest]")  

################Functions:here to end

#################################

#Resample to get distribution for P[success]
PD=function(x){
  resample=c()
  for(i in 1:1000){resample=c(resample,sum(sample(x,replace=T))/length(x))}}

#########
#create  plots for quintiles
PPlot5=function(stress,response,GT,xlabel,ylabel,c1,c2,c3,c4,c5){
  q=na.exclude(data.frame(response=ifelse(response>GT,1,0),stress))
  q1=q$response[q$stress<=quantile(q$stress,.2)]
  q2=q$response[q$stress>quantile(q$stress,.2) & q$stress<=quantile(q$stress,.4)]
  q3=q$response[q$stress>quantile(q$stress,.4) & q$stress<=quantile(q$stress,.6)]
  q4=q$response[q$stress>quantile(q$stress,.6) & q$stress<=quantile(q$stress,.8)]
  q5=q$response[q$stress>quantile(q$stress,.8)]
 #Resample distributions
    ucpDist=PD(q[,1]); Q1=PD(q1);Q2=PD(q2);Q3=PD(q3);Q4=PD(q4);Q5=PD(q5)
x=round(log10(c(quantile(q$stress,.2),quantile(q$stress,.4),quantile(q$stress,.6),
    quantile(q$stress,.8),quantile(q$stress,1))),1)
y=c(mean(Q1),mean(Q2),mean(Q3),mean(Q4),mean(Q5))
upper<-c(quantile(Q1,.975),quantile(Q2,.975),quantile(Q3,.975),quantile(Q4,.975),quantile(Q5,.975))
lower<-c(quantile(Q1,.025),quantile(Q2,.025),quantile(Q3,.025),quantile(Q4,.025),quantile(Q5,.025))
plot(x,y,ylim=c(0,1),pch=NA,cex=1.5,col="green",lwd=2, ylab=ylabel,lty=1, xlab=xlabel,axes=F)
#polygon(x=c(x[1]-.1*x[1],x[1]-.1*x[1], x[5]+.1*x[5],x[5]+.1*x[5]),
polygon(x=c(0,0,log10(max(q$stress)),log10(max(q$stress))),
        y=c(quantile(ucpDist,.025),quantile(ucpDist,.975),quantile(ucpDist,.975),quantile(ucpDist,.025)), 
        density=NA, col="azure2", border=NA)
library(Hmisc)
lines(x, y,col="cyan",lwd=2)
errbar(x,y,upper,lower,add=T,xlab=NULL,ylab=NULL,lwd=3,cex=1.5,col=c(c1,c2,c3,c4,c5))
#axis(side=2,lwd.ticks=0,labels=NA)
axis(side=1,at=x,labels=round(10^x,1))
axis(side=2)
#axis(side=4,at=y,labels=round(y,2),cex.axis=.7,las=1)
box()
out=round(rbind(10^x,p=y),2)
return(out)
}

#create  plots for quintiles without x-axis ticks
PPlot5a=function(stress,response,GT,xlabel,ylabel,c1,c2,c3,c4,c5){
  q=na.exclude(data.frame(response=ifelse(response>GT,1,0),stress))
  q1=q$response[q$stress<=quantile(q$stress,.2)]
  q2=q$response[q$stress>quantile(q$stress,.2) & q$stress<=quantile(q$stress,.4)]
  q3=q$response[q$stress>quantile(q$stress,.4) & q$stress<=quantile(q$stress,.6)]
  q4=q$response[q$stress>quantile(q$stress,.6) & q$stress<=quantile(q$stress,.8)]
  q5=q$response[q$stress>quantile(q$stress,.8)]
 #Resample distributions
    ucpDist=PD(q[,1]); Q1=PD(q1);Q2=PD(q2);Q3=PD(q3);Q4=PD(q4);Q5=PD(q5)
x=round(log10(c(quantile(q$stress,.2),quantile(q$stress,.4),quantile(q$stress,.6),
    quantile(q$stress,.8),quantile(q$stress,1))),1)
y=c(mean(Q1),mean(Q2),mean(Q3),mean(Q4),mean(Q5))
upper<-c(quantile(Q1,.975),quantile(Q2,.975),quantile(Q3,.975),quantile(Q4,.975),quantile(Q5,.975))
lower<-c(quantile(Q1,.025),quantile(Q2,.025),quantile(Q3,.025),quantile(Q4,.025),quantile(Q5,.025))
plot(x,y,ylim=c(0,1),pch=NA,cex=1.5,col="green",lwd=2, ylab=ylabel,lty=1, xlab=xlabel,axes=F)
#polygon(x=c(x[1]-.1*x[1],x[1]-.1*x[1], x[5]+.1*x[5],x[5]+.1*x[5]),
polygon(x=c(0,0,log10(max(q$stress)),log10(max(q$stress))),
        y=c(quantile(ucpDist,.025),quantile(ucpDist,.975),quantile(ucpDist,.975),quantile(ucpDist,.025)), 
        density=NA, col="azure2", border=NA)
library(Hmisc)
lines(x, y,col="cyan",lwd=2)
errbar(x,y,upper,lower,add=T,xlab=NULL,ylab=NULL,lwd=3,cex=1.5,col=c(c1,c2,c3,c4,c5))
#axis(side=2,lwd.ticks=0,labels=NA)
#axis(side=1,at=x,labels=round(10^x,1))
axis(side=2)
#axis(side=4,at=y,labels=round(y,2),cex.axis=.7,las=1)
box()
out=round(rbind(10^x,p=y),2)
return(out)
}












