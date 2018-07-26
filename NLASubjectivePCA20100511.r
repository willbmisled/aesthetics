rm(list=ls(all=T)) #clear workspace
# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
#con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/WaterbodyDatabase/WaterBodyDataBase.mdb")
#con <- odbcConnectAccess("L:/Public/Milstead_Lakes/WaterbodyDatabase/WaterbodyDatabase.mdb")
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
NLA <- sqlQuery(con, "
SELECT tblJoinNLAID_WBID.WB_ID, NLA2007Sites_DesignInfo.SITE_ID, tblNLA_WaterQualityData.PTL, tblNLA_WaterQualityData.NTL, tblNLA_WaterQualityData.CL, tblNLA_WaterQualityData.TURB, tblNLA_WaterQualityData.ANC, tblNLA_WaterQualityData.SO4, tblNLA_WaterQualityData.DO2_2M, tblNLA_Habitat_Metrics_Part_B.RDisInAg, tblNLA_Habitat_Metrics_Part_B.RDisInNonAG, tblNLA_Habitat_Metrics_Part_B.RDisInEx1a, tblNLA_VisualAssessment.SWIMMABILITY, tblNLA_VisualAssessment.PRISTINE, tblNLA_VisualAssessment.APPEALING, tblNLA_VisualAssessment.BIOTIC_INTEGRITY, tblNLA_VisualAssessment.TROPHIC_STATE, tblNLA_VisualAssessment.RECREATIONAL_VALUE, tblNLA_Microcystin.Microcystin_ugl
FROM (((tblJoinNLAID_WBID INNER JOIN (NLA2007Sites_DesignInfo INNER JOIN tblNLA_WaterQualityData ON NLA2007Sites_DesignInfo.SITE_ID = tblNLA_WaterQualityData.SITE_ID) ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) INNER JOIN tblNLA_VisualAssessment ON (tblNLA_WaterQualityData.VISIT_NO = tblNLA_VisualAssessment.VISIT_NO) AND (tblNLA_WaterQualityData.SITE_ID = tblNLA_VisualAssessment.SITE_ID)) INNER JOIN tblNLA_Microcystin ON tblNLA_WaterQualityData.SITE_ID = tblNLA_Microcystin.SITE_ID) INNER JOIN tblNLA_Habitat_Metrics_Part_B ON (tblNLA_WaterQualityData.VISIT_NO = tblNLA_Habitat_Metrics_Part_B.VISIT_NO) AND (tblNLA_WaterQualityData.SITE_ID = tblNLA_Habitat_Metrics_Part_B.SITE_ID)
WHERE (((tblNLA_WaterQualityData.VISIT_NO)=1));
")
close(con)
names(NLA)

#Data definitions from input file
  # [1] WB_ID: unique lake identification number           
  # [2] SITE_ID: National Lake Assessment (NLA) Lake Identification Number           
  # [3] PTL (ug/l): Total Phosphorus                
  # [4] NTL (ug/l): Total Nitrogen   
  # [5] CL: Chloride (ueq/L)                
  # [6] TURB: Turbidity (NTU)              
  # [7] ANC: Gran ANC (ueq/L)                
  # [8] SO4: Sulfate (ueq/L)               
  # [9] DO2_2M: MEAN DO2 CONC (mg/L) IN UPPER 2m (or UPPER 50% IF DEPTH < 4m)             
  #[10] RDisInAg: Riparian Agricultural Disturbance Intensity Index from tblNLA_Habitat_Metrics_Part_B             
  #[11] RDisInNonAG: Riparian Non-Agricultural Disturbance Intensity Index from tblNLA_Habitat_Metrics_Part_B          
  #[12] RDisInEx1a: Riparian Disturbance Intensity and Extent Index from tblNLA_Habitat_Metrics_Part_B        
  #[13] SWIMMABILITY: from NLA Visual Assessment
  #[14] PRISTINE: from NLA Visual Assessment
  #[15] APPEALING: from NLA Visual Assessment
  #[16] BIOTIC_INTEGRITY: from NLA Visual Assessment
  #[17] TROPHIC_STATE: from NLA Visual Assessment
  #[18] RECREATIONAL_VALUE: from NLA Visual Assessment
  #[19] Microcystin_ugl:   


 
#Generate matrix of transformed PCA variable and remove missing values
#Data transformed to minimize skew
#Ignore warnings of NAN's produced
   tVARS<- na.exclude(data.frame(P=log10(NLA$PTL+1),N=log10(NLA$NTL+1),CL=log10(NLA$CL+1),
                  TURB=log(log(NLA$TURB+1)+1),ANC=log10(NLA$ANC+1),SO4=log(log(NLA$SO4+1)+1),
                  DO=NLA$DO2_2M,InAg=sqrt(NLA$RDisInAg),NonAg=NLA$RDisInNonAG,
                  Ex1a=NLA$RDisInEx1a,SiteID=NLA$SITE_ID))
print("Ignore warnings of NAN's produced")                 

 #Calculated Fields
    #Appealing: Low=1 & 2; Med=3 & 4; High=5
         Appealing<-factor(NLA$APPEALING)
            levels(Appealing) <-c("Low","Low","Med","Med","High")
 
    #Pristine: Low=1 & 2; Med=3 & 4; High=5
         Pristine<-factor(NLA$PRISTINE)
            levels(Pristine) <-c("Low","Low","Med","Med","High")

    #Integrity: Low="Poor" or "Fair"; Med=""Good"; High="Excellent"
         Integrity<-factor(NLA$BIOTIC_INTEGRITY)
            levels(Integrity) <-c("High","Low","Med","Low")
                       
    #Recreation: Low="Poor" or "Fair"; Med=""Good"; High="Excellent"
         Recreation<-factor(NLA$RECREATIONAL_VALUE)
            levels(Recreation) <-c("High","Low","Med","Low")
                       
    #Swimmable: Low="Poor" or "Fair"; Med=""Good"; High="Excellent"
         Swimmable<-factor(NLA$SWIMMABILITY)
            levels(Swimmable) <-c("Med","High","Low")
           
#Generate data frame of categorical variables 
temp<- data.frame(SiteID=NLA$SITE_ID,Appealing,Pristine,Integrity,Recreation,Swimmable)        

#Match Merge transformed PCA variables and categorical variables
#PCA variables are tVARS[,2:11]
   tVARS<-merge(tVARS, temp, by.x="SiteID",by.y="SiteID", all= FALSE)


#Principal Components Analysis
  library(ade4) #ade4 package must be installed
  pca1 <- dudi.pca(df=tVARS[,2:11],center=TRUE,scale=TRUE,scannf=FALSE,nf=10)

#Function to perform Between Group Analysis of PCA Results and Create Plots
PCA_Cat <- function(f,label,c1,c2,c3,pcaX,pcaY)
  {
    y<-factor(f)
    Out1 <- between(dudi = pca1, fac = y, scannf = FALSE, nf = 10)
    par(mfrow=c(2,1))
    #s.arrow(Out1$co,1,2, sub="column coordinates",csub=.75)
    s.arrow(Out1$c1,pcaX,pcaY,sub="class normed column coordinates",csub=.75)
    s.class(dfxy=Out1$ls, fac=y, xax=pcaX, yax=pcaY,
      add.plot=F,cstar=0, #col=rainbow(length(levels(y))),
      col=c(c1,c2,c3),
      sub = label, csub = .75)
      
      
  Per=Out1$c1  #loadings
  names(Per)=100*round(Out1$eig/sum(Out1$eig),3)  #add percent contribution as column name
  return(Per)
      

    par(mfrow=c(1,1))
  }

#Plot Results
  #Turn window recording on
  #in Graphics window use PageUp and PageDown to scroll through results
  #scroll wheel will not work
    windows(record=T)


    PCA_Cat(tVARS$Pristine,"Pristine Class","red","grey","green",1,2)
    PCA_Cat(tVARS$Appealing,"Appealing Class","red","grey","green",1,2)
    PCA_Cat(tVARS$Swimmable,"Swimmable Class","red","grey","green",1,2)
    PCA_Cat(tVARS$Integrity,"Integrity Class","green","red","grey",1,2)
    PCA_Cat(tVARS$Recreation,"Recreation Class","green","red","grey",1,2)
    
    

#end of file

