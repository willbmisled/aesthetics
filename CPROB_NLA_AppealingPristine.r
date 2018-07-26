
#read the data
NLA<-read.csv(url("http://willbmisled.com/lakes/nla_test.csv"))

#Water Quality Thresholds for TP	Good Lt 20	Fair 20-80	Poor > 80
#PTL_POOR =1 if exceeds threshold for "Poor" designation
PTL_POOR<-(ifelse(is.na(NLA$PTL),NA,(ifelse(NLA$PTL>80,1,0))))
PTL_GOOD<-(ifelse(is.na(NLA$PTL),NA,(ifelse(NLA$PTL<20,1,0))))
IMPACTED<-(ifelse(is.na(NLA$CLASS),NA,(ifelse(NLA$CLASS=="TRASH",1,0))))
REFERENCE<-(ifelse(is.na(NLA$CLASS),NA,(ifelse(NLA$CLASS=="REF",1,0))))
#create dataframe of PRISTINE (1=least & 5=most), APPEALING (1=least & 5=most), 
#PTL_POOR (1=impacted & 0=not), PTL_GOOD (1=reference & 0=not),
#IMPACTED (overall 1=impacted & 0 = not), and REFERENCE (overall 1=reference & 0 = not)   
CTEST<-na.exclude(data.frame(APPEALING=NLA$APPEALNG,PRISTINE=NLA$PRISTINE,PTL_POOR,PTL_GOOD,
                             IMPACTED,REFERENCE))




