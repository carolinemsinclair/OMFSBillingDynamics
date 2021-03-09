library(readxl)
#script for loading RVU files

WORKGAF14=1.032
PEGAF14=1.187
MPGAF14=0.720

WORKGAF15=1.030
PEGAF15=1.180
MPGAF15=0.834

WORKGAF16=1.030
PEGAF16=1.180
MPGAF16=0.834

WORKGAF17=1.024
PEGAF17=1.091
MPGAF17=0.610

WORKGAF18=1.028
PEGAF18=1.108
MPGAF18=0.562

WORKGAF19=1.032
PEGAF19=1.126
MPGAF19=0.562

#jan 2014
jan14<-read_xlsx("PPRRVU14_V0324.xlsx")
jan14.HPCcode<-jan14[,1]
jan14.workRVU<-as.numeric(jan14$...6)
jan14.PERVU<-as.numeric(jan14$...7)
jan14.MPRVU<-as.numeric(jan14$...11)
jan14.CF<-as.numeric(jan14$...25)

jan14.OMFS<-((jan14.workRVU*WORKGAF14)+(jan14.PERVU*PEGAF14)+(jan14.MPRVU*MPGAF14))*jan14.CF

jan14df<-(jan14.HPCcode)
jan14df$OMFS<-jan14.OMFS
jan14df<-jan14df[10:length(jan14.OMFS),]
jan14colnames<-c("HPCCodes","OMFS")
colnames(jan14df)<-jan14colnames

write.csv(jan14df,file="OMFSJan14.csv")

#apr 2014
apr14<-read_xlsx("PPRRVU14_V0515.xlsx")
apr14.HPCcode<-apr14[,1]
apr14.workRVU<-as.numeric(apr14$...6)
apr14.PERVU<-as.numeric(apr14$...7)
apr14.MPRVU<-as.numeric(apr14$...11)
apr14.CF<-as.numeric(apr14$...25)

apr14.OMFS<-((apr14.workRVU*WORKGAF14)+(apr14.PERVU*PEGAF14)+(apr14.MPRVU*MPGAF14))*apr14.CF

apr14df<-(apr14.HPCcode)
apr14df$OMFS<-apr14.OMFS
apr14df<-apr14df[10:length(apr14.OMFS),]
apr14colnames<-c("HPCCodes","OMFS")
colnames(apr14df)<-apr14colnames

write.csv(apr14df,file="OMFSApr14.csv")

#jul 2014
jul14<-read_xlsx("PPRRVU14_V0815_v5.xlsx")
jul14.HPCcode<-jul14[,1]
jul14.workRVU<-as.numeric(jul14$...6)
jul14.PERVU<-as.numeric(jul14$...7)
jul14.MPRVU<-as.numeric(jul14$...11)
jul14.CF<-as.numeric(jul14$...25)

jul14.OMFS<-((jul14.workRVU*WORKGAF14)+(jul14.PERVU*PEGAF14)+(jul14.MPRVU*MPGAF14))*jul14.CF

jul14df<-(jul14.HPCcode)
jul14df$OMFS<-jul14.OMFS
jul14df<-jul14df[10:length(jul14.OMFS),]
jul14colnames<-c("HPCCodes","OMFS")
colnames(jul14df)<-jul14colnames

write.csv(jul14df,file="OMFSJul14.csv")

#oct 2014
oct14<-read_xlsx("PPRRVU14_V1219.xlsx")
oct14.HPCcode<-oct14[,1]
oct14.workRVU<-as.numeric(oct14$...6)
oct14.PERVU<-as.numeric(oct14$...7)
oct14.MPRVU<-as.numeric(oct14$...11)
oct14.CF<-as.numeric(oct14$...25)

oct14.OMFS<-((oct14.workRVU*WORKGAF14)+(oct14.PERVU*PEGAF14)+(oct14.MPRVU*MPGAF14))*oct14.CF

oct14df<-(oct14.HPCcode)
oct14df$OMFS<-oct14.OMFS
oct14df<-oct14df[10:length(oct14.OMFS),]
oct14colnames<-c("HPCCodes","OMFS")
colnames(oct14df)<-oct14colnames

write.csv(oct14df,file="OMFSOct14.csv")

#jan 2015
jan15<-read_xlsx("PPRRVU15_V1223c.xlsx")
jan15.HPCcode<-jan15[,1]
jan15.workRVU<-as.numeric(jan15$...6)
jan15.PERVU<-as.numeric(jan15$...7)
jan15.MPRVU<-as.numeric(jan15$...11)
jan15.CF<-as.numeric(jan15$...25)

jan15.OMFS<-((jan15.workRVU*WORKGAF15)+(jan15.PERVU*PEGAF15)+(jan15.MPRVU*MPGAF15))*jan15.CF

jan15df<-(jan15.HPCcode)
jan15df$OMFS<-jan15.OMFS
jan15df<-jan15df[10:length(jan15.OMFS),]
jan15colnames<-c("HPCCodes","OMFS")
colnames(jan15df)<-jan15colnames

write.csv(jan15df,file="OMFSJan15.csv")

#apr 2015
apr15<-read_xlsx("PPRRVU15_V0213_Current.xlsx")
apr15.HPCcode<-apr15[,1]
apr15.workRVU<-as.numeric(apr15$...6)
apr15.PERVU<-as.numeric(apr15$...7)
apr15.MPRVU<-as.numeric(apr15$...11)
apr15.CF<-as.numeric(apr15$...25)

apr15.OMFS<-((apr15.workRVU*WORKGAF15)+(apr15.PERVU*PEGAF15)+(apr15.MPRVU*MPGAF15))*apr15.CF

apr15df<-(apr15.HPCcode)
apr15df$OMFS<-apr15.OMFS
apr15df<-apr15df[10:length(apr15.OMFS),]
apr15colnames<-c("HPCCodes","OMFS")
colnames(apr15df)<-apr15colnames

write.csv(apr15df,file="OMFSApr15.csv")

#jul 2015
jul15<-read_xlsx("PPRRVU15_UP05_V0622.xlsx")
jul15.HPCcode<-jul15[,1]
jul15.workRVU<-as.numeric(jul15$...6)
jul15.PERVU<-as.numeric(jul15$...7)
jul15.MPRVU<-as.numeric(jul15$...11)
jul15.CF<-as.numeric(jul15$...25)

jul15.OMFS<-((jul15.workRVU*WORKGAF15)+(jul15.PERVU*PEGAF15)+(jul15.MPRVU*MPGAF15))*jul15.CF

jul15df<-(jul15.HPCcode)
jul15df$OMFS<-jul15.OMFS
jul15df<-jul15df[10:length(jul15.OMFS),]
jul15colnames<-c("HPCCodes","OMFS")
colnames(jul15df)<-jul15colnames

write.csv(jul15df,file="OMFSJul15.csv")

#oct 2015
oct15<-read_xlsx("PPRRVU15_OCT_V1001.xlsx")
oct15.HPCcode<-oct15[,1]
oct15.workRVU<-as.numeric(oct15$...6)
oct15.PERVU<-as.numeric(oct15$...7)
oct15.MPRVU<-as.numeric(oct15$...11)
oct15.CF<-as.numeric(oct15$...25)

oct15.OMFS<-((oct15.workRVU*WORKGAF15)+(oct15.PERVU*PEGAF15)+(oct15.MPRVU*MPGAF15))*oct15.CF

oct15df<-(oct15.HPCcode)
oct15df$OMFS<-oct15.OMFS
oct15df<-oct15df[10:length(oct15.OMFS),]
oct15colnames<-c("HPCCodes","OMFS")
colnames(oct15df)<-oct15colnames

write.csv(oct15df,file="OMFSOct15.csv")


#jan 2016
jan16<-read_xlsx("PPRRVU16_V0122.xlsx")
jan16.HPCcode<-jan16[,1]
jan16.workRVU<-as.numeric(jan16$...6)
jan16.PERVU<-as.numeric(jan16$...7)
jan16.MPRVU<-as.numeric(jan16$...11)
jan16.CF<-as.numeric(jan16$...25)

jan16.OMFS<-((jan16.workRVU*WORKGAF16)+(jan16.PERVU*PEGAF16)+(jan16.MPRVU*MPGAF16))*jan16.CF

jan16df<-(jan16.HPCcode)
jan16df$OMFS<-jan16.OMFS
jan16df<-jan16df[10:length(jan16.OMFS),]
jan16colnames<-c("HPCCodes","OMFS")
colnames(jan16df)<-jan16colnames

write.csv(jan16df,file="OMFSJan16.csv")

#apr 2016
apr16<-read_xlsx("PPRRVU16_April_V0202.xlsx")
apr16.HPCcode<-apr16[,1]
apr16.workRVU<-as.numeric(apr16$...6)
apr16.PERVU<-as.numeric(apr16$...7)
apr16.MPRVU<-as.numeric(apr16$...11)
apr16.CF<-as.numeric(apr16$...25)

apr16.OMFS<-((apr16.workRVU*WORKGAF16)+(apr16.PERVU*PEGAF16)+(apr16.MPRVU*MPGAF16))*apr16.CF

apr16df<-(apr16.HPCcode)
apr16df$OMFS<-apr16.OMFS
apr16df<-apr16df[10:length(apr16.OMFS),]
apr16colnames<-c("HPCCodes","OMFS")
colnames(apr16df)<-apr16colnames

write.csv(apr16df,file="OMFSApr16.csv")

#jul 2016
jul16<-read_xlsx("PPRRVU16_V0517.xlsx")
jul16.HPCcode<-jul16[,1]
jul16.workRVU<-as.numeric(jul16$...6)
jul16.PERVU<-as.numeric(jul16$...7)
jul16.MPRVU<-as.numeric(jul16$...11)
jul16.CF<-as.numeric(jul16$...25)

jul16.OMFS<-((jul16.workRVU*WORKGAF16)+(jul16.PERVU*PEGAF16)+(jul16.MPRVU*MPGAF16))*jul16.CF

jul16df<-(jul16.HPCcode)
jul16df$OMFS<-jul16.OMFS
jul16df<-jul16df[10:length(jul16.OMFS),]
jul16colnames<-c("HPCCodes","OMFS")
colnames(jul16df)<-jul16colnames

write.csv(jul16df,file="OMFSJul16.csv")

#oct 2016
oct16<-read_xlsx("PPRRVU16_V0804.xlsx")
oct16.HPCcode<-oct16[,1]
oct16.workRVU<-as.numeric(oct16$...6)
oct16.PERVU<-as.numeric(oct16$...7)
oct16.MPRVU<-as.numeric(oct16$...11)
oct16.CF<-as.numeric(oct16$...25)

oct16.OMFS<-((oct16.workRVU*WORKGAF16)+(oct16.PERVU*PEGAF16)+(oct16.MPRVU*MPGAF16))*oct16.CF

oct16df<-(oct16.HPCcode)
oct16df$OMFS<-oct16.OMFS
oct16df<-oct16df[10:length(oct16.OMFS),]
oct16colnames<-c("HPCCodes","OMFS")
colnames(oct16df)<-oct16colnames

write.csv(oct16df,file="OMFSOct16.csv")

#jan 2017
jan17<-read_xlsx("PPRRVU17_V1219.xlsx")
jan17.HPCcode<-jan17[,1]
jan17.workRVU<-as.numeric(jan17$...6)
jan17.PERVU<-as.numeric(jan17$...7)
jan17.MPRVU<-as.numeric(jan17$...11)
jan17.CF<-as.numeric(jan17$...25)

jan17.OMFS<-((jan17.workRVU*WORKGAF17)+(jan17.PERVU*PEGAF17)+(jan17.MPRVU*MPGAF17))*jan17.CF

jan17df<-(jan17.HPCcode)
jan17df$OMFS<-jan17.OMFS
jan17df<-jan17df[10:length(jan17.OMFS),]
jan17colnames<-c("HPCCodes","OMFS")
colnames(jan17df)<-jan17colnames

write.csv(jan17df,file="OMFSJan17.csv")

#apr 2017
apr17<-read_xlsx("PPRRVU17_V0209.xlsx")
apr17.HPCcode<-apr17[,1]
apr17.workRVU<-as.numeric(apr17$...6)
apr17.PERVU<-as.numeric(apr17$...7)
apr17.MPRVU<-as.numeric(apr17$...11)
apr17.CF<-as.numeric(apr17$...25)

apr17.OMFS<-((apr17.workRVU*WORKGAF17)+(apr17.PERVU*PEGAF17)+(apr17.MPRVU*MPGAF17))*apr17.CF

apr17df<-(apr17.HPCcode)
apr17df$OMFS<-apr17.OMFS
apr17df<-apr17df[10:length(apr17.OMFS),]
apr17colnames<-c("HPCCodes","OMFS")
colnames(apr17df)<-apr17colnames

write.csv(apr17df,file="OMFSApr17.csv")

#jul 2017
jul17<-read_xlsx("PPRRVU17_JULY_V0503.xlsx")
jul17.HPCcode<-jul17[,1]
jul17.workRVU<-as.numeric(jul17$...6)
jul17.PERVU<-as.numeric(jul17$...7)
jul17.MPRVU<-as.numeric(jul17$...11)
jul17.CF<-as.numeric(jul17$...25)

jul17.OMFS<-((jul17.workRVU*WORKGAF17)+(jul17.PERVU*PEGAF17)+(jul17.MPRVU*MPGAF17))*jul17.CF

jul17df<-(jul17.HPCcode)
jul17df$OMFS<-jul17.OMFS
jul17df<-jul17df[10:length(jul17.OMFS),]
jul17colnames<-c("HPCCodes","OMFS")
colnames(jul17df)<-jul17colnames

write.csv(jul17df,file="OMFSJul17.csv")

#oct 2017
oct17<-read_xlsx("PPRRVU17_OCT.xlsx")
oct17.HPCcode<-oct17[,1]
oct17.workRVU<-as.numeric(oct17$...6)
oct17.PERVU<-as.numeric(oct17$...7)
oct17.MPRVU<-as.numeric(oct17$...11)
oct17.CF<-as.numeric(oct17$...25)

oct17.OMFS<-((oct17.workRVU*WORKGAF17)+(oct17.PERVU*PEGAF17)+(oct17.MPRVU*MPGAF17))*oct17.CF

oct17df<-(oct17.HPCcode)
oct17df$OMFS<-oct17.OMFS
oct17df<-oct17df[10:length(oct17.OMFS),]
oct17colnames<-c("HPCCodes","OMFS")
colnames(oct17df)<-oct17colnames

write.csv(oct17df,file="OMFSOct17.csv")


#jan 2018
jan18<-read_xlsx("PPRRVU18_JAN.xlsx")
jan18.HPCcode<-jan18[,1]
jan18.workRVU<-as.numeric(jan18$...6)
jan18.PERVU<-as.numeric(jan18$...7)
jan18.MPRVU<-as.numeric(jan18$...11)
jan18.CF<-as.numeric(jan18$...25)

jan18.OMFS<-((jan18.workRVU*WORKGAF18)+(jan18.PERVU*PEGAF18)+(jan18.MPRVU*MPGAF18))*jan18.CF

jan18df<-(jan18.HPCcode)
jan18df$OMFS<-jan18.OMFS
jan18df<-jan18df[10:length(jan18.OMFS),]
jan18colnames<-c("HPCCodes","OMFS")
colnames(jan18df)<-jan18colnames

write.csv(jan18df,file="OMFSJan18.csv")

#apr 2018
apr18<-read_xlsx("PPRRVU18_APR.xlsx")
apr18.HPCcode<-apr18[,1]
apr18.workRVU<-as.numeric(apr18$...6)
apr18.PERVU<-as.numeric(apr18$...7)
apr18.MPRVU<-as.numeric(apr18$...11)
apr18.CF<-as.numeric(apr18$...25)

apr18.OMFS<-((apr18.workRVU*WORKGAF18)+(apr18.PERVU*PEGAF18)+(apr18.MPRVU*MPGAF18))*apr18.CF

apr18df<-(apr18.HPCcode)
apr18df$OMFS<-apr18.OMFS
apr18df<-apr18df[10:length(apr18.OMFS),]
apr18colnames<-c("HPCCodes","OMFS")
colnames(apr18df)<-apr18colnames

write.csv(apr18df,file="OMFSApr18.csv")

#jul 2018
jul18<-read_xlsx("PPRRVU18_JUL.xlsx")
jul18.HPCcode<-jul18[,1]
jul18.workRVU<-as.numeric(jul18$...6)
jul18.PERVU<-as.numeric(jul18$...7)
jul18.MPRVU<-as.numeric(jul18$...11)
jul18.CF<-as.numeric(jul18$...25)

jul18.OMFS<-((jul18.workRVU*WORKGAF18)+(jul18.PERVU*PEGAF18)+(jul18.MPRVU*MPGAF18))*jul18.CF

jul18df<-(jul18.HPCcode)
jul18df$OMFS<-jul18.OMFS
jul18df<-jul18df[10:length(jul18.OMFS),]
jul18colnames<-c("HPCCodes","OMFS")
colnames(jul18df)<-jul18colnames

write.csv(jul18df,file="OMFSJul18.csv")

#oct 2018
oct18<-read_xlsx("PPRRVU18_OCT.xlsx")
oct18.HPCcode<-oct18[,1]
oct18.workRVU<-as.numeric(oct18$...6)
oct18.PERVU<-as.numeric(oct18$...7)
oct18.MPRVU<-as.numeric(oct18$...11)
oct18.CF<-as.numeric(oct18$...25)

oct18.OMFS<-((oct18.workRVU*WORKGAF18)+(oct18.PERVU*PEGAF18)+(oct18.MPRVU*MPGAF18))*oct18.CF

oct18df<-(oct18.HPCcode)
oct18df$OMFS<-oct18.OMFS
oct18df<-oct18df[10:length(oct18.OMFS),]
oct18colnames<-c("HPCCodes","OMFS")
colnames(oct18df)<-oct18colnames

write.csv(oct18df,file="OMFSOct18.csv")

#jan 2019
jan19<-read_xlsx("PPRRVU19_V1213.xlsx")
jan19.HPCcode<-jan19[,1]
jan19.workRVU<-as.numeric(jan19$...6)
jan19.PERVU<-as.numeric(jan19$...7)
jan19.MPRVU<-as.numeric(jan19$...11)
jan19.CF<-as.numeric(jan19$...25)

jan19.OMFS<-((jan19.workRVU*WORKGAF19)+(jan19.PERVU*PEGAF19)+(jan19.MPRVU*MPGAF19))*jan19.CF

jan19df<-(jan19.HPCcode)
jan19df$OMFS<-jan19.OMFS
jan19df<-jan19df[10:length(jan19.OMFS),]
jan19colnames<-c("HPCCodes","OMFS")
colnames(jan19df)<-jan19colnames

write.csv(jan19df,file="OMFSJan19.csv")

#apr 2019
apr19<-read_xlsx("PPRRVU19_APR.xlsx")
apr19.HPCcode<-apr19[,1]
apr19.workRVU<-as.numeric(apr19$...6)
apr19.PERVU<-as.numeric(apr19$...7)
apr19.MPRVU<-as.numeric(apr19$...11)
apr19.CF<-as.numeric(apr19$...25)

apr19.OMFS<-((apr19.workRVU*WORKGAF19)+(apr19.PERVU*PEGAF19)+(apr19.MPRVU*MPGAF19))*apr19.CF

apr19df<-(apr19.HPCcode)
apr19df$OMFS<-apr19.OMFS
apr19df<-apr19df[10:length(apr19.OMFS),]
apr19colnames<-c("HPCCodes","OMFS")
colnames(apr19df)<-apr19colnames

write.csv(apr19df,file="OMFSApr19.csv")


#jul 2019
jul19<-read_xlsx("PPRRVU19_JUL.xlsx")
jul19.HPCcode<-jul19[,1]
jul19.workRVU<-as.numeric(jul19$...6)
jul19.PERVU<-as.numeric(jul19$...7)
jul19.MPRVU<-as.numeric(jul19$...11)
jul19.CF<-as.numeric(jul19$...25)

jul19.OMFS<-((jul19.workRVU*WORKGAF19)+(jul19.PERVU*PEGAF19)+(jul19.MPRVU*MPGAF19))*jul19.CF

jul19df<-(jul19.HPCcode)
jul19df$OMFS<-jul19.OMFS
jul19df<-jul19df[10:length(jul19.OMFS),]
jul19colnames<-c("HPCCodes","OMFS")
colnames(jul19df)<-jul19colnames

write.csv(jul19df,file="OMFSJul19.csv")


#oct 2019
oct19<-read_xlsx("PPRRVU19_OCT.xlsx")
oct19.HPCcode<-oct19[,1]
oct19.workRVU<-as.numeric(oct19$...6)
oct19.PERVU<-as.numeric(oct19$...7)
oct19.MPRVU<-as.numeric(oct19$...11)
oct19.CF<-as.numeric(oct19$...25)

oct19.OMFS<-((oct19.workRVU*WORKGAF19)+(oct19.PERVU*PEGAF19)+(oct19.MPRVU*MPGAF19))*oct19.CF

oct19df<-(oct19.HPCcode)
oct19df$OMFS<-oct19.OMFS
oct19df<-oct19df[10:length(oct19.OMFS),]
oct19colnames<-c("HPCCodes","OMFS")
colnames(oct19df)<-oct19colnames

write.csv(oct19df,file="OMFSOct19.csv")

rvu2003<-read.csv("RVU20030101.csv",header=FALSE)
rvu2003<-subset(rvu2003,select=c("V2","V7"))
write.csv(rvu2003,file="RVU2003.csv")


##########################
drugs<-read.csv("drugs.csv")
nodollar<-sapply(drugs$PRICE, function(x) substring(x,2))
drugs$PRICE<-nodollar
write.csv(drugs,file="updateddrugs.csv")



