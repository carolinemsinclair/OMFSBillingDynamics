library(readxl)
library(lubridate)
library(dbplyr)
library(dplyr)
library(tidyr)

drugs=read.csv("updateddrugs.csv")
OMFS2003<-read.csv("RVU2003.csv")
OMFSJan14<-read.csv("OMFSJan14.csv")
OMFSApr14<-read.csv("OMFSApr14.csv")
OMFSJul14<-read.csv("OMFSJul14.csv")
OMFSOct14<-read.csv("OMFSOct14.csv")

OMFSJan15<-read.csv("OMFSJan15.csv")
OMFSApr15<-read.csv("OMFSApr15.csv")
OMFSJul15<-read.csv("OMFSJul15.csv")
OMFSOct15<-read.csv("OMFSOct15.csv")

OMFSJan16<-read.csv("OMFSJan16.csv")
OMFSApr16<-read.csv("OMFSApr16.csv")
OMFSJul16<-read.csv("OMFSJul16.csv")
OMFSOct16<-read.csv("OMFSOct16.csv")

OMFSJan17<-read.csv("OMFSJan17.csv")
OMFSApr17<-read.csv("OMFSApr17.csv")
OMFSJul17<-read.csv("OMFSJul17.csv")
OMFSOct17<-read.csv("OMFSOct17.csv")

OMFSJan18<-read.csv("OMFSJan18.csv")
OMFSApr18<-read.csv("OMFSApr18.csv")
OMFSJul18<-read.csv("OMFSJul18.csv")
OMFSOct18<-read.csv("OMFSOct18.csv")

OMFSJan19<-read.csv("OMFSJan19.csv")
OMFSApr19<-read.csv("OMFSApr19.csv")
OMFSJul19<-read.csv("OMFSJul19.csv")
OMFSOct19<-read.csv("OMFSOct19.csv")

findcode<- function (rowinput) {
  column7.output=""
  if (rowinput[1]!="" & rowinput[2]!="" & rowinput[6]!=""){
    code=rowinput[2]
    date=rowinput[1]
    units=rowinput[4]
    charges=rowinput[5]
    column7.output=OMFS(date,code,units,charges)}
  return(column7.output)
}

OMFS <-function (date,code,units,charges) {
  parsed.date<-mdy(date)
  OMFS.amount=0.00
  OMFS.temp=0.00
  if (startsWith(code,"99070")){ #drug code
    OMFS.temp=search.drugs(code)
    if(length(OMFS.temp)==0 || OMFS.temp==0.00){
      OMFS.amount=gsub(".*$","",charges)
    }
    else{
      OMFS.amount=OMFS.temp
    }
  }
  else{
    if(startsWith(code,"ML") | startsWith(code, "ml")){ #med legal code
      OMFS.amount=gsub(".*$","",charges)
    }
    else{
      OMFS.temp=search.OMFS(parsed.date,substr(as.character(code[1]),1,5),units)
      if(length(OMFS.temp)==0){
        OMFS.amount=gsub(".*$","",charges)
      }
      else{
        OMFS.amount=OMFS.temp
      }
    }
  }
  return(OMFS.amount)
}

search.OMFS<-function(date,code,units){
  pre2014span<-seq.Date(from=as_date("2000-01-01"),to=as_date("2013-12-31"),by="day")
  jan14span<-seq.Date(from=as_date("2014-01-01"),to=as_date("2014-03-31"),by="day")
  apr14span<-seq.Date(from=as_date("2014-04-01"),to=as_date("2014-06-30"),by="day")
  jul14span<-seq.Date(from=as_date("2014-07-01"),to=as_date("2014-09-30"),by="day")
  oct14span<-seq.Date(from=as_date("2014-10-01"),to=as_date("2014-12-31"),by="day")
  
  jan15span<-seq.Date(from=as_date("2015-01-01"),to=as_date("2015-03-31"),by="day")
  apr15span<-seq.Date(from=as_date("2015-04-01"),to=as_date("2015-06-30"),by="day")
  jul15span<-seq.Date(from=as_date("2015-07-01"),to=as_date("2015-09-30"),by="day")
  oct15span<-seq.Date(from=as_date("2015-10-01"),to=as_date("2015-12-31"),by="day")
  
  jan16span<-seq.Date(from=as_date("2016-01-01"),to=as_date("2016-03-31"),by="day")
  apr16span<-seq.Date(from=as_date("2016-04-01"),to=as_date("2016-06-30"),by="day")
  jul16span<-seq.Date(from=as_date("2016-07-01"),to=as_date("2016-09-30"),by="day")
  oct16span<-seq.Date(from=as_date("2016-10-01"),to=as_date("2016-12-31"),by="day")
  
  jan17span<-seq.Date(from=as_date("2017-01-01"),to=as_date("2017-03-31"),by="day")
  apr17span<-seq.Date(from=as_date("2017-04-01"),to=as_date("2017-06-30"),by="day")
  jul17span<-seq.Date(from=as_date("2017-07-01"),to=as_date("2017-09-30"),by="day")
  oct17span<-seq.Date(from=as_date("2017-10-01"),to=as_date("2017-12-31"),by="day")
  
  jan18span<-seq.Date(from=as_date("2018-01-01"),to=as_date("2018-03-31"),by="day")
  apr18span<-seq.Date(from=as_date("2018-04-01"),to=as_date("2018-06-30"),by="day")
  jul18span<-seq.Date(from=as_date("2018-07-01"),to=as_date("2018-09-30"),by="day")
  oct18span<-seq.Date(from=as_date("2018-10-01"),to=as_date("2018-12-31"),by="day")
  
  jan19span<-seq.Date(from=as_date("2019-01-01"),to=as_date("2019-03-31"),by="day")
  apr19span<-seq.Date(from=as_date("2019-04-01"),to=as_date("2019-06-30"),by="day")
  jul19span<-seq.Date(from=as_date("2019-07-01"),to=as_date("2019-09-30"),by="day")
  oct19span<-seq.Date(from=as_date("2019-10-01"),to=as_date("2019-12-31"),by="day")
  
  if (date %in% pre2014span){
    Blood<-c("99001","99000","80050") #special blood codes
    if (code %in% Blood){
      if (code=="99001" | code=="99000"){
        price2=8.61
      }
      else{
        price2=43.8
      }
    }
    
    else{
    price=OMFS2003 %>% filter(V2==code) %>% select(V7)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    }
    return(price2)
  }
  
  if (date %in% jan14span){
    price=OMFSJan14 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% apr14span){
    price=OMFSApr14 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% jul14span){
    price=OMFSJul14 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% oct14span){
    price=OMFSOct14 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  
  if (date %in% jan15span){
    price=OMFSJan15 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% apr15span){
    price=OMFSApr15 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% jul15span){
    price=OMFSJul15 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% oct15span){
    price=OMFSOct15 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  
  if (date %in% jan16span){
    price=OMFSJan16 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% apr16span){
    price=OMFSApr16 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% jul16span){
    price=OMFSJul16 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% oct16span){
    price=OMFSOct16 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  
  if (date %in% jan17span){
    price=OMFSJan17 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% apr17span){
    price=OMFSApr17 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% jul17span){
    price=OMFSJul17 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% oct17span){
    price=OMFSOct17 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  
  if (date %in% jan18span){
    price=OMFSJan18 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% apr18span){
    price=OMFSApr18 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% jul18span){
    price=OMFSJul18 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% oct18span){
    price=OMFSOct18 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  
  if (date %in% jan19span){
    price=OMFSJan19 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% apr19span){
    price=OMFSApr19 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% jul19span){
    price=OMFSJul19 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (date %in% oct19span){
    price=OMFSOct19 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
}

search.drugs <-function(code){ #OMFS for drugs
  smallcode=gsub(".*-","",code)
  price=drugs %>% filter(Column2==smallcode) %>% select(PRICE)
  price=as.numeric(as.character(price[[1]]))
  return(price)
}

payments<-function(bill){
  locationofpayments
}

#####################################################
tempbill=file.choose(new=FALSE) #call in bill
bill=read.csv(tempbill) #read bill

bill1=bill[,1:6] #only the things we need
bill1=bill1[2:nrow(bill1),] #get rid of header
bill2=bill1 %>% filter(Patient.Ledger....Order.v1.1!="" | X.4!="") #only codes and amounts

namerows<-c("Date","Procedure","Description","Units","Charges","Payments") #change the column names
colnames(bill2)<-namerows

OMFSColumn<-apply(bill2,1,findcode)

bill2$OMFS=OMFSColumn

