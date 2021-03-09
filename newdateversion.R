#attemptwithxls
library(readxl)
library(lubridate)
library(dbplyr)
library(dplyr)
library(tidyr)
library(readr)

drugs=read.csv("updateddrugs.csv")
blood=read.csv("blood.csv",header = FALSE)
wcprice=read.csv("WCprice.csv")
weird2007codes<-read.csv("weird2007codes.csv",header=FALSE)
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

####Find code in row input####

findcode<- function (rowinput) {
  column7.output=""
  if (as.character(rowinput[1])!=""){
    code=as.character(rowinput[3])
    codedate=substr(as.character(rowinput[1]),1,10)
    units=as.character(rowinput[6])
    charges=as.character(rowinput[7])
    column7.output=OMFS(codedate,code,units,charges)}
  return(column7.output)
}

####OMFS type####

OMFS <-function (codedate,code,units,charges) {
  parsed.date<-mdy(codedate)
  OMFS.amount=0.00
  OMFS.temp=0.00
  if (startsWith(code,"99070")){ #drug code
    OMFS.temp=search.drugs(code)
    
    if(nchar(OMFS.temp)==0){
      OMFS.amount=parse_number(charges)
    }
    else{
      OMFS.amount=OMFS.temp
    }
    return(OMFS.amount)
  }
  else{
    
    if(startsWith(code,"ML") | startsWith(code, "ml")){ #med legal code
      OMFS.amount=parse_number(charges)
    }
    else{
      if(startsWith(code,"WC")|startsWith(code,"wc")){
        if (year(parsed.date)>2013){
          if (year(parsed.date)==2014){
            OMFS.amount=wcprice$X2014[wcprice$code==code]
          }
          if (year(parsed.date)==2015){
            OMFS.amount=wcprice$X2015[wcprice$code==code]
          }
          if (year(parsed.date)==2016){
            OMFS.amount=wcprice$X2016[wcprice$code==code]
          }
          if (year(parsed.date)==2017){
            OMFS.amount=wcprice$X2017[wcprice$code==code]
          }
          if (year(parsed.date)==2018){
            OMFS.amount=wcprice$X2018[wcprice$code==code]
          }
          if (year(parsed.date)==2019){
            OMFS.amount=wcprice$X2019[wcprice$code==code]
          }
        }
      }
      
      else{
        OMFS.temp=search.OMFS(parsed.date,substr(code,1,5),units)
        if(is.na(OMFS.temp)||nchar(OMFS.temp)==0 || OMFS.temp==0.00){
          OMFS.amount=parse_number(charges)
        }
        else{
          OMFS.amount=OMFS.temp
        }
      }}
    return(OMFS.amount)
  }
  
}


####searching for OMFS is CSV files####

search.OMFS<-function(codedate,code,units){
  pre2014span<-seq.Date(from=as_date("2000-01-01"),to=as_date("2013-12-31"),by="day")
  weird2007span<-seq.Date(from=as_date("2007-02-15"),to=as_date("2013-12-31"),by="day")
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
  
  if (codedate %in% pre2014span){
    if(code %in% blood$V1){
      price=blood[blood$V1==code,2]
      price2=price*as.numeric(units)
    }
    
    else{
      if (codedate %in% weird2007span & code %in% weird2007codes$V1){
        price=weird2007codes[weird2007codes$V1==code,2]
        price2=price*as.numeric(units)
      }
      else{
        price=OMFS2003 %>% filter(V2==code) %>% select(V7)
        price=as.numeric(as.character(price[[1]]))
        if (length(price>1)){price=max(price)}
        units=as.numeric(units)
        price2=price*units
      }
      
    }
    return(as.character(price2))
  }
  
  if (codedate %in% jan14span){
    price=OMFSJan14 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% apr14span){
    price=OMFSApr14 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% jul14span){
    price=OMFSJul14 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% oct14span){
    price=OMFSOct14 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  
  if (codedate %in% jan15span){
    price=OMFSJan15 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% apr15span){
    price=OMFSApr15 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% jul15span){
    price=OMFSJul15 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% oct15span){
    price=OMFSOct15 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  
  if (codedate %in% jan16span){
    price=OMFSJan16 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% apr16span){
    price=OMFSApr16 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% jul16span){
    price=OMFSJul16 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% oct16span){
    price=OMFSOct16 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  
  if (codedate %in% jan17span){
    price=OMFSJan17 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% apr17span){
    price=OMFSApr17 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% jul17span){
    price=OMFSJul17 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% oct17span){
    price=OMFSOct17 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  
  if (codedate %in% jan18span){
    price=OMFSJan18 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% apr18span){
    price=OMFSApr18 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% jul18span){
    price=OMFSJul18 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% oct18span){
    price=OMFSOct18 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  
  if (codedate %in% jan19span){
    price=OMFSJan19 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% apr19span){
    price=OMFSApr19 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% jul19span){
    price=OMFSJul19 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
  if (codedate %in% oct19span){
    price=OMFSOct19 %>% filter(HPCCodes==code) %>% select(OMFS)
    price=as.numeric(as.character(price[[1]]))
    if (length(price>1)){price=max(price)}
    units=as.numeric(units)
    price2=price*units
    return(price2)
  }
}

####searching for drug codes####

search.drugs <-function(code){ #OMFS for drugs
  smallcode=gsub(".*-","",code)
  price=drugs %>% filter(Column2==smallcode) %>% select(PRICE)
  price=as.numeric(as.character(price[[1]]))
  return(price)
}

####payments####
paymentsmade<-function(bill,CalculatedOMFSColumn){
  for (i in 1: nrow(bill)){
    if (startsWith(as.character(bill[i,7]),"($")){
      CalculatedOMFSColumn[i]=parse_number(as.character(bill[i,7]))*-1
    }
  }
  return(CalculatedOMFSColumn)
}

###################
####loadingbill####

tempbill=file.choose(new=FALSE) #call in bill

bill=read.csv(tempbill,colClasses = "character") #read bill
smallerbill=bill[,1:15]
smallerbill<-as.data.frame(smallerbill)
payments=(smallerbill[,10:15])
charges=(smallerbill[,1:9])

newnames<-c("X.1","X.2","X.3","X.4","X.6","X.7")
colnames(payments)<-newnames
payments$ID<-seq.int(nrow(payments))
payments$X.5<-c("")
payments$X.8<-c("")
payments$X<-c("")

charges$ID<-seq.int(nrow(charges))

newbill<-rbind(payments,charges)

newbill<-arrange_at(newbill,"ID")
newbill1<-newbill[,order(colnames(newbill))]
newbill1$ID<-NULL

newbill1$X.8<-NULL

colnamesbill<-c("DOS","DOE","Procedue","Modifier","Description","Unit","Charges","Total Charges")

colnames(newbill1)<-colnamesbill

newbill1<-newbill1 %>% filter(Charges!="")

OMFSColumn<-apply(newbill1,1,findcode) #generate OMFS column

updatedwithPayments<-paymentsmade(newbill1,OMFSColumn)

updatedwithPay<-vapply(updatedwithPayments,paste, collapse=",", character(1L))

newbill1$OMFS=updatedwithPay

write.csv(file="outputbill.csv",newbill1, na="")

