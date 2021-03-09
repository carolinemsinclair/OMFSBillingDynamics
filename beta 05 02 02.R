library(shiny)
library(rsconnect)
library(lubridate)
library(dbplyr)
library(dplyr)
library(tidyr)
library(readr)
library(DT)
library(stringr)
library(writexl)
library(openxlsx)

    ####Load in CSV files####
    
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
      if (as.character(rowinput[2])!=""){
        code=as.character(rowinput[3])
        codedate=substr(as.character(rowinput[2]),1,10)
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
        if(is.na(OMFS.temp) || nchar(OMFS.temp)==0 || is.null(OMFS.temp) || length(OMFS.temp)==0){
          OMFS.amount=parse_number(charges)
        }
        else{
          OMFS.amount=OMFS.temp
        }
        return(OMFS.amount)
      }
      else{
        
        if(startsWith(code,"M") || startsWith(code, "m") || startsWith(code,"I")|| startsWith(code, "i")){
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
            if(is.na(OMFS.temp) || nchar(OMFS.temp)==0 || is.null(OMFS.temp) || length(OMFS.temp)==0){
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
    
    ####num DOS####
    
    numDOS<-function(inputbill){
      count=0
      datelist<-c(0,0,0)
      for (i in 4:nrow(inputbill)){
        if (inputbill$X[i]!="" & inputbill$X[i-1]=="" & inputbill$X[i-2]==""
            & inputbill$X[i-3] ==""& inputbill$X.4[i-3]==""){
          count=count+1
        }
        datelist[i]=count
      }
      return(datelist)
    }
    
    ####Totalling for DOS and Check for 0 balance####
    
    totaling<-function(inputbill){
      DOSlist<-unique(inputbill$DOSNumber) #unique list of DOS
      grandtotal<-0.00
      billwithPayments<-data.frame(matrix(ncol=ncol(inputbill))) #create template for new bill
      colnames(billwithPayments)<-colnames(inputbill) #name columns same thing
      for (i in DOSlist){
        totalforDOS<-0.00
        smallDOS<-inputbill %>% filter(DOSNumber==i) #grab small DOS
        paymentvector<-smallDOS$Charges #grab Charges column
        newpayment<-numeric(0)
        paymentvector<-append(paymentvector,smallDOS$Adjustments) #add adjustments to charges column
        for (n in paymentvector){ #for item in charges column
          if (startsWith(n, "(")){ #if negative ie payment
            amount<-parse_number(as.character(n)) #parse number
            amount=amount*-1 #make it negative
            newpayment<-append(newpayment,amount) #add to new payment
          }
          else{ #if not negative ie charge
            amount<-parse_number(as.character(n)) #parse number
            newpayment<-append(newpayment,amount) #add to new payment
          }
        }
        if(sum(newpayment,na.rm=TRUE)<=0){ #0 balance
          smallDOS$OMFS=c("")
        }
        else{ #total up the DOS
          totalforDOS<-sum(parse_number(smallDOS$OMFS),na.rm=TRUE)
          if (totalforDOS<0){
            totalforDOS=0.00
          }
        }
        filler1<-c("","","","","","","","______","______")
        total<-c("","","","","","","","Total",totalforDOS)
        grandtotal<-grandtotal+totalforDOS
        filler2<-c("","","","","","","","","")
        billwithPayments<-rbind(billwithPayments,smallDOS,filler1,total,filler2)
      }
      billwithPayments[1,]<-c("","","","","","","","Bill Total",grandtotal)
      return(billwithPayments)
    }
    
    ####cascading function for pre 2014 dates####
    
    acuchiro<-function(inputbill){
      acuchiroRange<-c(97000:97669,97671:98999) #define ranges for chiro and acu
      pre2014span<-seq.Date(from=as_date("2000-01-01"),to=as_date("2013-12-31"),by="day") #define ranges of dates for chiro and acu
      inputbill$DOS<-substr(as.character(inputbill$DOS),1,10) #set DOS to first 10 characters
      for (i in 1:nrow(inputbill)){
        if (suppressWarnings(parse_number(inputbill[i,3])[1])%in% acuchiroRange){ #check if in acu chiro codes
          if (mdy(inputbill[i,2]) %in% pre2014span){ #check if in appropriate date range for cascading
            inputbill$ACU[i]="Y"
          }
          else{
            inputbill$ACU[i]="N"#not in appropriate date range for cascading
          }
        }
        else{
          inputbill$ACU[i]="N" #not in acu chiro codes
        }
      }
      multiplier<-c(1,0.75,0.5,0.25,0,0,0,0) #cascading multipliers
      DOSlist<-unique(inputbill[inputbill$ACU=="Y",2]) #grabs DOS that are ACU dates
      for (date in DOSlist){
        datewithchiro<-inputbill[inputbill$DOS==date & inputbill$ACU=="Y",] #grabs date from bill
        orderofdates<-order(datewithchiro$OMFS,decreasing=TRUE) #orders dates by 
        count=1
        for (m in 1:length(orderofdates)){
          datewithchiro[orderofdates[m],9]<-parse_number(datewithchiro[orderofdates[m],9])*multiplier[count]
          count=count+1
        }
        inputbill[inputbill$DOS==date & inputbill$ACU=="Y",]<-datewithchiro
      }
      inputbill$ACU<-NULL
      return(inputbill)
    }
    
    ####rounding function for OMFS####
    
    roundOMFS<-function(inputbill){
      for (i in 1:nrow(inputbill)){
        if(nchar(inputbill[i,9])>0){
          inputbill[i,9]=format(round(as.numeric(inputbill[i,9]),digits=2),nsmall=2)}
      }
      return(inputbill)
    }
    
    ####format chargespayments column####
    
    formatChargesPayments<-function(inputbill){
      for (i in 1:length(inputbill$ChargesPayments)){
        if (startsWith(inputbill$ChargesPayments[i],"($")){
          i1=substr(inputbill$ChargesPayments[i],2,nchar(inputbill$ChargesPayments[i])-1)
          i2=paste("-",i1)
          inputbill$ChargesPayments[i]=i2
        }
      }
      return(inputbill)
    }
    
    ###################
    ####loadingbill & Generating Output####
    
    tempbill=(input$bill) #call in bill
    
    #showModal(modalDialog("Please wait while your bill is processing.", footer=NULL)) #starting loading message
    
    bill=read.csv(tempbill$datapath,colClasses = "character") #read bill
    patient.name<-bill[1,11]
    patient.dob<-bill[1,8]
    patient.id<-bill[1,9]
    total.charges<-bill[1,75]
    total.payments<-bill[1,76]
    total.adjustments<-bill[1,77]
    insurancebalance<-bill[1,78]
    #smallerbill<-bill[,1:15]
    smallerbill=bill[,49:63]
    smallerbill1<-as.data.frame(smallerbill)
    payments=(smallerbill1[,10:15])
    charges=(smallerbill1[,1:9])
    charges[,8]<-c("")
    
    newnames.payments<-c("X.1","X.2","X.3","X.4","X.6","X.7")
    colnames(payments)<-newnames.payments
    newnames.charges<-c("X","X.1","X.2","X.3","X.4","X.5","X.6","X.7","X.8")
    colnames(charges)<-newnames.charges
    payments$ID<-seq.int(nrow(payments))
    payments$X.5<-c("")
    payments$X.8<-c("")
    payments$X<-c("")
    
    charges$ID<-seq.int(nrow(charges))
    
    newbill<-rbind(payments,charges)
    
    newbill<-arrange_at(newbill,"ID")
    
    newbill$DOSNum<-numDOS(newbill)
    
    newbill1<-newbill[,order(colnames(newbill))]
    newbill1$ID<-NULL
    
    newbill1$X.8<-NULL
    
    colnamesbill<-c("DOSNumber","DOS","DOE","Code","Modifier","Description","Unit","ChargesPayments","Adjustments")
    
    colnames(newbill1)<-colnamesbill
    
    newbill1<-newbill1 %>% filter(ChargesPayments!="")
    
    newbill1$DOE<-NULL
    
    newbill1$Description<-strtrim(newbill1$Description,60)
    
    OMFSColumn<-apply(newbill1,1,findcode) #generate OMFS column
    
    updatedwithPayments<-paymentsmade(newbill1,OMFSColumn)
    
    updatedwithPay<-vapply(updatedwithPayments,paste, collapse=",", character(1L))
    
    newbill1$OMFS=updatedwithPay
    
    newbill2<-acuchiro(newbill1)
    
    newbill2<-roundOMFS(newbill2)
    
    newbill3<-totaling(newbill2)
    
    newbill4<-formatChargesPayments(newbill3)
    newbill4$DOSNumber<-NULL
    OMFStotal<-newbill4$OMFS[1]
    newbill4[1,]<-c("")
    
    summarytablerow1<-c("Patient Name:", patient.name,"","Total Charges:",total.charges,"","","")
    summarytablerow2<-c("Patient ID:", patient.id,"","Total Payments:",total.payments,"","","")
    summarytablerow3<-c("Patient DOB:", patient.dob,"","Total Adjustments:",total.adjustments,"","","")
    summarytablerow4<-c("OMFS:", OMFStotal,"","Insurance Balance:",insurancebalance,"","","")
    summarytablerow5<-c("","","","","","","","")
    summarytablerow6<-names(newbill4)
    summarydf<-rbind(summarytablerow1,summarytablerow2,summarytablerow3,summarytablerow4,summarytablerow5,
                     summarytablerow6)
    colnames(summarydf)<-names(newbill4)
    
    report<-rbind(summarydf,newbill4)
    
    #removeModal() #remove loading message
