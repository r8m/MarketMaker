###############################################################################
################## SEPARATE TICK AND BID_ASK SMARTCOM LOG READER###############
library(data.table)
options(digits.secs=3)

fname<-"f:/TRADE/Data/research/SI/"
setwd(fname)

fileList<-dir()
fileList[grepl("Ticks",fileList)]
fileList[grepl("BidAsk",fileList)]
tickDT<-rbindlist(lapply(fileList[grepl("Ticks",fileList)],
                         FUN=fread,
                         sep=",",
                         header=FALSE, 
                         stringsAsFactors=FALSE))
bidaskDT<-rbindlist(lapply(fileList[grepl("BidAsk",fileList)],
                           FUN=fread,
                           sep=",",
                           header=FALSE, 
                           stringsAsFactors=FALSE))

bidaskHeader<-c("datetime",
                "Symbol",
                "brokerDateTime",
                "Row",
                "NRows",
                "Bid",
                "BidSize",
                "Ask",
                "AskSize")

tickHeader<-c("datetime",
              "Symbol",
              "brokerDateTime",
              "price",
              "volume",
              "buysell")

setnames(tickDT,tickHeader)
setnames(bidaskDT,bidaskHeader)
dtFormat<-"%d-%m-%Y %H:%M:%OS"
tickDT[,"datetime":=as.POSIXct(strptime(datetime,dtFormat))]
bidaskDT[,"datetime":=as.POSIXct(strptime(datetime,dtFormat))]

dtFormat<-"%m/%d/%Y %H:%M:%OS"
tickDT[,"brokerDateTime":=as.POSIXct(strptime(brokerDateTime,dtFormat))]
bidaskDT[,"brokerDateTime":=as.POSIXct(strptime(brokerDateTime,dtFormat))]
#bidaskDT<-bidaskDT[Row==0,]
bidaskHeader<-c("datetime", 
                paste("bidprice", 0:max(bidaskDT$Row), sep=""),
                paste("bidvolume", 0:max(bidaskDT$Row), sep=""),
                paste("askprice", 0:max(bidaskDT$Row), sep=""),
                paste("askvolume", 0:max(bidaskDT$Row), sep=""))

bidaskDT<-dcast.data.table(bidaskDT,datetime~Row, fun= min,value.var=c("Bid","BidSize","Ask", "AskSize"))

setnames(bidaskDT,bidaskHeader)
setkey(tickDT, datetime)
setkey(bidaskDT, datetime)

tbaDT<-bidaskDT[tickDT,roll=T]
for (j in 1:ncol(tbaDT)) set(tbaDT, which(is.infinite(tbaDT[[j]])), j, NA)
tbaDT<-tbaDT[complete.cases(tbaDT),]

for (j in 1:ncol(tickDT)) set(tickDT, which(is.infinite(tickDT[[j]])), j, NA)
tickDT<-tickDT[complete.cases(tickDT),]

for (j in 1:ncol(bidaskDT)) set(bidaskDT, which(is.infinite(bidaskDT[[j]])), j, NA)
bidaskDT<-bidaskDT[complete.cases(bidaskDT),]

df<-tbaDT[,.SD,.SDcols=c(tickHeader, bidaskHeader[-1])]
save(df,bidaskDT, tickDT, file=paste(tbaDT$Symbol[1], as.Date(tbaDT$datetime[1]),".RData", sep=""))
#rm(tbaDT, bidaskDT, tickDT)
library(ggplot2)
ggplot(data=df[datetime>=as.POSIXct("2015-12-21 10:15:05") & datetime<=as.POSIXct("2015-12-21 10:20:05")])+
  geom_point(aes(datetime,price), colour="darkgrey")+
  geom_point(aes(datetime,askprice0), colour="lightcoral", alpha=I(0.5))+
  geom_point(aes(datetime,bidprice0), colour="mediumaquamarine",alpha=I(0.5))

###################################################################################
###################### PLAZA DATA FROM zxweed (combined tick + bid/ask) #####################################
library(data.table)
options(digits.secs=3)
setwd("~/repos/Data/research/RI/")
fileList<-dir()


makeData<-function(fname) {
  #<-"SBRF-3.16_04.01.2016.csv"
  tbaDT<-fread(fname,sep=";",stringsAsFactors=FALSE)
  colnames(tbaDT)<-tolower(colnames(tbaDT))
  dtFormat<-"%d.%m.%Y %H:%M:%OS"
  tbaDT[,"datetime":=as.POSIXct(strptime(datetime,dtFormat))]
  setnames(tbaDT,"sign", "buysell")
  df<-tbaDT
  save(df, file=paste("RTS-3.16", as.Date(tbaDT$datetime[1]),".RData", sep=""))
}

lapply(fileList, FUN=makeData)



###################################################################################
###################### PLAZA DATA FROM zxweed (separate tick and bid/ask) #####################################
library(data.table)
options(digits.secs=3)
setwd("~/repos/Data/research/SBRF/")
symb<-"SBRF-3.16"
fileList<-dir()

fileList<-fileList[grepl("snap",fileList)]
fileList<-gsub(".snap","",fileList)
makeData<-function(fname,symb){
  obDT<-fread(paste(fname,".snap",sep=""), sep=";", stringsAsFactors = FALSE)
  setnames(obDT,c("datetime","bidprice0","askprice0","bidvolume0","bidvolume1","bidvolume2","askvolume0","askvolume1","askvolume2"))
  dtFormat<-"%d.%m.%y %H:%M:%OS"
  obDT[,datetime:=as.POSIXct(strptime(datetime,dtFormat))]
  
  
  tickDT<-fread(paste(fname,".tick",sep=""), sep="auto", stringsAsFactors = FALSE, header=FALSE)
  tickDT<-tickDT[,.(V2,V4,V5)]
  setnames(tickDT,c("datetime", "price", "volume"))
  tickDT[,price:=gsub("Price=","",price)]
  tickDT[,price:=as.numeric(gsub(" ","",price))]
  tickDT[,volume:=gsub("Volume=","",volume)]
  tickDT[,volume:=as.numeric(gsub(" ","",volume))]
  tickDT[,buysell:=ifelse(volume>0,"buy", "sell")]
  tickDT[,volume:=abs(volume)]
  dtFormat<-"%d.%m.%Y %H:%M:%OS"
  tickDT[,datetime:=as.POSIXct(strptime(datetime,dtFormat))]

  save(tickDT,obDT, file=paste(symb, as.Date(tickDT$datetime[1]),".RData", sep=""))
}

lapply(fileList, FUN=function(x) makeData(x,symb))


###################### PLAZA DATA FROM FULL ORDERLOG (MY CLASSES) #####################################
library(data.table)
options(digits.secs=3)
setwd("~/repos/Data/research/SI/")
symb<-"SI-3.16"
fileList<-dir()
makeBidAskData<-function(fname)
{
  print(paste(Sys.time(),"Start procesing bid/ask data from",fname))
  obDT<-fread(fname, sep=",", stringsAsFactors = FALSE)
  obDT[,`:=`(V2,NULL),]
  obDT[,`:=`(V9,NULL),]
  setnames(obDT,c("datetime","bidprice0","bidvolume0",
                  "bidprice1","bidvolume1",
                  "bidprice2","bidvolume2",
                  "askprice0","askvolume0",
                  "askprice1","askvolume1",
                  "askprice2","askvolume2"))
  dtFormat<-"%Y-%m-%d %H:%M:%OS"
  obDT[,datetime:=as.POSIXct(strptime(datetime,dtFormat))]
  # Filter 
  #' if best bid and best ask were not changed.
  #' if price=0
  
  obDT<-obDT[askprice0>0 &bidprice0>0 &  askprice1>0 & bidprice1>0 &  askprice2>0 & bidprice2>0]
  obDT<-obDT[askprice0>bidprice0]
  print(paste(Sys.time(),"Found",obDT[,.N]," bidasks within ",obDT[1,datetime],"-",obDT[.N,datetime]))
  #obDT<-obDT[shift(askprice0)!=askprice0 | shift(bidprice0)!=bidprice0]
}

makeTickData<-function(fname)
{
  print(paste(Sys.time(),"Start procesing tick data from",fname))
  tickDT<-fread(fname, sep=",", stringsAsFactors = FALSE)
  dtFormat<-"%Y-%m-%d %H:%M:%OS"
  if(ncol(tickDT)==4)
    setnames(tickDT,c("datetime", "price", "volume","buysell"))
  if(ncol(tickDT)==5)
    setnames(tickDT,c("datetime", "price", "volume","buysell","dealid"))
  
  tickDT[,buysell:=ifelse(buysell==TRUE,"buy","sell")]
  tickDT[,datetime:=as.POSIXct(strptime(datetime,dtFormat))]
  print(paste(Sys.time(),"Found",tickDT[,.N]," ticks within ",tickDT[1,datetime],"-",tickDT[.N,datetime]))
  
}

obDT<-rbindlist(lapply(grep("bid",fileList,value=T),FUN=makeBidAskData))

obDT<-obDT[datetime>=as.POSIXct(paste(as.Date(obDT$datetime[1])+1,"10:00:00.000"))]
setkey(obDT,datetime)

tickDT<-rbindlist(lapply(grep("tick",fileList,value=T),FUN=makeTickData))
tickDT<-tickDT[datetime>=as.POSIXct(paste(as.Date(tickDT$datetime[1])+1,"10:00:00.000"))]

setkey(tickDT,datetime)



save(tickDT,obDT, file=paste(symb, as.Date(tickDT$datetime[1]),".RData", sep=""))

print(paste(Sys.time(),"Total found",obDT[,.N]," bidasks within ",obDT[1,datetime],"-",obDT[.N,datetime]))
print(paste(Sys.time(),"Total found",tickDT[,.N]," ticks within ",tickDT[1,datetime],"-",tickDT[.N,datetime]))

