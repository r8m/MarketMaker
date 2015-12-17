###############################################################################
################## SEPARATE TICK AND BID_ASK SMARTCOM LOG READER###############
library(data.table)
options(digits.secs=3)

fname<-"g:/TRADE/Data/research/SBRF/20151103/"
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
tbaDT<-tbaDT[complete.cases(tbaDT),]
df<-data.frame(tbaDT[,.SD,.SDcols=c(tickHeader, bidaskHeader[-1])])
save(df, file=paste(tbaDT$Symbol[1], as.Date(tbaDT$datetime[1]),".RData", sep=""))



###################################################################################
###################### PLAZA DATA FROM _LANDY #####################################
library(data.table)
options(digits.secs=3)
setwd("f:/TRADE/Data/research/_landy/")
fname<-"SBRF-12.15_18.09.2015.csv"

tbaDT<-fread(fname,sep=";",stringsAsFactors=FALSE)
colnames(tbaDT)<-tolower(colnames(tbaDT))
dtFormat<-"%d.%m.%Y %H:%M:%OS"
tbaDT[,"datetime":=as.POSIXct(strptime(datetime,dtFormat))]
setnames(tbaDT,"sign", "buysell")

df<-tbaDT
save(df, file=paste("SBRF-12.15", as.Date(tbaDT$datetime[1]),".RData", sep=""))
