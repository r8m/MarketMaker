#Orderbook Analysis
library(dplyr)
library(tidyr)
library(xts)
library(ggplot2)
library(scales)
options(digits.secs=3)

# Read Data from File and conver to XTS
ReadTickAndOrderBookData<-function(fname,dtFormat="%d/%m/%Y %H:%M:%OS", delay=2.5){
    rawData<-read.csv(fname, header=FALSE,stringsAsFactors=FALSE)
    tickcolnames<-c("datetime", 
                    "datetimebroker",
                    "symbol",
                    "price",
                    "volume",
                    "buysell")
    orderbookcolnames<-c("bidprice",
                            "bidvolume", 
                            "askprice", 
                            "askvolume")
    
    orderbookdepth<-(ncol(rawData)-length(tickcolnames))/(length(orderbookcolnames)+1)
    rawData<-rawData[,c(rep(TRUE, each=length(tickcolnames)),rep(c(FALSE, rep(TRUE, length(orderbookcolnames))), orderbookdepth))]

    names(rawData)<-c(tickcolnames,paste(orderbookcolnames,rep(0:(orderbookdepth-1), each=length(orderbookcolnames)), sep=""))
    

    rawData<-xts(x=rawData[,c(-1,-3)],
                  order.by=as.POSIXct(strptime(rawData$datetime,dtFormat))+delay)
    rawData
    
}

# Make miliseconds for brokertime
getMilliSecTime<-function(btime){
    btdf<-filter(df,datetimebroker==btime)
    dtF<-btdf$datetime[1]
    dtL<-btdf$datetime[nrow(btdf)]
    btime+as.numeric(
               difftime(btdf$datetime,dtF, units="secs"))/as.numeric(
                   difftime(dtL, dtF, units="secs"))
    
}



fname<-"g:/TRADE/Data/research/"
fnamedate<-"24062015"
setwd(paste(fname, fnamedate,"/",sep=""))
filescount<-length(dir())-1
# Read all files with tick and orderbook data data
datats<-xts()
fname<-"TicksAndOrderBook-Si-9.15_FT-"
for(i in 0:filescount){
    rawData<-ReadTickAndOrderBookData(fname=paste(fname,i,".log", sep=""),
                                      dtFormat="%d/%m/%Y %H:%M:%OS",
                                      delay=0)
    if(i==0)
        datats<-rawData
    else
        datats<-rbind(datats,rawData)
    rm(rawData)
}
gc()

# Prepare data frame
df<-data.frame(datetime=as.POSIXct(index(datats)),as.data.frame(datats), 
               stringsAsFactors=FALSE)
# Convert numeric data to numeric
for(i in 3 :ncol(df))
    df[,i]<-as.numeric(levels(df[,i]))[df[,i]]

df$datetimebroker<-as.POSIXct(strptime(df$datetimebroker, "%d/%m/%Y %H:%M:%OS"))

df$goodtime<-as.POSIXct(NA)
tfdf<-as.data.frame(table(df$datetimebroker))

for(i in 1:nrow(tfdf)){
    ti<-as.POSIXct(tfdf[i,1])
    #print(tfdf[i,])
    df[df$datetimebroker==ti,"goodtime"]<-data.frame(goodtime=getMilliSecTime(ti))
           
}
    
df$datetime<-df$goodtime
df<-dplyr::select(df,-goodtime,-datetimebroker)
#df$timediff<-as.numeric(difftime(df$datetimebroker,df$goodtime, units="secs"))
#qplot(y=timediff, data=df)

#df<-df[-1,]
save(df, file=paste("tickorderbookSI",fnamedate,".RData", sep=""))

#load("tickorderbookSI30062015.RData")

rm(datats)
rm(df,tfdf)


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
setwd("f:/TRADE/Data/research/_landy/")
fname<-"RTS-12.15.csv"

tbaDT<-fread(fname,sep=";",stringsAsFactors=FALSE)
colnames(tbaDT)<-tolower(colnames(tbaDT))
dtFormat<-"%d.%m.%Y %H:%M:%OS"
tbaDT[,"datetime":=as.POSIXct(strptime(datetime,dtFormat))]
tba
setnames(tbaDT,"sign", "buysell")

df<-data.frame(tbaDT)
save(df, file=paste("RTS-12.15", as.Date(tbaDT$datetime[1]),".RData", sep=""))
