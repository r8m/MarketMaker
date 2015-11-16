library(blotter)
library(rusquant)
#Portfolio and dAcconut names
userPortf<-"user_port"
userAcc<-"user_acc"

#Remove account and portfolio if run previously
if (!exists('.blotter')) .blotter <- new.env()
try(rm(list=c(paste("portfolio",userPortf,sep="."),
              paste("account",userAcc,sep=".")),
       pos=.blotter), silent =FALSE)


#download user data and trades data

#Read trades data
setwd("TradeStat/")
userData<-read.csv("trades0707b.csv",sep=",", header=FALSE, stringsAsFactors=FALSE, skip=1)
#userData<-userData[,c("V4", "V3", "V6", "V5")]
colnames(userData)<-c("V1", "V2", "V3", "V4")
userData$V1<-as.POSIXct(strptime(userData$V1,"%d/%m/%Y %H:%M"))#"%d.%m.%Y %H:%M"))#


#Processing data and declare symbol
#userSymbol<-"SiZ4"
userSymbols<-levels(factor(userData$V2))

#Load historical data for the symbol
#symbol<-"SiZ4 (12.2014)"
#tickers <- loadStockListMfd()
#symbols<-unlist(sapply(paste(userSymbols, " ", sep=""), 
#                       searchSymbol, USE.NAMES=FALSE))
symbols<-"SiU5 (09.2015)"

from<-as.Date(userData[1,1])
to<-as.Date(userData[nrow(userData),1])
period="1min"

for(s in symbols)
    getSymbols(s, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)

#Initialize stocks
cur<-currency('RUR')
#symbol<-toupper(symbol)
symbols<-toupper(symbols)
symbols.df<-data.frame(symbols, userSymbols)
symbol<-symbols[1]

#stock(symbol,currency="RUB",multiplier=1)
stock(symbols,currency=cur,multiplier=1)


# Initialize the Portfolio
initDate<-"2010-01-14"
initEq<-100000
#initPortf(userPortf,symbols=symbol,initDate=initDate)
initPortf(userPortf,symbols=symbols,initDate=initDate, currency=cur)

initAcct(userAcc,portfolios=userPortf,initDate=initDate, initEq=initEq,currency=cur)

# look at the transactions data
#symbol.trades

# Add the transactions to the portfolio
for(s in symbols){
    us<-as.character(symbols.df[symbols.df[,1]==s,2])
    symbol.trades<-userData[userData$V2==us,]
    symbol.trades<-xts(cbind(symbol.trades$V4,symbol.trades$V3),
                        order.by=(symbol.trades[,1]))
    colnames(symbol.trades)<-c("TxnPrice","TxnQty")
    for(txni in 1:nrow(symbol.trades))
        blotter:::addTxn(userPortf,s,
                      TxnDate=index(symbol.trades[txni,1]),
                      TxnQty=as.numeric(symbol.trades[txni,2]),
                      TxnPrice=as.numeric(symbol.trades[txni,1]),
                      TxnFees=-0.5,
                      #TxnData=symbol.trades,
                      verbose=FALSE)
    
}

# update the portfolio stats
updatePortf(userPortf,Symbols=symbols)

# update the account P&L
updateAcct(userAcc)

# and look at it
portfolio = getPortfolio(userPortf)
account = getAccount(userAcc)


##################### CONTAINERS CALLED IN TESTING #####################
rets  = PortfReturns(userAcc)                                     #########
tStats = tradeStats(userPortf, inclZeroDays=TRUE)
dStats = dailyStats(userPortf)                                  #########
########################################################################


############################# RESULTS ################################
tStats[,4:ncol(tStats)] <- round(tStats[,4:ncol(tStats)], 2)
#Trades Statistics
print(data.frame(t(tStats[,-c(1)])))
#write.csv(data.frame(t(tStats[,-c(1,2)])),paste(fast, slow, sd,from, to,".csv", sep="_"))


#Daily Statistics
print(t(dStats))

# FORMAT THEME
theme<-chart_theme()
theme$col$up.col<-'#81F7BE'
theme$col$up.border<-'#81F7BE'
theme$col$dn.col<-'#FAAC58'
theme$col$dn.border<-'#FAAC58'


# Absolute Cumulutive P/L
chart.Posn(userPortf, symbol,
           theme=theme,
           Dates=paste(min(userData$V1[1],userData$V1[nrow(userData)]),
                       max(userData$V1[1],userData$V1[nrow(userData)]), sep='/')
           )

# Maximum Adverse Execursion
chart.ME(Portfolio=userPortf,
         Symbol=symbol,
         type='MAE',
         scale='cash')

# Maximum Favourable Execursion
chart.ME(Portfolio=userPortf,
         Symbol=symbol,
         type='MFE',
         scale='cash')

# Returns
charts.PerformanceSummary(PortfReturns(userAcc))





