#Orderbook Research Jiangmin Xu Algorithm

#Load libraries
library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(xts)
library(zoo)
library(sde)
library(ggplot2)
library(scales)
library(caret)
library(markovchain)
library(manipulate)
library(quantmod)
source('orderbookOU.R', echo=FALSE)
source('orderbookGetMarketParam.R', echo=FALSE)
source('orderbookBackWardInductionMy.R', echo=TRUE)
source('orderbookPlotStrategies.R', echo=FALSE)

options(digits.secs=3)

fname<-"data/"
setwd(fname)
setwd("f:/TRADE/Data/research/_landy/")


fname<-c("RTS-12.152015-09-16.RData")
#"tickorderbookSI07072015.RData",
#"tickorderbookSI30062015.RData",
#          "tickorderbookSI2804.RData",
#          "tickorderbookSI2704.RData",
#          "tickorderbookSI2404.RData",
#          "tickorderbookSI2304.RData",
#          "tickorderbookSI2204.RData",
#          "tickorderbookSI2104.RData",
#          "tickorderbookSI2004.RData",
#          "tickorderbookSI1704.RData",
#          "tickorderbookSI1604.RData")

# obMarketParams<-list()
# for(i in 1:length(fname)){    
#     obMarketParams[[i]]<-getMarketParams(fname[i])
# }
# 
# #' Solve trade politics
# obMPdf<-obMarketParams[[1]]

obMPdf<-getMarketParams(fname,
                        TFrame=1, 
                        deltat=0.1,
                        MY=10,
                        deltaY=1, 
                        MF=10, 
                        # Disbalance step
                        deltaF=0.1, 
                        # Price min step
                        deltaTick=10,
                        #Commision
                        eps=2,
                        # Invenory penalization (Risk)
                        gamma=2,
                        # Max market order size in lot
                        dzetamax=10,
                        #Spread Max
                        SMax=10, 
                        # Orderbook max level
                        levelF=0)

w<-array(data=0, dim=c(obMPdf$NT,obMPdf$NY, obMPdf$NF, obMPdf$NS))
plt.thtkq<-array(data=0,dim=c(obMPdf$NT,obMPdf$NY, obMPdf$NF, obMPdf$NS))
plt.polmk<-array(dim=c(obMPdf$NT,obMPdf$NY, obMPdf$NF, obMPdf$NS))
plt.thmkb<-array(data=0,dim=c(obMPdf$NT,obMPdf$NY, obMPdf$NF, obMPdf$NS))
plt.thmka<-array(data=0,dim=c(obMPdf$NT,obMPdf$NY, obMPdf$NF, obMPdf$NS))
plt.Lmatrix<-SolveLMatrix()

SolveBackwardInduction()

gc()

polmkdf<-melt(plt.polmk)
colnames(polmkdf)<-c("t", "y", "f", "s", "pol")
thmkadf<-melt(plt.thmka)
colnames(thmkadf)<-c("t", "y", "f", "s","pingAsk")
thmkbdf<-melt(plt.thmkb)
colnames(thmkbdf)<-c("t", "y", "f", "s","pingBid")
thtkqdf<-melt(plt.thtkq)
colnames(thtkqdf)<-c("t", "y", "f", "s","qtyMO")

politics<-cbind(polmkdf, thmkadf$pingAsk,thmkbdf$pingBid,thtkqdf$qtyMO)
politics<-politics[!is.na(politics$pol),]

colnames(politics)<-c("t",  "y", "f", "s",   "PLT", "MMAQTY", "MMBQTY", "TQTY")
politics$TV<-obMPdf$TT[politics$t]
politics$YV<-obMPdf$YY[politics$y]
politics$FV<-obMPdf$FF[politics$f]
politics$SV<-obMPdf$SS[politics$s]
politicsNames<-c("MomentumSell" , "MomentumBuy" ,               
                 "InventoryControlSell"  ,      "InventoryControlBuy"  ,      
                 "PartialInventoryControlSell", "PartialInventoryControlBuy", 
                 "MarketMaking","PingingBidSide", "PingingAskSide" ,"PingingBidAskSide" )


politics<-mutate(politics, 
                 MomentumSell=(PLT==FALSE)&(TQTY+YV<0)&(TQTY<0),
                 MomentumBuy=(PLT==FALSE)&(TQTY+YV>0)&(TQTY>0),
                 InventoryControlSell=(PLT==FALSE)&(TQTY+YV==0)&(TQTY<0),
                 InventoryControlBuy=(PLT==FALSE)&(TQTY+YV==0)&(TQTY>0),
                 PartialInventoryControlSell=(PLT==FALSE)&(TQTY+YV>0)&(TQTY<0),
                 PartialInventoryControlBuy=(PLT==FALSE)&(TQTY+YV<0)&(TQTY>0),                  
                 MarketMaking=(PLT==TRUE)&(MMBQTY==0)&(MMAQTY==0),
                 PingingBidSide=(PLT==TRUE)&(MMBQTY==1)&(MMAQTY==0),
                 PingingAskSide=(PLT==TRUE)&(MMAQTY==1)&(MMBQTY==0),
                 PingingBidAskSide=(PLT==TRUE)&(MMBQTY==1)&(MMAQTY==1))

politics$Str<-as.factor( (apply(politics[,politicsNames],1,FUN=function(x)politicsNames[x])))
politics<-data.table(politics)
rm(polmkdf, thmkadf, thmkbdf, thtkqdf)

save(obMPdf,plt.polmk,plt.thtkq,plt.thmkb,plt.thmka,politics,
     file=paste("politics",obMPdf$dfdate,
                "gamma",obMPdf$gamma,
                "dzetamax", obMPdf$dzetamax,
                ".RData", sep=""))
rm(plt.polmk, plt.thmka, plt.thmkb, plt.thtkq, w)
rm(plt.Lmatrix)
write.csv(dplyr::select(politics,t, y, f, s,  PLT, MMAQTY, MMBQTY, TQTY,  TV,  YV, FV, SV),
          file=paste("politics",obMPdf$dfdate,
                     "gamma",obMPdf$gamma,
                     "dzetamax", obMPdf$dzetamax,
                     ".csv", sep=""))
obMPdf$symbol<-"SBRF-12.15_FT"
write.csv(data.frame( obMPdf$dfdate,
                      obMPdf$symbol,
                      obMPdf$lambdaS,
                      obMPdf$alfaF,
                      obMPdf$sigmaF,
                      obMPdf$lambdaJ1,
                      obMPdf$lambdaJ2,
                      obMPdf$beta1,
                      obMPdf$beta2,
                      obMPdf$lambdaMA,
                      obMPdf$lambdaMB,
                      obMPdf$dzeta0,   
                      obMPdf$dzeta1,
                      obMPdf$TFrame,
                      obMPdf$deltat,
                      obMPdf$NT,
                      obMPdf$MY,       
                      obMPdf$deltaY,
                      obMPdf$NY,
                      obMPdf$MF,
                      obMPdf$deltaF, 
                      obMPdf$NF, 
                      obMPdf$deltaTick, 
                      obMPdf$eps,
                      obMPdf$gamma,
                      obMPdf$dzetamax,
                      obMPdf$SMax,     
                      obMPdf$NS),
          file=paste("marketparams",obMPdf$dfdate,
                     ".csv", sep=""))

manipulate(PlotStrategies(t,s), 
           t=slider(1,(obMPdf$NT-1)),
           s=slider(1,obMPdf$NS))

# ANIMATION
#load(file="politics2015-07-07gamma2dzetamax2.RData")
#oopt <- animation::ani.options(interval = 0.1,
#                              convert = 'c:/PROGRA~1/ImageMagick-6.9.0-Q16/convert.exe')
#saveGIF(plotAllStrategies(1:(obMPdf$NT-1),6),
#        interval = 0.2,
#        movie.name = "orderbookS6.gif", ani.width = 800, ani.height = 400
