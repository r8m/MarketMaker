#' Orderbook Research  with Jiangmin Xu Algorithm
#' Main script
#' 
#Load libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(manipulate)
library(TTR)
setwd("~/repo/MarketMaker")
source('orderbookPrepareData.R',echo=FALSE)
source('orderbookOU.R', echo=FALSE)
source('orderbookGetMarketParam_VolumeDisbalanceSignal.R', echo=FALSE)
#source('orderbookGetMarketParam_IntensitySignal.R', echo=F)
source('orderbookBackWardInductionMy.R', echo=FALSE)
source('orderbookPlotStrategies.R', echo=FALSE)
options(digits.secs=3)

# Prepare Raw Data
startTime<-Sys.time()
symb<-"SBRF-12.16"
rawDataPath<-"~/repo/MarketMaker/Demo/RawData/"
orderBookPrepareData(rawDataPath, symb)
Sys.time()-startTime

# Make Market Parameters
setwd(rawDataPath)
fnames<-grep("MM", dir(),value = T)

obMarketParams<-list()
for(i in 1:length(fnames)){    
  print(fnames[i])   
  obMarketParams[[i]]<-getMarketParams(fnames[i],
                                          TFrame=20, 
                                          deltat=1,
                                          MY=10,
                                          deltaY=1, 
                                          MF=10, 
                                          # Disbalance step
                                          deltaF=0.05, 
                                          # Price min step
                                          deltaTick=1,
                                          #Commision
                                          eps=0.25,
                                          # Invenory penalization (Risk)
                                          gamma=0.1,
                                          # Max market order size in lot
                                          dzetamax=1,
                                          #Spread Max
                                          SMax=7, 
                                          # Orderbook max level
                                          levelF=3, 
                                          #Obseravation step
                                          deltaN = 1,
                                          #Observation control window
                                          NFrame=5,
                                          byT=FALSE)
 }


#Market Params stat
head<-c("dfdate",
        "lambdaS",
        "alfaF",
        "sigmaF",
        "lambdaJ1",
        "lambdaJ2",
        "beta1",
        "beta2",
        "lambdaMA",
        "lambdaMB",
        "dzeta0",   
        "dzeta1",
        "SMax",
        "deltat",
        "TFrame",
        "deltaN",
        "NFrame")
dtMP<-rbindlist(lapply(obMarketParams, FUN=function(x){
  data.table(x$dfdate,
             x$lambdaS,
             x$alfaF,
             x$sigmaF,
             x$lambdaJ1,
             x$lambdaJ2,
             x$beta1,
             x$beta2,
             x$lambdaMA,
             x$lambdaMB,
             x$dzeta0,   
             x$dzeta1,
             x$SMax,
             x$deltat,
             x$TFrame,
             x$deltaN,
             x$NFrame)
}))
setnames(dtMP, head)
dtMP

obMPdf<-obMarketParams[[dtMP[,.SD,by=1:nrow(dtMP)][SMax==min(SMax),nrow][1]]]
SMin<-dtMP[,min(SMax)][1]/obMPdf$deltaTick
roS<-lapply(obMarketParams,FUN=function(x)x$roS@transitionMatrix[1:SMin,1:SMin])
roS<-apply(simplify2array(roS),1:2,median)


obMPdf$lambdaS<-dtMP[,median(lambdaS)]
obMPdf$alfaF<-dtMP[,median(alfaF)]
obMPdf$sigmaF<-dtMP[,median(sigmaF)]
obMPdf$lambdaJ1<-dtMP[,median(lambdaJ1)]
obMPdf$lambdaJ2<-dtMP[,median(lambdaJ2)]
obMPdf$beta1<-dtMP[,median(beta1)]
obMPdf$beta2<-dtMP[,median(beta2)]
obMPdf$lambdaMA<-dtMP[,median(lambdaMA)]
obMPdf$lambdaMB<-dtMP[,median(lambdaMB)]
obMPdf$dzeta0<-dtMP[,median(dzeta0)]   
obMPdf$dzeta1<-dtMP[,median(dzeta1)]
obMPdf$deltat<-dtMP[,round(median(deltat),2)]
obMPdf$deltaN<-dtMP[,median(deltaN)]
obMPdf$TFrame<-dtMP[,round(median(TFrame),2)]
obMPdf$NFrame<-dtMP[,median(NFrame)]
obMPdf$TT<- seq(0,obMPdf$TFrame, by=round(obMPdf$deltat,2))
obMPdf$NT<-length(obMPdf$TT)


obMPdf$roS<-roS
Sys.time()-startTime

#' Make politics
w<-array(data=0, dim=c(obMPdf$NT,obMPdf$NY, obMPdf$NF, obMPdf$NS))
plt.thtkq<-array(data=0,dim=c(obMPdf$NT,obMPdf$NY, obMPdf$NF, obMPdf$NS))
plt.polmk<-array(dim=c(obMPdf$NT,obMPdf$NY, obMPdf$NF, obMPdf$NS))
plt.thmkb<-array(data=0,dim=c(obMPdf$NT,obMPdf$NY, obMPdf$NF, obMPdf$NS))
plt.thmka<-array(data=0,dim=c(obMPdf$NT,obMPdf$NY, obMPdf$NF, obMPdf$NS))
plt.Lmatrix<-SolveLMatrix()

startTime<-Sys.time()
SolveBackwardInduction()
Sys.time()-startTime
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


politics<-data.table(politics)
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

politics<-data.table(politics)
politics[,Str:=politicsNames[which(unlist(.SD))], .SDcols=politicsNames, by=1:nrow(politics)]

rm(polmkdf, thmkadf, thmkbdf, thtkqdf)

obMPdf$symbol<-symb
save(obMPdf,plt.polmk,plt.thtkq,plt.thmkb,plt.thmka,politics,
     file=paste("politics",symb,obMPdf$dfdate,
                "gamma",obMPdf$gamma,
                "dzetamax", obMPdf$dzetamax,
                ".RData", sep="_"))
rm(plt.polmk, plt.thmka, plt.thmkb, plt.thtkq, w)
rm(plt.Lmatrix)

# Save politics
write.csv(politics[,.(t, y, f, s,  PLT, MMAQTY, MMBQTY, TQTY,  TV,  YV, FV, SV)],
          file=paste("politics",obMPdf$symbol,obMPdf$dfdate,
                     "gamma",obMPdf$gamma,
                     "dzetamax", obMPdf$dzetamax,
                     ".csv", sep="_"))
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
                      obMPdf$NS,
                      obMPdf$deltaN,
                      obMPdf$NFrame),
          file=paste("marketparams",obMPdf$symbol,obMPdf$dfdate,
                     "gamma",obMPdf$gamma,
                     "dzetamax", obMPdf$dzetamax,
                     ".csv", sep="_"))

# PLot Politics
manipulate(PlotStrategies(t,s), 
           t=slider(1,(obMPdf$NT-1), step=1),
           s=slider(1,obMPdf$NS,step=1))

politics[,.N/politics[,.N],by=Str][order(-V1)]
# ANIMATION
library(animation)
#load(file="politics2015-07-07gamma2dzetamax2.RData")
oopt <- animation::ani.options(interval = 0.1)

saveGIF(lapply(1:(obMPdf$NT-1), FUN=function(x)PlotStrategies(x,2)),
        interval = 0.5,
        movie.name = "orderbook.gif", ani.width = 800, ani.height = 400)


# 
# 


