#' Function for calculation Market parametrs with orderbook intensity signal 

library(markovchain)
library(data.table)
source('orderbookOU.R', echo=FALSE)
getMarketParams<-function(fname, 
                          #Time frame
                          TFrame=10, 
                          #Time step
                          deltat=0.5,
                          #Open position frame
                          MY=10,
                          #Open position step
                          deltaY=1, 
                          #Disbalance frame
                          MF=10, 
                          # Disbalance step
                          deltaF=0.1, 
                          # Price min step
                          deltaTick=1,
                          #Commision
                          eps=0.5,
                          # Invenory penalization (Risk)
                          gamma=2,
                          # Max market order size in lot
                          dzetamax=10,
                          #Spread Max
                          SMax=5, 
                          # Orderbook max depth
                          levelF=2, 
                          
                          deltaN=10,
                          NFrame=20,
                          byT=FALSE){
# Load Dataset
  load(fname) 

    
# Clean and Filter Data
    dfdate<-format(obDT[.N,datetime], "%Y-%m-%d")
    downlimit<-as.POSIXct(paste(dfdate,"10:05:00.000"))
    uplimit<-as.POSIXct(paste(dfdate,"18:40:00.000"))

# Make sum of  volumes on bid  and ask sides with some depth
    obDT[,bidCum:=rowSums(.SD),.SDcols=paste("bidvolume",1:levelF,sep="")]
    obDT[,askCum:=rowSums(.SD),.SDcols=paste("askvolume",1:levelF,sep="")]
    
        
# Make intensity signal
    ddt<-0.01
    timedepth<-1/ddt
    timeDT<-data.table(datetime=seq(downlimit, uplimit, ddt))
    
    setkey(timeDT, datetime)
    setkey(obDT, datetime)
    obtDT<-obDT[timeDT,roll =T, mult="last"]

    obtDT[, paste("lagdbidvol",1:timedepth,sep=""):=lapply(1:timedepth, FUN=function(i)shift(abs(bidCum-shift(bidCum, n=1, type="lag")>0), 
                                                                                              n=i, type="lag"))]
    obtDT[, intbid1:=rowSums(.SD),.SDcols=paste("lagdbidvol",1:timedepth,sep="")]
    obtDT[,paste("lagdbidvol",1:timedepth,sep=""):=NULL]

    
    obtDT[, paste("lagdaskvol",1:timedepth,sep=""):=lapply(1:timedepth, FUN=function(i)shift(abs(askCum-shift(askCum, n=1, type="lag")>0), 
                                                                                              n=i, type="lag"))]
    obtDT[, intask1:=rowSums(.SD),.SDcols=paste("lagdaskvol",1:timedepth,sep="")]
    obtDT[,paste("lagdaskvol",1:timedepth,sep=""):=NULL]
    
    obtDT[, logF:=round(log(intask1)-log(intbid1),abs(floor(log10(deltaF))))]      
    #ggplot(obtDT[1E5:1.1E5])+
    #  geom_line(aes(x=datetime, y=(askprice1+bidprice1)/2))+
    #  geom_point(aes(x=datetime, y=(askprice1+bidprice1)/2, color=factor(sign(logF))))
    
    obtDT<-obtDT[, .(datetime, logF)]
    obtDT<-obtDT[!is.na(logF)]
    obtDT<-obtDT[!is.infinite(logF)]
    
    obDT<-obtDT[obDT,roll =T, mult="last"]
    
    rm(obtDT, timeDT, ddt,timeDT)
    
######################
    
    
    
    #' Spread S
    obDT[,deltaS:=round(askprice1-bidprice1, abs(floor(log10(deltaTick))))]
    #' Volume Imbalance F
    #obDT[,logF:=round(log(bidCum)-log(askCum),abs(floor(log10(deltaF))))]
    #' Fair Price
    obDT[,pricemid:=(askprice1+bidprice1)/2]
    
    #' Moving average filter
    #obDT[,pricemid:=EMA(pricemid,50)]
    #obDT[,deltaS:=EMA(deltaS,deltaN)]
    #obDT[,logF:=EMA(logF,50)]

    obDT[,deltaS:=round(deltaS, abs(floor(log10(deltaTick))))]
    #' Volume Imbalance F
    obDT[,logF:=round(logF,abs(floor(log10(deltaF))))]


    
    #Clean and Filter Data
    SMax<-SMax*deltaTick
    obDT<-obDT[deltaS<=SMax & deltaS>0,]
    obDT<-obDT[datetime>downlimit & datetime<uplimit]
    obDT<-obDT[complete.cases(obDT)]
    tickDT<-tickDT[datetime>downlimit & datetime<uplimit]
    
    MF<-ceiling(max(abs(obDT$logF)))
    obDT<-obDT[abs(logF)<=MF]

    #' Orderbook parameter Estimates:
    #' TOTAL TIME
    NN<-obDT[,.N]
    TT<-NN/deltaN  #as.numeric(difftime(obDT[.N,datetime],obDT[1,datetime], unit="secs"))
    
    #' TOTAL NUMBER OF OBSERVATIONS
    
    # Average time interval between  bid-asks times shiftvalue
    if (byT==FALSE)
    {
      deltat<-deltaN*TT/NN
      TFrame<-round(deltat*(NFrame/deltaN),2)
    }
    if (byT==TRUE)
    {
      deltaN=round(deltat*NN/TT,0)
      NFrame=deltaN*round(TFrame/deltat,0)
    }
    
    obDT[,jumpS:=shift(deltaS,deltaN,type="lead")-obDT$deltaS]
    obDT<-obDT[complete.cases(obDT)]
    #' Spread jump intensivity lambdaS
    lambdaS<-obDT[jumpS!=0,.N]/TT
    #' Spread transition matrix roS
    roS<- markovchainFit(data=obDT[jumpS!=0,deltaS])
    SMax=nrow(roS$estimate)*deltaTick
    
    #' Mean reversion parameter F alfaF
    #' Volatility paramter F sigmaF
    
    ouCoef<-ou.fit (obDT$logF,deltat)
    alfaF<-as.numeric(ouCoef["theta2"])
    sigmaF<-as.numeric(ouCoef["theta3"])
    
    #' Price jump intensivity lambdaJ1, lambdaJ2
    obDT[,pricemidJump:=shift(pricemid, deltaN,type="lead")-pricemid]
    obDT<-obDT[complete.cases(obDT)]
    
    pmJump1<-obDT[abs(pricemidJump)>=deltaTick/2 & abs(pricemidJump)<deltaTick]
    lambdaJ1<-pmJump1[,.N]/TT
    
    pmJump2<-obDT[abs(pricemidJump)>=deltaTick]
    lambdaJ2<-pmJump2[,.N]/TT
    
    #' prob. distribution parameters of directions of mid-price jumps beta1, beta2
    psi1<-data.table(table(pmJump1$logF))
    names(psi1)<-c("logF", "Freq")
    psi1[,logF:=as.numeric(logF)]
    psi1[,Freq:=Freq/pmJump1[,.N]]
    psi1[,Prob:=cumsum(Freq)]
    
    
    beta1<-as.numeric(coef(glm(Prob~logF-1,data=psi1, family=quasibinomial(link = "logit")))[1])
    #beta1<-1/as.numeric(coef(fitdistr(psi1$Prob, "logistic", location=0)))
    psi1[,Fit:=1/(1+exp(-beta1*logF))]
    
    psi2<-data.table(table(pmJump2$logF))
    names(psi2)<-c("logF", "Freq")
    psi2[,logF:=as.numeric(logF)]
    psi2[,Freq:=Freq/pmJump2[,.N]]
    psi2[,Prob:=cumsum(Freq)]
    
    beta2<-as.numeric(coef(glm(Prob~logF-1,data=psi2, family=quasibinomial))[1])
    #beta2<-1/as.numeric(coef(fitdistr(psi2$Prob, "logistic", location=0)))
    psi2[,Fit:=1/(1+exp(-beta2*logF))]
    
#     
#     ggplot()+
#       geom_point(data=psi2,aes(x=logF, y=Prob),color="mediumaquamarine")+
##       geom_point(data=psi1,aes(x=logF, y=Prob),color="lightcoral")+
#       geom_line(data=psi2,aes(x=logF, y=Fit),color="lightcoral")+
#       ggtitle(paste("beta2 =",round(beta2,2), sep=" "))
     
#       
#          ggplot()+
#            geom_point(data=psi1,aes(x=logF, y=Prob),color="mediumaquamarine")+
#              geom_line(data=psi1,aes(x=logF, y=Fit),color="lightcoral")+
#              ggtitle(paste("beta1 =",round(beta1,2), sep=" "))
    
    
    # Market order jump intensivity at ask (lambdaMA) and bid size (lambdaMB)
    setkey(tickDT, datetime)
    setkey(obDT, datetime)
    tbaDT<-obDT[tickDT,roll =T, mult="last"]
    tbaDT<-tbaDT[complete.cases(tbaDT)]
    
    lambdaMA<-tbaDT[,sum(price>=askprice1)]/TT
    lambdaMB<-tbaDT[,sum(price<=bidprice1)]/TT
    
    # Limit order fill rates dzeta0, dzeta1
    h<-data.table(table(tbaDT[price<=tbaDT$bidprice1 | price>=tbaDT$askprice1,logF]))    
    names(h)<-c("logF", "Freq")
    h[,logF:=as.numeric(logF)]
    h[,Freq:=Freq/tbaDT[,.N]]
    h[,Prob:=cumsum(Freq)]
    
    dzeta<-as.numeric(coef(glm(Prob~logF, data=h, family=quasibinomial(link = "logit"))))
    
    ff<-glm(Prob~logF, data=h, family=quasibinomial(link = "logit"))
    #h$FitP<-predict(ff,type="response")
    h$Fit<-1/(1+exp(-(dzeta[1]+dzeta[2]*h$logF)))
    
#         ggplot()+
#             geom_point(data=h,aes(x=-logF, y=Prob),color="mediumaquamarine")+
#             geom_line(data=h,aes(x=-logF, y=Fit),color="lightcoral")#+
#       geom_line(data=h,aes(x=logF, y=FitP),color="lightblue")
    
    # Time Length in seconds
    # Size of time step in seconds
    TT<- seq(0,TFrame, by=round(deltat,2))
    NT<-length(TT)
    
    # Inventory grid bound in lot
    # Inventorygrid step size in lot
    YY<-seq(-MY, MY, by=deltaY)
    NY<-length(YY)
    
    # Depth imbalance grid bound
    # Depth imbalance grid step size
    FF<-seq(-MF, MF, by=deltaF)
    NF<-length(FF)
    
    # Tick size
    # Commision
    SS<-seq(deltaTick,SMax, by=deltaTick)
    NS<-length(SS)
    # Number of Monte Carlo simulation paths
    NMC<-0
    # Initial cash
    X0<-0
    # Initial inventory
    Y0<-0
    # Initial mid-price of stock
    P0<-0
    
    obMarketParam<-list(
      dfdate=dfdate,
      lambdaS=lambdaS,
      roS=roS$estimate,
      alfaF=alfaF,
      sigmaF=sigmaF,
      lambdaJ1=lambdaJ1,
      lambdaJ2=lambdaJ2,
      beta1=beta1,
      beta2=beta2,
      lambdaMA=lambdaMA,
      lambdaMB=lambdaMB,
      dzeta0=dzeta[1],
      dzeta1=dzeta[2],
      TFrame=TFrame, 
      deltat=deltat,
      deltaN=deltaN,
      NFrame=NFrame,
      TT= TT,
      NT=NT,
      MY=MY,
      deltaY=deltaY,
      YY=YY,
      NY= NY,
      MF=MF,
      deltaF=deltaF,
      FF=FF,
      NF=NF,
      deltaTick=deltaTick,
      eps=eps,
      gamma=gamma,
      dzetamax=dzetamax,
      SMax=SMax,
      SS=SS,
      NS=NS,
      NMC=NMC,
      X0=X0,
      Y0=Y0,
      P0=0
    )
}