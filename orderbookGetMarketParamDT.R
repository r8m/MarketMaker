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
                          shiftvalue=5){
  # Load Dataset
  load(fname) 
  #if(!is.numeric(df$price))
  #  for(i in 3 :ncol(df))
  #    df[,i]<-as.numeric(levels(df[,i]))[df[,i]]
    
    
    #Discrete data
    #dfts<-xts(x=dplyr::select(df,-datetime,-Symbol, -buysell,-brokerDateTime),order.by=df$datetime, unique=FALSE)
    #dfts<-xts(x=dplyr::select(df,-datetime,-buysell),order.by=df$datetime, unique=FALSE)
    #dfts<-dfts[!is.na(index(dfts))]
    #dfts<-make.index.unique(dfts,deltat/100, drop=TRUE)
    
    #df<-data.frame(datetime=as.POSIXct(index(dfts)),as.data.frame(dfts, stringsAsFactors=FALSE), 
    #               stringsAsFactors=FALSE)
    
    
    # Clean and Filter Data
    dfdate<-format(df$datetime[1], "%Y-%m-%d")
    downlimit<-as.POSIXct(paste(dfdate,"10:00:00.000"))
    uplimit<-as.POSIXct(paste(dfdate,"18:45:00.000"))
    
    
    df[,bidCum:=rowSums(.SD),.SDcols=paste("bidvolume",0:levelF,sep="")]
    df[,askCum:=rowSums(.SD),.SDcols=paste("askvolume",0:levelF,sep="")]
    
    #' Spread S
    df[,deltaS:=round(askprice0-bidprice0, abs(floor(log10(deltaTick))))]
    #' Volume Imbalance F
    df[,logF:=round(log(bidCum)-log(askCum),abs(floor(log10(deltaF))))]
    #' Fair Price
    df[,pricemid:=(askprice0+bidprice0)/2]
    
    
    #Clean and Filter Data
    SMax<-SMax*deltaTick
    df<-df[deltaS<=SMax & deltaS>0,]
    df<-df[datetime>downlimit & datetime<uplimit]
    MF<-ceiling(max(abs(df$logF)))
    df<-df[abs(logF)<=MF]

    #' Orderbook parameter Estimates:
    #' TOTAL TIME
    TT<-as.numeric(difftime(df[.N,datetime],df[1,datetime], unit="secs"))
    
    df[,jumpS:=shift(deltaS,shiftvalue)-df$deltaS]
    #' Spread jump intensivity lambdaS
    lambdaS<-df[jumpS!=0,.N]/TT
    #' Spread transition matrix roS
    roS<- markovchainFit(data=df[jumpS!=0,deltaS])
    SMax=nrow(roS$estimate)*deltaTick
    
    #' Mean reversion parameter F alfaF
    #' Volatility paramter F sigmaF
    dtOU<-as.numeric(df[,mean(difftime(
      shift(datetime,shiftvalue,type="lead"),datetime, unit="secs"),na.rm=TRUE)])
    ouCoef<-ou.fit (df$logF,dtOU)
    alfaF<-as.numeric(ouCoef["theta2"])
    sigmaF<-as.numeric(ouCoef["theta3"])
    
    #' Price jump intensivity lambdaJ1, lambdaJ2
    df[,pricemidJump:=shift(pricemid, shiftvalue,type="lead")-pricemid]
    
    pmJump1<-df[abs(pricemidJump)>=deltaTick/2 & abs(pricemidJump)<deltaTick]
    lambdaJ1<-pmJump1[,.N]/TT
    
    pmJump2<-df[abs(pricemidJump)>=deltaTick]
    lambdaJ2<-pmJump2[,.N]/TT
    
    #' prob. distribution parameters of directions of mid-price jumps beta1, beta2
    psi1<-data.table(table(pmJump1$logF))
    names(psi1)<-c("logF", "Freq")
    psi1[,logF:=as.numeric(logF)]
    psi1[,Freq:=Freq/pmJump1[,.N]]
    psi1[,Prob:=cumsum(Freq)]
    
    beta1<-as.numeric(coef(glm(Prob~logF-1,data=psi1, family=quasibinomial))[1])
    psi1[,Fit:=1/(1+exp(-beta1*logF))]
    
    psi2<-data.table(table(pmJump2$logF))
    names(psi2)<-c("logF", "Freq")
    psi2[,logF:=as.numeric(logF)]
    psi2[,Freq:=Freq/pmJump2[,.N]]
    psi2[,Prob:=cumsum(Freq)]
    
    beta2<-as.numeric(coef(glm(Prob~logF-1,data=psi2, family=quasibinomial))[1])
    psi2[,Fit:=1/(1+exp(-beta2*logF))]
    
#     
#     ggplot()+
#       geom_point(data=psi2,aes(x=logF, y=Prob),color="mediumaquamarine")+
#       geom_line(data=psi2,aes(x=logF, y=Fit),color="lightcoral")+
#       ggtitle(paste("beta2 =",round(beta2,2), sep=" "))
#     
#       
#          ggplot()+
#            geom_point(data=psi1,aes(x=logF, y=Prob),color="mediumaquamarine")+
#              geom_line(data=psi1,aes(x=logF, y=Fit),color="lightcoral")+
#              ggtitle(paste("beta1 =",round(beta1,2), sep=" "))
    
    
    # Market order jump intensivity at ask (lambdaMA) and bid size (lambdaMB)
    
    lambdaMA<-df[,sum(price>=askprice0)]/TT
    lambdaMB<-df[,sum(price<=bidprice0)]/TT
    
    # Limit order fill rates dzeta0, dzeta1
    h<-data.table(table(df[price<=df$bidprice0 | price>=df$askprice0,logF]))    
    names(h)<-c("logF", "Freq")
    h[,logF:=as.numeric(logF)]
    h[,Freq:=Freq/df[,.N]]
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
    TT<- seq(0,TFrame, by=deltat)
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
    NMC<-10000
    # Initial cash
    X0<-0
    # Initial inventory
    Y0<-0
    # Initial mid-price of stock
    P0<-52000
    
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
      P0=P0
    )
}