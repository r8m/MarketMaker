getMarketParams<-function(fname, 
                          TFrame=1, 
                          deltat=0.1,
                          MY=10,
                          deltaY=1, 
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
                          # Orderbook max level
                          levelF=0){
    # Load Dataset
  SMax<-SMax*deltaTick
  load(fname) 
    if(!is.numeric(df$price))
        for(i in 3 :ncol(df))
            df[,i]<-as.numeric(levels(df[,i]))[df[,i]]
    
    #Discrete data
     dfts<-xts(x=dplyr::select(df,-datetime,-Symbol, -buysell,-brokerDateTime),order.by=df$datetime, unique=FALSE)
     dfts<-dfts[!is.na(index(dfts))]
     dfts<-make.index.unique(dfts,deltat/100, drop=TRUE)
    
     df<-data.frame(datetime=as.POSIXct(index(dfts)),as.data.frame(dfts, stringsAsFactors=FALSE), 
                    stringsAsFactors=FALSE)
    
    
    # Clean and Filter Data
    dfdate<-format(df$datetime[2], "%Y-%m-%d")
    downlimit<-as.POSIXct(paste(dfdate,"10:05:00.000"))
    uplimit<-as.POSIXct(paste(dfdate,"18:00:00.000"))
    
    df$bidCum<-apply(cbind(0,df[,paste("bidvolume",0:levelF,sep="")]),1,FUN=sum)
    df$askCum<-apply(cbind(0,df[,paste("askvolume",0:levelF,sep="")]),1,FUN=sum)
    
    df<-df %>%
        mutate(
            #' Spread S
            deltaS=round(askprice0-bidprice0, abs(floor(log10(deltaTick)))),
            
            #' Volume Imbalance F
            disF=bidCum-askCum,
            #logF=round(log(bidCum)-log(askCum),abs(floor(log10(deltaF)))),
            logF=round(log(bidCum)-log(askCum),abs(floor(log10(deltaF)))),
            # Mid Price
            pricemid=(askprice0+bidprice0)/2
            
        )%>%
        filter(deltaS>0)%>%
        filter(deltaS<=SMax)%>%
        filter(abs(logF)<=MF)%>%
        #filter(abs(logF)>0)%>%
        filter(datetime>downlimit & datetime<uplimit)
    #%>%
    #filter(price>=bidprice5 & price<= askprice5)
    
    MF<-ceiling(max(abs(df$logF)))
    
    #' Orderbook parameter Estimates:
    #' TOTAL TIME
    TT<-as.numeric(difftime(last(df$datetime),first(df$datetime), unit="secs"))
    
    #' Spread jump intensivity lambdaS
    #' Spread transition matrix roS
    #df$jumpS<-lag(df$deltaS,1)-df$deltaS
    df$jumpS<-as.numeric(Next(df$deltaS,1))-df$deltaS
    
    lambdaS<-nrow(filter(df, jumpS!=0))/TT
    roS<- markovchainFit(data=filter(df, jumpS!=0)$deltaS)
    
    #' Mean reversion parameter F alfaF
    #' Volatility paramter F sigmaF
    ouCoef<-ou.fit (df$logF, deltat)
    alfaF<-as.numeric(ouCoef["theta2"])
    sigmaF<-as.numeric(ouCoef["theta3"])
    
    #' Price jump intensivity lambdaJ1, lambdaJ2
    #df$pricemidJump<-lag(df$pricemid,1)-df$pricemid
    df$pricemidJump<-as.numeric(Next(df$pricemid,1))-df$pricemid
    
    pmJump1<-filter(df, abs(pricemidJump)>=deltaTick/2 & abs(pricemidJump)<deltaTick)
    lambdaJ1<-nrow(pmJump1)/TT
    
    pmJump2<-filter(df, abs(pricemidJump)>=deltaTick)
    lambdaJ2<-nrow(pmJump2)/TT
    
    #' prob. distribution parameters of directions of mid-price jumps beta1, beta2
    psi1<-as.data.frame(table(pmJump1$logF))
    names(psi1)<-c("logF", "Freq")
    psi1$logF<-as.numeric(levels(psi1$logF))
    psi1$Freq<-psi1$Freq/nrow(pmJump1)
    psi1$Prob<-cumsum(psi1$Freq)
    
    beta1<-as.numeric(coef(glm(Prob~logF-1,data=psi1, family=quasibinomial))[1])
    psi1$Fit<-1/(1+exp(-beta1*psi1$logF))

    
    psi2<-as.data.frame(table(pmJump2$logF))
    names(psi2)<-c("logF", "Freq")
    psi2$logF<-as.numeric(levels(psi2$logF))
    psi2$Freq<-psi2$Freq/nrow(pmJump2)
    psi2$Prob<-cumsum(psi2$Freq)
    
    beta2<-as.numeric(coef(glm(Prob~logF-1,data=psi2, family=quasibinomial))[1])
    psi2$Fit<-1/(1+exp(-beta2*psi2$logF))
    
    #     ggplot()+
    #         geom_point(data=psi2,aes(x=logF, y=Prob),color="mediumaquamarine")+
    #         geom_line(data=psi2,aes(x=logF, y=Fit),color="lightcoral")+
    #         ggtitle(paste("beta2 =",round(beta2,2), sep=" "))
    
    
    # Market order jump intensivity at ask (lambdaMA) and bid size (lambdaMB)
    
    lambdaMA<-sum(df$price>=df$askprice0)/TT
    lambdaMB<-sum(df$price<=df$bidprice0)/TT

    #lambdaMA<-sum(df$price>df$pricemid)/TT
    #lambdaMB<-sum(df$price<df$pricemid)/TT
    
        
    # Limit order fill rates dzeta0, dzeta1
    h<-as.data.frame(table(filter(df, 
                                  price<=df$bidprice0 | price>=df$askprice0)$logF))    
    names(h)<-c("logF", "Freq")
    h$logF<-as.numeric(levels(h$logF))
    h$Freq<-h$Freq/nrow(df)
    h$Prob<-cumsum(h$Freq)
    
    dzeta<-as.numeric(coef(glm(Prob~logF, data=h, family=quasibinomial(link = "logit"))))
    
    ff<-glm(Prob~logF, data=h, family=quasibinomial(link = "logit"))
    #h$FitP<-predict(ff,type="response")
    h$Fit<-1/(1+exp(-(dzeta[1]+dzeta[2]*h$logF)))
    
    #     ggplot()+
    #         geom_point(data=h,aes(x=-logF, y=Prob),color="mediumaquamarine")+
    #         geom_line(data=h,aes(x=-logF, y=Fit),color="lightcoral")#+
    #   geom_line(data=h,aes(x=logF, y=FitP),color="lightblue")
    
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