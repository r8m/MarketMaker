PlotStrategies<-function(tt, ss){
    StrMap <- ggplot(politics[t==tt & s==ss],
                  aes(x=YV, y=FV, fill = Str))
    StrMap <- StrMap + geom_tile()+
        xlab("Inventory Level (Lot Size)")+
        ylab("Depth Imbalance (Log)")+
        ggtitle(paste(paste(obMPdf$symbol,obMPdf$dfdate,sep=" / "),
                      paste("t=",obMPdf$TT[tt],"sec", 
                            ", S=", obMPdf$SS[ss],
                            ", Gamma=",obMPdf$gamma,
                            ", DzetaMax=",obMPdf$dzetamax, 
                            sep=""), 
                      sep="\n"))#+
    StrMap <- StrMap + theme_bw()
    print(StrMap)
    
}