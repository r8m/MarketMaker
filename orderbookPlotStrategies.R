#library(grid)
library(ggplot2)
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
#     pushViewport(viewport(layout = grid.layout(1, 2)))
#     print(StrMap,
#           vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
#     print(qplot(x=Str, color=Str,data=politics, fill=Str),
#           vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

    
}