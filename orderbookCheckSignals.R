# OrderFlow : buy/Sell count ratio
load(fnames[11])

# Check disbalance signal
lapply(seq(1,100,by=10),FUN=function(x) {lm(df[, log(shift(price,n=x, type="lead"))-log(price)]~df[,log(askvolume0)-log(bidvolume0)])})



# Check orderflow ratio
VTORatio<-function(tickDT){
  nBuy=sum(tickDT=="buy")
  nSell=sum(tickDT=="sell")
  (nBuy-nSell)/(nBuy+nSell)
  
}

df[,VTO:=rollapply(buysell,width=50,FUN=VTORatio, fill=NA, align="right")]
df[, pricegap:=log(shift(price,n=10, type="lead"))-log(price)]

# Price Gap Check
data.table(lapply(seq(1,100,by=10),FUN=function(x) {
  modFit<-lm(df[, log(shift(price,n=x, type="lead"))-log(price)]~df$VTO)
  modFit$coefficients}))


# VTO window check
data.table(seq(1,20,by=1),lapply(seq(1,20,by=1),FUN=function(x) {
  modFit<-lm(df[, log(shift(price,n=10, type="lead"))-log(price)]~
               df[,rollapply(buysell,width=x,FUN=VTORatio, fill=NA, align="right")])
  modFit$coefficients}))




# BALANCE window check
data.table(seq(1,200,by=10),lapply(seq(1,200,by=10),FUN=function(x) {
  modFit<-lm(df[, log(shift(price,n=x, type="lead"))-log(price)]~
               df[,log(bidvolume0)-log(askvolume0)])
  modFit$coefficients}))


#VTO +Balance
data.table(seq(10,50,by=1),lapply(seq(10,50,by=1),FUN=function(x) {
  modFit<-lm(df[, log(shift(price,n=100, type="lead"))-log(price)]~
               df[,(log(bidvolume0)-log(askvolume0))*rollapply(buysell,width=x,FUN=VTORatio, fill=NA, align="right")])
  modFit$coefficients}))

# Check order book jumps intensivity
setwd("~/repos/MarketMaker/data/RIDATA/")
symb<-"RTS-3.16_FT"
fnames<-dir()

rbindlist(lapply(fnames,FUN=function(f){
  load(f)
  df[,.(symb,
        as.Date(datetime[1]),
        bid0Jump=sum(price<bidprice0),
        bid1Jump=sum(price<bidprice1),
        bid2Jump=sum(price<bidprice2),
        ask0Jump=sum(price>askprice0),
        ask1Jump=sum(price>askprice1),
        ask2Jump=sum(price>askprice2),
        .N)]
  
}))
