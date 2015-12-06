require(zoo)    #na.trim
require(TTR)    #ROC
require(quantmod)

#read MSFT and get ROC
load("MSFT.Rda")
MSFT.rets<-na.trim(ROC(MSFT$Close,type="discrete",n=1))

#read Citi and get ROC
load("Citi.Rda")
Citi.rets<-na.trim(ROC(Citi$Close,type="discrete",n=1))

#read IBM and get ROC
load("IBM.Rda")
IBM.rets<-na.trim(ROC(IBM$Close,type="discrete",n=1))

op<-par(mfrow=c(3,3),mar=c(2,4,3,2)+.1)

plot.ts(MSFT$Date[-1],MSFT.rets,main="MSFT",xlab="time",ylab="MSFT ROC",type="l")
acf(MSFT.rets,main="")
pacf(MSFT.rets,main="")

plot.ts(Citi$Date[-1],Citi.rets,main="Citi",xlab="time",ylab="Citi ROC",type="l")
acf(Citi.rets,main="")
pacf(Citi.rets,main="")

plot.ts(IBM$Date[-1],IBM.rets,main="IBM",xlab="time",ylab="IBM ROC",type="l")
acf(IBM.rets,main="")
pacf(IBM.rets,main="")

par(op)
