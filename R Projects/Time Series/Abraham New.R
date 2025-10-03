library(TSA)
library(forecast)
library(readxl)
Cass <- read_excel("C:/Users/DELL/Desktop/Working Papers/Akpan/Cassava.xlsx")

abraham.cass<- ts(Cass$x,start = c(1961),end = c(2022), frequency = 1 )
ltcass <- log(tcass)

AICM=NULL
for(d in 1:4) 
{predator.tar=tar(y=log(abraham.cass),p1=8,p2=8,d=d,a=.1,b=.9)
AICM=rbind(AICM, c(d,predator.tar$AIC,signif(predator.tar$thd,4), predator.tar$p1,predator.tar$p2))}

colnames(AICM)=c('d','nominal AIC','r','p1','p2')
rownames(AICM)=NULL


predator.tar.1=tar(y=log(abraham.cass),p1=4,p2=4,d=2,a=.1,b=.9, print=T)
tar(y=log(abraham.cass),p1=1,p2=4,d=3,a=.1,b=.9,print=T, method='CLS') # re-do the estimation using the CLS method
tar(y=log(abraham.cass),p1=4,p2=4,d=3,a=.1,b=.9,print=T, method='CLS') # the CLS method does not estimate the AR order
tsdiag(predator.tar.1)

set.seed(2357125)
dcass <- diff(abraham.cass)
predator.tar.2=tar(y=(abraham.cass),p1=4,p2=2,d=2,a=.1,b=.9, print=T)
yy.1.1=tar.sim(predator.tar.2,n=11)$y
yy.1.1

pred.cass=predict(predator.tar.2,n.ahead=15)
pred.cass
pf <- forecast(pred.cass$fit,h=11)
accuracy(pf)
summary(pf)

yy=ts(c((abraham.cass),pred.cass$fit),frequency=1,start=1)
plot(yy,type='n',ylim=range(c(yy,pred.cass$pred.interval)),ylab='Log abraham.cass',
     xlab=expression(t))
lines(log(abraham.cass))
lines(window(yy, start=(abraham.cass)[1]+1),lty=2)
lines(ts(pred.cass$pred.interval[2,],start=end(abraham.cass)[1]+1),lty=2)
lines(ts(pred.cass$pred.interval[1,],start=end(abraham.cass)[1]+1),lty=2)

yy.1=tar.sim(predator.tar.2,n=1000)$y
yy.1
spec.1.4=spec(yy.1,taper=.1, span=c(200,200),plot=F)

tf <- tempfile(fileext = ".csv")
predator.tar.1=tar(y=(abraham.cass[1:42,]),p1=1,p2=4,d=3,a=.1,b=.9,print=T, method='CLS')
pred.predator=predict(predator.tar.1,n.ahead=20,n.sim=10000)
write.csv(exp(pred.predator$fit), file=tf)
write.csv(forecast(auto.arima(abraham.cass[1:42,]), h=20), file="xxx2.csv") 
write.csv(abraham.cass[43:62,1], file="xxx3.csv")
write
