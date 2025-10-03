##Plots for the pdf of the Inverse Remkan distribution
par(mfrow=c(2,2))
y<-seq(0,100)
a1= 50.5
b1= 14.0
p1= ((a1^2/(a1+2*b1+6))*((y^3+b1*a1*y+a1^2)/y^5)*exp(-a1*y^-1))
a2= 60.4
b2= 3.7
p2= ((a2^2/(a2+2*b2+6))*((y^3+b2*a2*y+a2^2)/y^5)*exp(-a2*y^-1))
a3= 65.6
b3= 26.5
p3= ((a3^2/(a3+2*b3+6))*((y^3+b3*a3*y+a3^2)/y^5)*exp(-a3*y^-1))
a4= 130.9
b4= 57.8
p4= ((a4^2/(a4+2*b4+6))*((y^3+b4*a4*y+a4^2)/y^5)*exp(-a4*y^-1))
a5= 120.3
b5= 45.0
p5= ((a5^2/(a5+2*b5+6))*((y^3+b5*a5*y+a5^2)/y^5)*exp(-a5*y^-1))
plot(y,p1,type="l",col="red",ylab="g(y,"~eta~","~phi~")",pch="*")
lines(y,p3,type="l",col="blue")
lines(y,p5,type="l",col="green")
lines(y,p4,type="l",col="black")
lines(y,p2,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==50.5,","~phi=="14.0")),expression(paste(eta==60.4,","~phi=="3.7")),expression(paste(eta==65.6,","~phi=="26.5")),expression(paste(eta==130.9,","~phi=="57.8")),expression(paste(eta==120.3,","~phi=="45.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.8)

y<-seq(0,100)
a1= 71.0
b1= 9.0
p1= ((a1^2/(a1+2*b1+6))*((y^3+b1*a1*y+a1^2)/y^5)*exp(-a1*y^-1))
a2= 100.4
b2= 1.7
p2= ((a2^2/(a2+2*b2+6))*((y^3+b2*a2*y+a2^2)/y^5)*exp(-a2*y^-1))
a3= 66.56
b3= 1.5
p3= ((a3^2/(a3+2*b3+6))*((y^3+b3*a3*y+a3^2)/y^5)*exp(-a3*y^-1))
a4= 92.7
b4= 3.0
p4= ((a4^2/(a4+2*b4+6))*((y^3+b4*a4*y+a4^2)/y^5)*exp(-a4*y^-1))
a5= 88.9
b5= 11.0
p5= ((a5^2/(a5+2*b5+6))*((y^3+b5*a5*y+a5^2)/y^5)*exp(-a5*y^-1))
plot(y,p1,type="l",col="red",ylab="g(y,"~eta~","~phi~")",pch="*")
lines(y,p5,type="l",col="blue")
lines(y,p3,type="l",col="green")
lines(y,p4,type="l",col="black")
lines(y,p2,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==71.0,","~phi=="9.0")),expression(paste(eta==100.4,","~phi=="1.7")),expression(paste(eta==66.56,","~phi=="1.5")),expression(paste(eta==92.7,","~phi=="3.0")),expression(paste(eta==88.9,","~phi=="11.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.8)

y<-seq(0,100)
a1= 60.8
b1= 20.0
p1= ((a1^2/(a1+2*b1+6))*((y^3+b1*a1*y+a1^2)/y^5)*exp(-a1*y^-1))
a2= 70.5
b2= 31.7
p2= ((a2^2/(a2+2*b2+6))*((y^3+b2*a2*y+a2^2)/y^5)*exp(-a2*y^-1))
a3= 65.6
b3= 43.5
p3= ((a3^2/(a3+2*b3+6))*((y^3+b3*a3*y+a3^2)/y^5)*exp(-a3*y^-1))
a4= 77.0
b4= 35.0
p4= ((a4^2/(a4+2*b4+6))*((y^3+b4*a4*y+a4^2)/y^5)*exp(-a4*y^-1))
a5= 80.3
b5= 25.0
p5= ((a5^2/(a5+2*b5+6))*((y^3+b5*a5*y+a5^2)/y^5)*exp(-a5*y^-1))
plot(y,p3,type="l",col="red",ylab="g(y,"~eta~","~phi~")",pch="*")
lines(y,p5,type="l",col="blue")
lines(y,p1,type="l",col="green")
lines(y,p4,type="l",col="black")
lines(y,p2,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==60.8,","~phi=="20.0")),expression(paste(eta==70.5,","~phi=="31.7")),expression(paste(eta==65.6,","~phi=="43.5")),expression(paste(eta==77.0,","~phi=="35.0")),expression(paste(eta==80.3,","~phi=="25.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.8)

y<-seq(0,100)
a1= 59.32
b1= 0.5
p1= ((a1^2/(a1+2*b1+6))*((y^3+b1*a1*y+a1^2)/y^5)*exp(-a1*y^-1))
a2= 45.15
b2= 1.0
p2= ((a2^2/(a2+2*b2+6))*((y^3+b2*a2*y+a2^2)/y^5)*exp(-a2*y^-1))
a3= 68.66
b3= 1.8
p3= ((a3^2/(a3+2*b3+6))*((y^3+b3*a3*y+a3^2)/y^5)*exp(-a3*y^-1))
a4= 41.49
b4= 2.9
p4= ((a4^2/(a4+2*b4+6))*((y^3+b4*a4*y+a4^2)/y^5)*exp(-a4*y^-1))
a5= 30.45
b5= 5.1
p5= ((a5^2/(a5+2*b5+6))*((y^3+b5*a5*y+a5^2)/y^5)*exp(-a5*y^-1))
plot(y,p5,type="l",col="red",ylab="g(y,"~eta~","~phi~")",pch="*")
lines(y,p3,type="l",col="blue")
lines(y,p2,type="l",col="green")
lines(y,p4,type="l",col="black")
lines(y,p1,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==59.32,","~phi=="0.5")),expression(paste(eta==45.15,","~phi=="1.0")),expression(paste(eta==68.66,","~phi=="1.8")),expression(paste(eta==41.49,","~phi=="2.9")),expression(paste(eta==30.45,","~phi=="5.1"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.8)

####################################################################################
##Plots for the cdf of the New Inverse Remkan distribution
par(mfrow=c(2,2))
y<-seq(0,150)
a1= 50.5
b1= 160.4
q1= (1+(a1^3*b1+3*b1*a1^2*y+6*a1*b1*y^2)/(6*y^3*(b1+a1)))*exp(-a1*y^-1)
a2= 60.5
b2= 150.7
q2= (1+(a2^3*b2+3*b2*a2^2*y+6*a2*b2*y^2)/(6*y^3*(b2+a2)))*exp(-a2*y^-1)
a3= 70.5
b3= 120.1
q3= (1+(a3^3*b3+3*b3*a3^2*y+6*a3*b3*y^2)/(6*y^3*(b3+a3)))*exp(-a3*y^-1)
a4= 80.6
b4= 110.2
q4= (1+(a4^3*b4+3*b4*a4^2*y+6*a4*b4*y^2)/(6*y^3*(b4+a4)))*exp(-a4*y^-1)
a5= 120.2
b5= 140.0
q5= (1+(a5^3*b5+3*b5*a5^2*y+6*a5*b5*y^2)/(6*y^3*(b5+a5)))*exp(-a5*y^-1)
plot(y,q2,type="l",col="red",ylab="G(y,"~eta~","~phi~")",pch="*")
lines(y,q1,type="l",col="blue")
lines(y,q3,type="l",col="green")
lines(y,q5,type="l",col="black")
lines(y,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==50.5,","~phi=="160.4")),expression(paste(eta==60.5,","~phi=="150.7")),expression(paste(eta==70.5,","~phi=="120.1")),expression(paste(eta==80.6,","~phi=="110.2")),expression(paste(eta==120.2,","~phi=="140.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


y<-seq(0,150)
a1= 60.8
b1= 170.4
q1= (1+(a1^3*b1+3*b1*a1^2*y+6*a1*b1*y^2)/(6*y^3*(b1+a1)))*exp(-a1*y^-1)
a2= 70.9
b2= 160.7
q2= (1+(a2^3*b2+3*b2*a2^2*y+6*a2*b2*y^2)/(6*y^3*(b2+a2)))*exp(-a2*y^-1)
a3= 80.3
b3= 180.1
q3= (1+(a3^3*b3+3*b3*a3^2*y+6*a3*b3*y^2)/(6*y^3*(b3+a3)))*exp(-a3*y^-1)
a4= 90.6
b4= 190.20
q4= (1+(a4^3*b4+3*b4*a4^2*y+6*a4*b4*y^2)/(6*y^3*(b4+a4)))*exp(-a4*y^-1)
a5= 100.2
b5= 150.0
q5= (1+(a5^3*b5+3*b5*a5^2*y+6*a5*b5*y^2)/(6*y^3*(b5+a5)))*exp(-a5*y^-1)
plot(y,q2,type="l",col="red",ylab="G(y,"~eta~","~phi~")",pch="*")
lines(y,q1,type="l",col="blue")
lines(y,q3,type="l",col="green")
lines(y,q5,type="l",col="black")
lines(y,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==60.8,","~phi=="170.4")),expression(paste(eta==70.9,","~phi=="160.7")),expression(paste(eta==80.3,","~phi=="180.1")),expression(paste(eta==90.6,","~phi=="190.20")),expression(paste(eta==100.2,","~phi=="150.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


y<-seq(0,150)
a1= 12.28
b1= 30.4
q1= (1+(a1^3*b1+3*b1*a1^2*y+6*a1*b1*y^2)/(6*y^3*(b1+a1)))*exp(-a1*y^-1)
a2= 15.45
b2= 34.5
q2= (1+(a2^3*b2+3*b2*a2^2*y+6*a2*b2*y^2)/(6*y^3*(b2+a2)))*exp(-a2*y^-1)
a3= 13.3
b3= 21.1
q3= (1+(a3^3*b3+3*b3*a3^2*y+6*a3*b3*y^2)/(6*y^3*(b3+a3)))*exp(-a3*y^-1)
a4= 26.4
b4= 42.0
q4= (1+(a4^3*b4+3*b4*a4^2*y+6*a4*b4*y^2)/(6*y^3*(b4+a4)))*exp(-a4*y^-1)
a5= 34.59
b5= 55.0
q5= (1+(a5^3*b5+3*b5*a5^2*y+6*a5*b5*y^2)/(6*y^3*(b5+a5)))*exp(-a5*y^-1)
plot(y,q3,type="l",col="red",ylab="G(y,"~eta~","~phi~")",pch="*")
lines(y,q1,type="l",col="blue")
lines(y,q5,type="l",col="green")
lines(y,q2,type="l",col="black")
lines(y,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==12.28,","~phi=="30.4")),expression(paste(eta==15.45,","~phi=="34.5")),expression(paste(eta==13.3,","~phi=="21.1")),expression(paste(eta==26.4,","~phi=="42.0")),expression(paste(eta==34.59,","~phi=="55.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


y<-seq(0,150)
a1= 0.9
b1= 0.4
q1= (1+(a1^3*b1+3*b1*a1^2*y+6*a1*b1*y^2)/(6*y^3*(b1+a1)))*exp(-a1*y^-1)
a2= 2.8
b2= 3.5
q2= (1+(a2^3*b2+3*b2*a2^2*y+6*a2*b2*y^2)/(6*y^3*(b2+a2)))*exp(-a2*y^-1)
a3= 0.73
b3= 1.3
q3= (1+(a3^3*b3+3*b3*a3^2*y+6*a3*b3*y^2)/(6*y^3*(b3+a3)))*exp(-a3*y^-1)
a4= 1.2
b4= 2.0
q4= (1+(a4^3*b4+3*b4*a4^2*y+6*a4*b4*y^2)/(6*y^3*(b4+a4)))*exp(-a4*y^-1)
a5= 3.9
b5= 5.0
q5= (1+(a5^3*b5+3*b5*a5^2*y+6*a5*b5*y^2)/(6*y^3*(b5+a5)))*exp(-a5*y^-1)
plot(y,q3,type="l",col="red",ylab="G(y,"~eta~","~phi~")",pch="*")
lines(y,q2,type="l",col="blue")
lines(y,q1,type="l",col="green")
lines(y,q5,type="l",col="black")
lines(y,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.9,","~phi=="0.4")),expression(paste(eta==2.8,","~phi=="3.5")),expression(paste(eta==0.73,","~phi=="1.3")),expression(paste(eta==1.2,","~phi=="2.0")),expression(paste(eta==3.9,","~phi=="5.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)



####################################################################################
##Plots for the Hazard Rate Function of the Inverse Remkan distribution
par(mfrow=c(2,2))
y<-seq(0,150)
a1= 60.7
b1= 600.4
d1= ((a1^2/(a1+2*b1+6))*((y^3+b1*a1*y+a1^2)/y^5)*exp(-a1*y^-1))
c1= 1-(1+(a1^3*b1+3*b1*a1^2*y+6*a1*b1*y^2)/(6*y^3*(b1+a1)))*exp(-a1*y^-1)
w1= d1/c1
a2= 50.6
b2= 500.7
d2= ((a2^2/(a2+2*b2+6))*((y^3+b2*a2*y+a2^2)/y^5)*exp(-a2*y^-1))
c2= 1-(1+(a2^3*b2+3*b2*a2^2*y+6*a2*b2*y^2)/(6*y^3*(b2+a2)))*exp(-a2*y^-1)
w2= d2/c2
a3= 40.87
b3= 400.1
d3= ((a3^2/(a3+2*b3+6))*((y^3+b3*a3*y+a3^2)/y^5)*exp(-a3*y^-1))
c3= 1-(1+(a3^3*b3+3*b3*a3^2*y+6*a3*b3*y^2)/(6*y^3*(b3+a3)))*exp(-a3*y^-1)
w3= d3/c3
a4= 70.5
b4= 700.20
d4= ((a4^2/(a4+2*b4+6))*((y^3+b4*a4*y+a4^2)/y^5)*exp(-a4*y^-1))
c4= 1-(1+(a4^3*b4+3*b4*a4^2*y+6*a4*b4*y^2)/(6*y^3*(b4+a4)))*exp(-a4*y^-1)
w4= d4/c4
a5= 80.4
b5= 300.2
d5= ((a5^2/(a5+2*b5+6))*((y^3+b5*a5*y+a5^2)/y^5)*exp(-a5*y^-1))
c5= 1-(1+(a5^3*b5+3*b5*a5^2*y+6*a5*b5*y^2)/(6*y^3*(b5+a5)))*exp(-a5*y^-1)
w5= d5/c5
plot(y,w1,type="l",col="red",ylab="h(y,"~eta~","~phi~")",pch="*")
lines(y,w3,type="l",col="blue")
lines(y,w2,type="l",col="green")
lines(y,w5,type="l",col="black")
lines(y,w4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==60.7,","~phi=="600.4")),expression(paste(eta==50.6,","~phi=="500.7")),expression(paste(eta==40.87,","~phi=="400.1")),expression(paste(eta==70.5,","~phi=="700.20")),expression(paste(eta==80.4,","~phi=="300.2"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


y<-seq(0,150)
a1= 20.85
b1= 780.4
d1= ((a1^2/(a1+2*b1+6))*((y^3+b1*a1*y+a1^2)/y^5)*exp(-a1*y^-1))
c1= 1-(1+(a1^3*b1+3*b1*a1^2*y+6*a1*b1*y^2)/(6*y^3*(b1+a1)))*exp(-a1*y^-1)
w1= d1/c1
a2= 60.91
b2= 820.4
d2= ((a2^2/(a2+2*b2+6))*((y^3+b2*a2*y+a2^2)/y^5)*exp(-a2*y^-1))
c2= 1-(1+(a2^3*b2+3*b2*a2^2*y+6*a2*b2*y^2)/(6*y^3*(b2+a2)))*exp(-a2*y^-1)
w2= d2/c2
a3= 50.83
b3= 620.6
d3= ((a3^2/(a3+2*b3+6))*((y^3+b3*a3*y+a3^2)/y^5)*exp(-a3*y^-1))
c3= 1-(1+(a3^3*b3+3*b3*a3^2*y+6*a3*b3*y^2)/(6*y^3*(b3+a3)))*exp(-a3*y^-1)
w3= d3/c3
a4= 40.57
b4= 840.30
d4= ((a4^2/(a4+2*b4+6))*((y^3+b4*a4*y+a4^2)/y^5)*exp(-a4*y^-1))
c4= 1-(1+(a4^3*b4+3*b4*a4^2*y+6*a4*b4*y^2)/(6*y^3*(b4+a4)))*exp(-a4*y^-1)
w4= d4/c4
a5= 30.64
b5= 390.2
d5= ((a5^2/(a5+2*b5+6))*((y^3+b5*a5*y+a5^2)/y^5)*exp(-a5*y^-1))
c5= 1-(1+(a5^3*b5+3*b5*a5^2*y+6*a5*b5*y^2)/(6*y^3*(b5+a5)))*exp(-a5*y^-1)
w5= d5/c5
plot(y,w1,type="l",col="red",ylab="h(y,"~eta~","~phi~")",pch="*")
lines(y,w2,type="l",col="blue")
lines(y,w5,type="l",col="green")
lines(y,w4,type="l",col="black")
lines(y,w3,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==20.85,","~phi=="780.4")),expression(paste(eta==60.91,","~phi=="820.4")),expression(paste(eta==50.83,","~phi=="620.6")),expression(paste(eta==40.57,","~phi=="840.30")),expression(paste(eta==30.64,","~phi=="390.2"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


y<-seq(0,150)
a1= 120.71
b1= 580.4
d1= ((a1^2/(a1+2*b1+6))*((y^3+b1*a1*y+a1^2)/y^5)*exp(-a1*y^-1))
c1= 1-(1+(a1^3*b1+3*b1*a1^2*y+6*a1*b1*y^2)/(6*y^3*(b1+a1)))*exp(-a1*y^-1)
w1= d1/c1
a2= 110.54
b2= 720.3
d2= ((a2^2/(a2+2*b2+6))*((y^3+b2*a2*y+a2^2)/y^5)*exp(-a2*y^-1))
c2= 1-(1+(a2^3*b2+3*b2*a2^2*y+6*a2*b2*y^2)/(6*y^3*(b2+a2)))*exp(-a2*y^-1)
w2= d2/c2
a3= 80.93
b3= 610.6
d3= ((a3^2/(a3+2*b3+6))*((y^3+b3*a3*y+a3^2)/y^5)*exp(-a3*y^-1))
c3= 1-(1+(a3^3*b3+3*b3*a3^2*y+6*a3*b3*y^2)/(6*y^3*(b3+a3)))*exp(-a3*y^-1)
w3= d3/c3
a4= 90.86
b4= 930.0
d4= ((a4^2/(a4+2*b4+6))*((y^3+b4*a4*y+a4^2)/y^5)*exp(-a4*y^-1))
c4= 1-(1+(a4^3*b4+3*b4*a4^2*y+6*a4*b4*y^2)/(6*y^3*(b4+a4)))*exp(-a4*y^-1)
w4= d4/c4
a5= 100.64
b5= 770.1
d5= ((a5^2/(a5+2*b5+6))*((y^3+b5*a5*y+a5^2)/y^5)*exp(-a5*y^-1))
c5= 1-(1+(a5^3*b5+3*b5*a5^2*y+6*a5*b5*y^2)/(6*y^3*(b5+a5)))*exp(-a5*y^-1)
w5= d5/c5
plot(y,w3,type="l",col="red",ylab="h(y,"~eta~","~phi~")",pch="*")
lines(y,w2,type="l",col="blue")
lines(y,w5,type="l",col="green")
lines(y,w4,type="l",col="black")
lines(y,w1,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==120.71,","~phi=="580.4")),expression(paste(eta==110.54,","~phi=="720.3")),expression(paste(eta==80.93,","~phi=="610.6")),expression(paste(eta==90.86,","~phi=="930.0")),expression(paste(eta==100.64,","~phi=="770.1"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)



y<-seq(0,150)
a1= 170.49
b1= 253.4
d1= ((a1^2/(a1+2*b1+6))*((y^3+b1*a1*y+a1^2)/y^5)*exp(-a1*y^-1))
c1= 1-(1+(a1^3*b1+3*b1*a1^2*y+6*a1*b1*y^2)/(6*y^3*(b1+a1)))*exp(-a1*y^-1)
w1= d1/c1
a2= 130.38
b2= 368.9
d2= ((a2^2/(a2+2*b2+6))*((y^3+b2*a2*y+a2^2)/y^5)*exp(-a2*y^-1))
c2= 1-(1+(a2^3*b2+3*b2*a2^2*y+6*a2*b2*y^2)/(6*y^3*(b2+a2)))*exp(-a2*y^-1)
w2= d2/c2
a3= 120.97
b3= 387.8
d3= ((a3^2/(a3+2*b3+6))*((y^3+b3*a3*y+a3^2)/y^5)*exp(-a3*y^-1))
c3= 1-(1+(a3^3*b3+3*b3*a3^2*y+6*a3*b3*y^2)/(6*y^3*(b3+a3)))*exp(-a3*y^-1)
w3= d3/c3
a4= 150.61
b4= 279.2
d4= ((a4^2/(a4+2*b4+6))*((y^3+b4*a4*y+a4^2)/y^5)*exp(-a4*y^-1))
c4= 1-(1+(a4^3*b4+3*b4*a4^2*y+6*a4*b4*y^2)/(6*y^3*(b4+a4)))*exp(-a4*y^-1)
w4= d4/c4
a5= 100.87
b5= 298.7
d5= ((a5^2/(a5+2*b5+6))*((y^3+b5*a5*y+a5^2)/y^5)*exp(-a5*y^-1))
c5= 1-(1+(a5^3*b5+3*b5*a5^2*y+6*a5*b5*y^2)/(6*y^3*(b5+a5)))*exp(-a5*y^-1)
w5= d5/c5
plot(y,w3,type="l",col="red",ylab="h(y,"~eta~","~phi~")",pch="*")
lines(y,w2,type="l",col="blue")
lines(y,w4,type="l",col="green")
lines(y,w1,type="l",col="black")
lines(y,w5,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==170.49,","~phi=="253.4")),expression(paste(eta==130.38,","~phi=="368.9")),expression(paste(eta==120.97,","~phi=="387.8")),expression(paste(eta==150.61,","~phi=="279.2")),expression(paste(eta==100.87,","~phi=="298.7"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)

####################################################################################
##Plots for the survival function of the New distribution
par(mfrow=c(2,2))
y<-seq(0,150)
a1= 40.7
b1= 0.4
s1= 1-(1+(a1^3*b1+3*b1*a1^2*y+6*a1*b1*y^2)/(6*y^3*(b1+a1)))*exp(-a1*y^-1)
a2= 90.6
b2= 0.7
s2= 1-(1+(a2^3*b2+3*b2*a2^2*y+6*a2*b2*y^2)/(6*y^3*(b2+a2)))*exp(-a2*y^-1)
a3= 60.7
b3= 0.1
s3= 1-(1+(a3^3*b3+3*b3*a3^2*y+6*a3*b3*y^2)/(6*y^3*(b3+a3)))*exp(-a3*y^-1)
a4= 70.5
b4= 0.20
s4= 1-(1+(a4^3*b4+3*b4*a4^2*y+6*a4*b4*y^2)/(6*y^3*(b4+a4)))*exp(-a4*y^-1)
a5= 50.4
b5= 2.2
s5= 1-(1+(a5^3*b5+3*b5*a5^2*y+6*a5*b5*y^2)/(6*y^3*(b5+a5)))*exp(-a5*y^-1)
plot(y,s2,type="l",col="red",ylab="S(y,"~eta~","~phi~")",pch="*")
lines(y,s1,type="l",col="blue")
lines(y,s3,type="l",col="green")
lines(y,s5,type="l",col="black")
lines(y,s4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.7,","~phi=="20.4")),expression(paste(eta==0.6,","~phi=="50.7")),expression(paste(eta==0.7,","~phi=="40.1")),expression(paste(eta==0.5,","~phi=="60.20")),expression(paste(eta==0.4,","~phi=="32.2"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.7)


x<-seq(0,35)
a1= 0.8
b1= 70.4
s1= (1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
a2= 0.9
b2= 60.7
s2= (1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
a3= 0.3
b3= 80.1
s3= (1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
a4= 0.6
b4= 90.20
s4= (1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x)
a5= 0.2
b5= 55.0
s5= (1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
plot(x,s2,type="l",col="red",ylab="S(y,"~eta~","~phi~")",pch="*")
lines(x,s1,type="l",col="blue")
lines(x,s3,type="l",col="green")
lines(x,s5,type="l",col="black")
lines(x,s4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.28,","~phi=="100.4")),expression(paste(eta==0.45,","~phi=="114.7")),expression(paste(eta==0.37,","~phi=="110.1")),expression(paste(eta==0.64,","~phi=="120.0")),expression(paste(eta==0.59,","~phi=="115.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,35)
a1= 0.28
b1= 100.4
s1= (1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
a2= 0.45
b2= 114.7
s2= (1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
a3= 0.37
b3= 110.1
s3= (1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
a4= 0.64
b4= 120.0
s4= (1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x)
a5= 0.59
b5= 115.0
s5= (1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
plot(x,s2,type="l",col="red",ylab="S(y,"~eta~","~phi~")",pch="*")
lines(x,s1,type="l",col="blue")
lines(x,s3,type="l",col="green")
lines(x,s5,type="l",col="black")
lines(x,s4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.8,","~phi=="70.4")),expression(paste(eta==0.9,","~phi=="60.7")),expression(paste(eta==0.3,","~phi=="80.1")),expression(paste(eta==0.6,","~phi=="90.20")),expression(paste(eta==0.2,","~phi=="55.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,35)
a1= 0.79
b1= 130.4
s1= (1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
a2= 0.84
b2= 144.5
s2= (1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
a3= 0.73
b3= 130.3
s3= (1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
a4= 0.23
b4= 150.0
s4= (1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x)
a5= 0.39
b5= 165.0
s5= (1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
plot(x,s2,type="l",col="red",ylab="S(y,"~eta~","~phi~")",pch="*")
lines(x,s1,type="l",col="blue")
lines(x,s3,type="l",col="green")
lines(x,s5,type="l",col="black")
lines(x,s4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.79,","~phi=="130.4")),expression(paste(eta==0.84,","~phi=="144.5")),expression(paste(eta==0.73,","~phi=="130.3")),expression(paste(eta==0.23,","~phi=="150.0")),expression(paste(eta==0.39,","~phi=="165.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)

####################################################################################
##Plots for the Mean Residual Life Function of the New distribution
####################################################################################
par(mfrow=c(2,2))
x<-seq(0,50)
a1= 0.07
b1= 600.4
d1= (6*a1*(a1^3*x^3*b1+6*a1^2*x^2*b1+18*a1*x*b1+24))
c1= (a1*(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1+6*a1+6*b1))
w1= d1/c1
a2= 0.06
b2= 50.7
d2= (6*a2*(a2^3*x^3*b2+6*a2^2*x^2*b2+18*a2*x*b2+24))
c2= (a2*(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2+6*a2+6*b2))
w2= d2/c2
a3= 0.08
b3= 40.1
d3= (6*a3*(a3^3*x^3*b3+6*a3^2*x^2*b3+18*a3*x*b3+24))
c3= (a3*(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3+6*a3+6*b3))
w3= d3/c3
a4= 0.2
b4= 700.20
d4= (6*a4*(a4^3*x^3*b4+6*a4^2*x^2*b4+18*a4*x*b4+24))
c4= (a4*(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4+6*a4+6*b4))
w4= d4/c4
a5= 0.1
b5= 300.2
d5= (6*a5*(a5^3*x^3*b5+6*a5^2*x^2*b5+18*a5*x*b5+24))
c5= (a5*(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5+6*a5+6*b5))
w5= d5/c5
plot(x,w3,type="l",col="red",ylab="m(x)",pch="*")
lines(x,w1,type="l",col="blue")
lines(x,w2,type="l",col="green")
lines(x,w5,type="l",col="black")
lines(x,w4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.07,","~phi=="600.4")),expression(paste(eta==0.06,","~phi=="50.7")),expression(paste(eta==0.08,","~phi=="40.1")),expression(paste(eta==0.2,","~phi=="700.20")),expression(paste(eta==0.4,","~phi=="300.2"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,50)
a1= 0.85
b1= 780.4
d1= (6*a1*(a1^3*x^3*b1+6*a1^2*x^2*b1+18*a1*x*b1+24))
c1= (a1*(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1+6*a1+6*b1))
w1= d1/c1
a2= 0.09
b2= 820.7
d2= (6*a2*(a2^3*x^3*b2+6*a2^2*x^2*b2+18*a2*x*b2+24))
c2= (a2*(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2+6*a2+6*b2))
w2= d2/c2
a3= 0.06
b3= 620.1
d3= (6*a3*(a3^3*x^3*b3+6*a3^2*x^2*b3+18*a3*x*b3+24))
c3= (a3*(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3+6*a3+6*b3))
w3= d3/c3
a4= 0.08
b4= 840.20
d4= (6*a4*(a4^3*x^3*b4+6*a4^2*x^2*b4+18*a4*x*b4+24))
c4= (a4*(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4+6*a4+6*b4))
w4= d4/c4
a5= 0.364
b5= 390.2
d5= (6*a5*(a5^3*x^3*b5+6*a5^2*x^2*b5+18*a5*x*b5+24))
c5= (a5*(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5+6*a5+6*b5))
w5= d5/c5
plot(x,w1,type="l",col="red",ylab="m(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w5,type="l",col="green")
lines(x,w4,type="l",col="black")
lines(x,w3,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.85,","~phi=="780.4")),expression(paste(eta==0.09,","~phi=="820.7")),expression(paste(eta==0.06,","~phi=="620.1")),expression(paste(eta==0.08,","~phi=="840.30")),expression(paste(eta==0.364,","~phi=="390.2"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,50)
a1= 0.71
b1= 580.4
d1= (6*a1*(a1^3*x^3*b1+6*a1^2*x^2*b1+18*a1*x*b1+24))
c1= (a1*(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1+6*a1+6*b1))
w1= d1/c1
a2= 0.54
b2= 720.3
d2= (6*a2*(a2^3*x^3*b2+6*a2^2*x^2*b2+18*a2*x*b2+24))
c2= (a2*(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2+6*a2+6*b2))
w2= d2/c2
a3= 0.93
b3= 610.6
d3= (6*a3*(a3^3*x^3*b3+6*a3^2*x^2*b3+18*a3*x*b3+24))
c3= (a3*(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3+6*a3+6*b3))
w3= d3/c3
a4= 0.1
b4= 93.0
d4= (6*a4*(a4^3*x^3*b4+6*a4^2*x^2*b4+18*a4*x*b4+24))
c4= (a4*(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4+6*a4+6*b4))
w4= d4/c4
a5= 0.36
b5= 70.1
d5= (6*a5*(a5^3*x^3*b5+6*a5^2*x^2*b5+18*a5*x*b5+24))
c5= (a5*(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5+6*a5+6*b5))
w5= d5/c5
plot(x,w1,type="l",col="red",ylab="m(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w5,type="l",col="green")
lines(x,w4,type="l",col="black")
lines(x,w3,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.71,","~phi=="580.4")),expression(paste(eta==0.54,","~phi=="720.3")),expression(paste(eta==0.93,","~phi=="610.6")),expression(paste(eta==0.1,","~phi=="93.0")),expression(paste(eta==0.36,","~phi=="70.1"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)



x<-seq(0,50)
a1= 0.49
b1= 993.8
d1= (6*a1*(a1^3*x^3*b1+6*a1^2*x^2*b1+18*a1*x*b1+24))
c1= (a1*(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1+6*a1+6*b1))
w1= d1/c1
a2= 0.38
b2= 87.9
d2= (6*a2*(a2^3*x^3*b2+6*a2^2*x^2*b2+18*a2*x*b2+24))
c2= (a2*(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2+6*a2+6*b2))
w2= d2/c2
a3= 0.27
b3= 970.3
d3= (6*a3*(a3^3*x^3*b3+6*a3^2*x^2*b3+18*a3*x*b3+24))
c3= (a3*(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3+6*a3+6*b3))
w3= d3/c3
a4= 0.1
b4= 733.7
d4= (6*a4*(a4^3*x^3*b4+6*a4^2*x^2*b4+18*a4*x*b4+24))
c4= (a4*(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4+6*a4+6*b4))
w4= d4/c4
a5= 0.14
b5= 86.7
d5= (6*a5*(a5^3*x^3*b5+6*a5^2*x^2*b5+18*a5*x*b5+24))
c5= (a5*(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5+6*a5+6*b5))
w5= d5/c5
plot(x,w3,type="l",col="red",ylab="m(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w4,type="l",col="green")
lines(x,w1,type="l",col="black")
lines(x,w5,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.49,","~phi=="993.8")),expression(paste(eta==0.38,","~phi=="87.9")),expression(paste(eta==0.27,","~phi=="970.3")),expression(paste(eta==0.1,","~phi=="733.7")),expression(paste(eta==0.14,","~phi=="86.7"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)

####################################################################################
###Mean
##########################################################
a <- 3
b <- 0.1
t <- (a^3+24*a^2*b)  
p <-(6*a^3*(a+b))
k <- t/p
k

####################################################################################
### Standard deviation
##########################################################
a <- 3
b <- 0.1
t <- (144*a^2+684*a*b+11*b^2)
p <-(36*a^2*(a+b)^2)
k <- t/p
sqrt(k)

####################################################################################
### Coefficient of variation
##########################################################
a <- 3
b <- 0.1
t <- sqrt((144*a^2+684*a*b+11*b^2))
p <-(24*a*b+a)
k <- t/p
sqrt(k)


####################################################################################
###coefficient of skewness
##########################################################
a <- 3
b <- 0.1
t <- (182*a^2+648*a^2*b+23436*a^2*b+172*b^3)  
p <-((144*a^2+684*a*b+11*b^2)*(144*a^2+684*a*b+11*b^2)^(3/2))
k <- t/p
k
values <- data.frame(k)
plot(k,type="l",col="red",ylab="S(x)",pch="*")


####################################################################################
###coefficient of Kurtosis
##########################################################
a= 0.5
b= 2
t = (4389*a^4+741312*a*b^3+789696*a^2*b^2+93312*b^4+985608*a^3*b)/(20736*a^4+196992*a^3*b+471024*a^2*b^2+15048*a*b^3+121*b^4)
t
values <- data.frame(t)
plot(t,type="l",col="red",ylab="beta",pch="*")

###############################

library(ggplot2)

ggplot(values, aes(x = k)) + 
  geom_line(color = "red") +
  labs(y = "S(x)", x = "k") +
  geom_point(shape = "*", color = "red")




##Plots for the pdf of the New distribution
par(mfrow=c(2,2))
x<-seq(0,50)
a1= 0.5
b1= 542.0
p1= ((a1^2/(b1+a1))*((6+b1*a1^2*x^3)/6)*exp(-a1*x))
a2= 0.4
b2= 543.7
p2= ((a2^2/(b2+a2))*((6+b2*a2^2*x^3)/6)*exp(-a2*x))
a3= 0.6
b3= 548.5
p3= ((a3^2/(b3+a3))*((6+b3*a3^2*x^3)/6)*exp(-a3*x))
a4= 0.9
b4= 544.0
p4= ((a4^2/(b4+a4))*((6+b4*a4^2*x^3)/6)*exp(-a4*x))
a5= 0.3
b5= 545.0
p5= ((a5^2/(b5+a5))*((6+b5*a5^2*x^3)/6)*exp(-a5*x))
plot(x,p3,type="l",col="red",ylab="g(x)",pch="*")
lines(x,p5,type="l",col="blue")
lines(x,p1,type="l",col="green")
lines(x,p4,type="l",col="black")
lines(x,p2,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==0.5,","~phi=="542.0")),expression(paste(eta==0.4,","~phi=="543.7")),expression(paste(eta==0.6,","~phi=="548.5")),expression(paste(eta==0.9,","~phi=="544.0")),expression(paste(eta==0.3,","~phi=="545.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.8)

x<-seq(0,50)
a1= 1.1
b1= 294.0
p1= ((a1^2/(b1+a1))*((6+b1*a1^2*x^3)/6)*exp(-a1*x))
a2= 0.4
b2= 291.7
p2= ((a2^2/(b2+a2))*((6+b2*a2^2*x^3)/6)*exp(-a2*x))
a3= 0.56
b3= 298.5
p3= ((a3^2/(b3+a3))*((6+b3*a3^2*x^3)/6)*exp(-a3*x))
a4= 0.7
b4= 293.0
p4= ((a4^2/(b4+a4))*((6+b4*a4^2*x^3)/6)*exp(-a4*x))
a5= 0.9
b5= 295.0
p5= ((a5^2/(b5+a5))*((6+b5*a5^2*x^3)/6)*exp(-a5*x))
plot(x,p1,type="l",col="red",ylab="g(x)",pch="*")
lines(x,p5,type="l",col="blue")
lines(x,p3,type="l",col="green")
lines(x,p4,type="l",col="black")
lines(x,p2,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==1.1,","~phi=="294.0")),expression(paste(eta==0.4,","~phi=="291.7")),expression(paste(eta==0.56,","~phi=="298.5")),expression(paste(eta==0.7,","~phi=="293.0")),expression(paste(eta==0.9,","~phi=="295.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.8)

x<-seq(0,50)
a1= 0.8
b1= 200.0
p1= ((a1^2/(b1+a1))*((6+b1*a1^2*x^3)/6)*exp(-a1*x))
a2= 0.5
b2= 11.7
p2= ((a2^2/(b2+a2))*((6+b2*a2^2*x^3)/6)*exp(-a2*x))
a3= 0.6
b3= 198.5
p3= ((a3^2/(b3+a3))*((6+b3*a3^2*x^3)/6)*exp(-a3*x))
a4= 7.0
b4= 200.0
p4= ((a4^2/(b4+a4))*((6+b4*a4^2*x^3)/6)*exp(-a4*x))
a5= 0.3
b5= 195.0
p5= ((a5^2/(b5+a5))*((6+b5*a5^2*x^3)/6)*exp(-a5*x))
plot(x,p1,type="l",col="red",ylab="g(x)",pch="*")
lines(x,p5,type="l",col="blue")
lines(x,p3,type="l",col="green")
lines(x,p4,type="l",col="black")
lines(x,p2,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==0.8,","~phi=="200.0")),expression(paste(eta==0.5,","~phi=="11.7")),expression(paste(eta==0.6,","~phi=="198.5")),expression(paste(eta==7.0,","~phi=="200.0")),expression(paste(eta==0.3,","~phi=="195.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.8)

x<-seq(0,50)
a1= 0.32
b1= 424.0
p1= ((a1^2/(b1+a1))*((6+b1*a1^2*x^3)/6)*exp(-a1*x))
a2= 0.15
b2= 131.0
p2= ((a2^2/(b2+a2))*((6+b2*a2^2*x^3)/6)*exp(-a2*x))
a3= 0.66
b3= 148.0
p3= ((a3^2/(b3+a3))*((6+b3*a3^2*x^3)/6)*exp(-a3*x))
a4= 1.49
b4= 438.0
p4= ((a4^2/(b4+a4))*((6+b4*a4^2*x^3)/6)*exp(-a4*x))
a5= 0.45
b5= 445.0
p5= ((a5^2/(b5+a5))*((6+b5*a5^2*x^3)/6)*exp(-a5*x))
plot(x,p3,type="l",col="red",ylab="g(x)",pch="*")
lines(x,p5,type="l",col="blue")
lines(x,p2,type="l",col="green")
lines(x,p4,type="l",col="black")
lines(x,p1,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==0.32,","~phi=="424.0")),expression(paste(eta==0.15,","~phi=="131.0")),expression(paste(eta==0.66,","~phi=="148.0")),expression(paste(eta==1.99,","~phi=="438.0")),expression(paste(eta==0.45,","~phi=="445.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.8)


####################################################################################
##Plots for the cdf of the New distribution
par(mfrow=c(2,2))
x<-seq(0,35)
a1= 0.7
b1= 20.4
q1= 1-(1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
a2= 0.6
b2= 50.7
q2= 1-(1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
a3= 0.7
b3= 40.1
q3= 1-(1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
a4= 0.5
b4= 60.20
q4= 1-(1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x)
a5= 0.4
b5= 32.2
q5= 1-(1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
plot(x,q2,type="l",col="red",ylab="G(x)",pch="*")
lines(x,q1,type="l",col="blue")
lines(x,q3,type="l",col="green")
lines(x,q5,type="l",col="black")
lines(x,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.7,","~phi=="20.4")),expression(paste(eta==0.6,","~phi=="50.7")),expression(paste(eta==0.7,","~phi=="40.1")),expression(paste(eta==0.5,","~phi=="60.20")),expression(paste(eta==0.4,","~phi=="32.2"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,35)
a1= 0.8
b1= 70.4
q1= 1-(1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
a2= 0.9
b2= 60.7
q2= 1-(1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
a3= 0.3
b3= 80.1
q3= 1-(1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
a4= 0.6
b4= 90.20
q4= 1-(1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x)
a5= 0.2
b5= 55.0
q5= 1-(1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
plot(x,q2,type="l",col="red",ylab="G(x)",pch="*")
lines(x,q1,type="l",col="blue")
lines(x,q3,type="l",col="green")
lines(x,q5,type="l",col="black")
lines(x,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.28,","~phi=="100.4")),expression(paste(eta==0.45,","~phi=="114.7")),expression(paste(eta==0.37,","~phi=="110.1")),expression(paste(eta==0.64,","~phi=="120.0")),expression(paste(eta==0.59,","~phi=="115.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,35)
a1= 0.28
b1= 100.4
q1= 1-(1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
a2= 0.45
b2= 114.7
q2= 1-(1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
a3= 0.37
b3= 110.1
q3= 1-(1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
a4= 0.64
b4= 120.0
q4= 1-(1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x)
a5= 0.59
b5= 115.0
q5= 1-(1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
plot(x,q2,type="l",col="red",ylab="G(x)",pch="*")
lines(x,q1,type="l",col="blue")
lines(x,q3,type="l",col="green")
lines(x,q5,type="l",col="black")
lines(x,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.8,","~phi=="70.4")),expression(paste(eta==0.9,","~phi=="60.7")),expression(paste(eta==0.3,","~phi=="80.1")),expression(paste(eta==0.6,","~phi=="90.20")),expression(paste(eta==0.2,","~phi=="55.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,35)
a1= 0.79
b1= 130.4
q1= 1-(1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
a2= 0.84
b2= 144.5
q2= 1-(1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
a3= 0.73
b3= 130.3
q3= 1-(1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
a4= 0.23
b4= 150.0
q4= 1-(1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x)
a5= 0.39
b5= 165.0
q5= 1-(1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
plot(x,q2,type="l",col="red",ylab="G(x)",pch="*")
lines(x,q1,type="l",col="blue")
lines(x,q3,type="l",col="green")
lines(x,q5,type="l",col="black")
lines(x,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.79,","~phi=="130.4")),expression(paste(eta==0.84,","~phi=="144.5")),expression(paste(eta==0.73,","~phi=="130.3")),expression(paste(eta==0.23,","~phi=="150.0")),expression(paste(eta==0.39,","~phi=="165.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)



####################################################################################
##Plots for the Hazard Rate Function of the New distribution
par(mfrow=c(2,2))
x<-seq(0,150)
a1= 0.7
b1= 600.4
d1= ((a1^2/(b1+a1))*((6+b1*a1^2*x^3)/6)*exp(-a1*x))
c1= (1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
w1= d1/c1
a2= 0.6
b2= 500.7
d2= ((a2^2/(b2+a2))*((6+b2*a2^2*x^3)/6)*exp(-a2*x))
c2= (1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
w2= d2/c2
a3= 0.87
b3= 400.1
d3= ((a3^2/(b3+a3))*((6+b3*a3^2*x^3)/6)*exp(-a3*x))
c3= (1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
w3= d3/c3
a4= 0.5
b4= 700.20
d4= ((a4^2/(b4+a4))*((6+b4*a4^2*x^3)/6)*exp(-a4*x))
c4= (1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a1*x*b4)/(6*(b4+a4)))*exp(-a4*x)
w4= d4/c4
a5= 0.4
b5= 300.2
d5= ((a5^2/(b5+a5))*((6+b5*a5^2*x^3)/6)*exp(-a5*x))
c5= (1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
w5= d5/c5
plot(x,w3,type="l",col="red",ylab="h(x)",pch="*")
lines(x,w1,type="l",col="blue")
lines(x,w2,type="l",col="green")
lines(x,w5,type="l",col="black")
lines(x,w4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.7,","~phi=="600.4")),expression(paste(eta==0.6,","~phi=="500.7")),expression(paste(eta==0.87,","~phi=="400.1")),expression(paste(eta==0.5,","~phi=="700.20")),expression(paste(eta==0.4,","~phi=="300.2"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,150)
a1= 0.85
b1= 780.4
d1= ((a1^2/(b1+a1))*((6+b1*a1^2*x^3)/6)*exp(-a1*x))
c1= (1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
w1= d1/c1
a2= 0.91
b2= 820.4
d2= ((a2^2/(b2+a2))*((6+b2*a2^2*x^3)/6)*exp(-a2*x))
c2= (1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
w2= d2/c2
a3= 0.83
b3= 620.6
d3= ((a3^2/(b3+a3))*((6+b3*a3^2*x^3)/6)*exp(-a3*x))
c3= (1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
w3= d3/c3
a4= 0.57
b4= 840.30
d4= ((a4^2/(b4+a4))*((6+b4*a4^2*x^3)/6)*exp(-a4*x))
c4= (1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a1*x*b4)/(6*(b4+a4)))*exp(-a4*x)
w4= d4/c4
a5= 0.64
b5= 390.2
d5= ((a5^2/(b5+a5))*((6+b5*a5^2*x^3)/6)*exp(-a5*x))
c5= (1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
w5= d5/c5
plot(x,w1,type="l",col="red",ylab="h(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w5,type="l",col="green")
lines(x,w4,type="l",col="black")
lines(x,w3,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.85,","~phi=="780.4")),expression(paste(eta==0.91,","~phi=="820.4")),expression(paste(eta==0.83,","~phi=="620.6")),expression(paste(eta==0.57,","~phi=="840.30")),expression(paste(eta==0.64,","~phi=="390.2"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,150)
a1= 0.71
b1= 580.4
d1= ((a1^2/(b1+a1))*((6+b1*a1^2*x^3)/6)*exp(-a1*x))
c1= (1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
w1= d1/c1
a2= 0.54
b2= 720.3
d2= ((a2^2/(b2+a2))*((6+b2*a2^2*x^3)/6)*exp(-a2*x))
c2= (1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
w2= d2/c2
a3= 0.93
b3= 610.6
d3= ((a3^2/(b3+a3))*((6+b3*a3^2*x^3)/6)*exp(-a3*x))
c3= (1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
w3= d3/c3
a4= 0.86
b4= 930.0
d4= ((a4^2/(b4+a4))*((6+b4*a4^2*x^3)/6)*exp(-a4*x))
c4= (1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a1*x*b4)/(6*(b4+a4)))*exp(-a4*x)
w4= d4/c4
a5= 0.64
b5= 770.1
d5= ((a5^2/(b5+a5))*((6+b5*a5^2*x^3)/6)*exp(-a5*x))
c5= (1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
w5= d5/c5
plot(x,w1,type="l",col="red",ylab="h(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w5,type="l",col="green")
lines(x,w4,type="l",col="black")
lines(x,w3,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.71,","~phi=="580.4")),expression(paste(eta==0.54,","~phi=="720.3")),expression(paste(eta==0.93,","~phi=="610.6")),expression(paste(eta==0.86,","~phi=="930.0")),expression(paste(eta==0.64,","~phi=="770.1"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)



x<-seq(0,150)
a1= 0.49
b1= 993.4
d1= ((a1^2/(b1+a1))*((6+b1*a1^2*x^3)/6)*exp(-a1*x))
c1= (1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
w1= d1/c1
a2= 0.38
b2= 870.9
d2= ((a2^2/(b2+a2))*((6+b2*a2^2*x^3)/6)*exp(-a2*x))
c2= (1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
w2= d2/c2
a3= 0.97
b3= 970.8
d3= ((a3^2/(b3+a3))*((6+b3*a3^2*x^3)/6)*exp(-a3*x))
c3= (1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
w3= d3/c3
a4= 0.61
b4= 793.2
d4= ((a4^2/(b4+a4))*((6+b4*a4^2*x^3)/6)*exp(-a4*x))
c4= (1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a1*x*b4)/(6*(b4+a4)))*exp(-a4*x)
w4= d4/c4
a5= 0.87
b5= 866.7
d5= ((a5^2/(b5+a5))*((6+b5*a5^2*x^3)/6)*exp(-a5*x))
c5= (1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
w5= d5/c5
plot(x,w3,type="l",col="red",ylab="h(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w4,type="l",col="green")
lines(x,w1,type="l",col="black")
lines(x,w5,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.49,","~phi=="993.4")),expression(paste(eta==0.38,","~phi=="870.9")),expression(paste(eta==0.97,","~phi=="970.8")),expression(paste(eta==0.61,","~phi=="793.2")),expression(paste(eta==0.87,","~phi=="866.7"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)

####################################################################################
##Plots for the survival function of the New distribution
par(mfrow=c(2,2))
x<-seq(0,35)
a1= 0.7
b1= 20.4
s1= (1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
a2= 0.6
b2= 50.7
s2= (1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
a3= 0.7
b3= 40.1
s3= (1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
a4= 0.5
b4= 60.20
s4= (1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x)
a5= 0.4
b5= 32.2
s5= (1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
plot(x,s2,type="l",col="red",ylab="S(x)",pch="*")
lines(x,s1,type="l",col="blue")
lines(x,s3,type="l",col="green")
lines(x,s5,type="l",col="black")
lines(x,s4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.7,","~phi=="20.4")),expression(paste(eta==0.6,","~phi=="50.7")),expression(paste(eta==0.7,","~phi=="40.1")),expression(paste(eta==0.5,","~phi=="60.20")),expression(paste(eta==0.4,","~phi=="32.2"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.7)


x<-seq(0,35)
a1= 0.8
b1= 70.4
s1= (1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
a2= 0.9
b2= 60.7
s2= (1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
a3= 0.3
b3= 80.1
s3= (1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
a4= 0.6
b4= 90.20
s4= (1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x)
a5= 0.2
b5= 55.0
s5= (1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
plot(x,s2,type="l",col="red",ylab="S(x)",pch="*")
lines(x,s1,type="l",col="blue")
lines(x,s3,type="l",col="green")
lines(x,s5,type="l",col="black")
lines(x,s4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.28,","~phi=="100.4")),expression(paste(eta==0.45,","~phi=="114.7")),expression(paste(eta==0.37,","~phi=="110.1")),expression(paste(eta==0.64,","~phi=="120.0")),expression(paste(eta==0.59,","~phi=="115.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,35)
a1= 0.28
b1= 100.4
s1= (1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
a2= 0.45
b2= 114.7
s2= (1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
a3= 0.37
b3= 110.1
s3= (1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
a4= 0.64
b4= 120.0
s4= (1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x)
a5= 0.59
b5= 115.0
s5= (1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
plot(x,s2,type="l",col="red",ylab="S(x)",pch="*")
lines(x,s1,type="l",col="blue")
lines(x,s3,type="l",col="green")
lines(x,s5,type="l",col="black")
lines(x,s4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.8,","~phi=="70.4")),expression(paste(eta==0.9,","~phi=="60.7")),expression(paste(eta==0.3,","~phi=="80.1")),expression(paste(eta==0.6,","~phi=="90.20")),expression(paste(eta==0.2,","~phi=="55.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,35)
a1= 0.79
b1= 130.4
s1= (1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x)
a2= 0.84
b2= 144.5
s2= (1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x)
a3= 0.73
b3= 130.3
s3= (1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x)
a4= 0.23
b4= 150.0
s4= (1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x)
a5= 0.39
b5= 165.0
s5= (1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x)
plot(x,s2,type="l",col="red",ylab="S(x)",pch="*")
lines(x,s1,type="l",col="blue")
lines(x,s3,type="l",col="green")
lines(x,s5,type="l",col="black")
lines(x,s4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.79,","~phi=="130.4")),expression(paste(eta==0.84,","~phi=="144.5")),expression(paste(eta==0.73,","~phi=="130.3")),expression(paste(eta==0.23,","~phi=="150.0")),expression(paste(eta==0.39,","~phi=="165.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)

####################################################################################
##Plots for the Hazard Rate Function of the New distribution
####################################################################################
par(mfrow=c(2,2))
x<-seq(0,50)
a1= 0.07
b1= 600.4
d1= (6*a1*(a1^3*x^3*b1+6*a1^2*x^2*b1+18*a1*x*b1+24))
c1= (a1*(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1+6*a1+6*b1))
w1= d1/c1
a2= 0.06
b2= 50.7
d2= (6*a2*(a2^3*x^3*b2+6*a2^2*x^2*b2+18*a2*x*b2+24))
c2= (a2*(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2+6*a2+6*b2))
w2= d2/c2
a3= 0.08
b3= 40.1
d3= (6*a3*(a3^3*x^3*b3+6*a3^2*x^2*b3+18*a3*x*b3+24))
c3= (a3*(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3+6*a3+6*b3))
w3= d3/c3
a4= 0.2
b4= 700.20
d4= (6*a4*(a4^3*x^3*b4+6*a4^2*x^2*b4+18*a4*x*b4+24))
c4= (a4*(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4+6*a4+6*b4))
w4= d4/c4
a5= 0.1
b5= 300.2
d5= (6*a5*(a5^3*x^3*b5+6*a5^2*x^2*b5+18*a5*x*b5+24))
c5= (a5*(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5+6*a5+6*b5))
w5= d5/c5
plot(x,w3,type="l",col="red",ylab="m(x)",pch="*")
lines(x,w1,type="l",col="blue")
lines(x,w2,type="l",col="green")
lines(x,w5,type="l",col="black")
lines(x,w4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.07,","~phi=="600.4")),expression(paste(eta==0.06,","~phi=="50.7")),expression(paste(eta==0.08,","~phi=="40.1")),expression(paste(eta==0.2,","~phi=="700.20")),expression(paste(eta==0.4,","~phi=="300.2"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,50)
a1= 0.85
b1= 780.4
d1= (6*a1*(a1^3*x^3*b1+6*a1^2*x^2*b1+18*a1*x*b1+24))
c1= (a1*(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1+6*a1+6*b1))
w1= d1/c1
a2= 0.09
b2= 820.7
d2= (6*a2*(a2^3*x^3*b2+6*a2^2*x^2*b2+18*a2*x*b2+24))
c2= (a2*(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2+6*a2+6*b2))
w2= d2/c2
a3= 0.06
b3= 620.1
d3= (6*a3*(a3^3*x^3*b3+6*a3^2*x^2*b3+18*a3*x*b3+24))
c3= (a3*(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3+6*a3+6*b3))
w3= d3/c3
a4= 0.08
b4= 840.20
d4= (6*a4*(a4^3*x^3*b4+6*a4^2*x^2*b4+18*a4*x*b4+24))
c4= (a4*(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4+6*a4+6*b4))
w4= d4/c4
a5= 0.364
b5= 390.2
d5= (6*a5*(a5^3*x^3*b5+6*a5^2*x^2*b5+18*a5*x*b5+24))
c5= (a5*(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5+6*a5+6*b5))
w5= d5/c5
plot(x,w1,type="l",col="red",ylab="m(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w5,type="l",col="green")
lines(x,w4,type="l",col="black")
lines(x,w3,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.85,","~phi=="780.4")),expression(paste(eta==0.09,","~phi=="820.7")),expression(paste(eta==0.06,","~phi=="620.1")),expression(paste(eta==0.08,","~phi=="840.30")),expression(paste(eta==0.364,","~phi=="390.2"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,50)
a1= 0.71
b1= 580.4
d1= (6*a1*(a1^3*x^3*b1+6*a1^2*x^2*b1+18*a1*x*b1+24))
c1= (a1*(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1+6*a1+6*b1))
w1= d1/c1
a2= 0.54
b2= 720.3
d2= (6*a2*(a2^3*x^3*b2+6*a2^2*x^2*b2+18*a2*x*b2+24))
c2= (a2*(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2+6*a2+6*b2))
w2= d2/c2
a3= 0.93
b3= 610.6
d3= (6*a3*(a3^3*x^3*b3+6*a3^2*x^2*b3+18*a3*x*b3+24))
c3= (a3*(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3+6*a3+6*b3))
w3= d3/c3
a4= 0.1
b4= 93.0
d4= (6*a4*(a4^3*x^3*b4+6*a4^2*x^2*b4+18*a4*x*b4+24))
c4= (a4*(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4+6*a4+6*b4))
w4= d4/c4
a5= 0.36
b5= 70.1
d5= (6*a5*(a5^3*x^3*b5+6*a5^2*x^2*b5+18*a5*x*b5+24))
c5= (a5*(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5+6*a5+6*b5))
w5= d5/c5
plot(x,w1,type="l",col="red",ylab="m(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w5,type="l",col="green")
lines(x,w4,type="l",col="black")
lines(x,w3,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.71,","~phi=="580.4")),expression(paste(eta==0.54,","~phi=="720.3")),expression(paste(eta==0.93,","~phi=="610.6")),expression(paste(eta==0.1,","~phi=="93.0")),expression(paste(eta==0.36,","~phi=="70.1"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)



x<-seq(0,50)
a1= 0.49
b1= 993.8
d1= (6*a1*(a1^3*x^3*b1+6*a1^2*x^2*b1+18*a1*x*b1+24))
c1= (a1*(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1+6*a1+6*b1))
w1= d1/c1
a2= 0.38
b2= 87.9
d2= (6*a2*(a2^3*x^3*b2+6*a2^2*x^2*b2+18*a2*x*b2+24))
c2= (a2*(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2+6*a2+6*b2))
w2= d2/c2
a3= 0.27
b3= 970.3
d3= (6*a3*(a3^3*x^3*b3+6*a3^2*x^2*b3+18*a3*x*b3+24))
c3= (a3*(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3+6*a3+6*b3))
w3= d3/c3
a4= 0.1
b4= 733.7
d4= (6*a4*(a4^3*x^3*b4+6*a4^2*x^2*b4+18*a4*x*b4+24))
c4= (a4*(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4+6*a4+6*b4))
w4= d4/c4
a5= 0.14
b5= 86.7
d5= (6*a5*(a5^3*x^3*b5+6*a5^2*x^2*b5+18*a5*x*b5+24))
c5= (a5*(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5+6*a5+6*b5))
w5= d5/c5
plot(x,w3,type="l",col="red",ylab="m(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w4,type="l",col="green")
lines(x,w1,type="l",col="black")
lines(x,w5,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.49,","~phi=="993.8")),expression(paste(eta==0.38,","~phi=="87.9")),expression(paste(eta==0.27,","~phi=="970.3")),expression(paste(eta==0.1,","~phi=="733.7")),expression(paste(eta==0.14,","~phi=="86.7"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)

