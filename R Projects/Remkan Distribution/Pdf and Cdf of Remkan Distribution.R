##Plots for the pdf of the New Remkan distribution
par(mfrow=c(2,2))
x<-seq(0,50)
a1= 0.1
b1= 542.0
p1= ((a1^2/(2*b1+a1+6))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))
a2= 0.24
b2= 543.7
p2= ((a2^2/(2*b2+a2+6))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))
a3= 0.35
b3= 548.5
p3= ((a3^2/(2*b3+a3+6))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))
a4= 0.2
b4= 544.0
p4= ((a4^2/(2*b4+a4+6))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))
a5= 0.17
b5= 545.0
p5= ((a5^2/(2*b5+a5+6))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))
plot(x,p3,type="l",col="red",ylab="g(x)",pch="*")
lines(x,p5,type="l",col="blue")
lines(x,p1,type="l",col="green")
lines(x,p4,type="l",col="black")
lines(x,p2,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==0.1,","~phi=="542.0")),expression(paste(eta==0.24,","~phi=="543.7")),expression(paste(eta==0.35,","~phi=="548.5")),expression(paste(eta==0.2,","~phi=="544.0")),expression(paste(eta==0.17,","~phi=="545.0"))),col=c("red","blue","green","black","gold"),lty=1:1:1:1:2,cex=0.8)

x<-seq(0,50)
a1= 0.11
b1= 394.0
p1= ((a1^2/(2*b1+a1+6))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))
a2= 0.95
b2= 291.7
p2= ((a2^2/(2*b2+a2+6))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))
a3= 0.056
b3= 298.5
p3= ((a3^2/(2*b3+a3+6))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))
a4= 0.07
b4= 293.0
p4= ((a4^2/(2*b4+a4+6))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))
a5= 0.09
b5= 295.0
p5= ((a5^2/(2*b5+a5+6))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))
plot(x,p1,type="l",col="red",ylab="g(x)",pch="*")
lines(x,p5,type="l",col="blue")
lines(x,p3,type="l",col="green")
lines(x,p4,type="l",col="black")
lines(x,p2,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==0.11,","~phi=="394.0")),expression(paste(eta==0.95,","~phi=="291.7")),expression(paste(eta==0.056,","~phi=="298.5")),expression(paste(eta==0.07,","~phi=="293.0")),expression(paste(eta==0.09,","~phi=="295.0"))),col=c("red","blue","green","black","gold"),lty=1:1:1:1:2,cex=0.7)

x<-seq(0,50)
a1= 0.18
b1= 2000.0
p1= ((a1^2/(2*b1+a1+6))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))
a2= 0.5
b2= 1100.7
p2= ((a2^2/(2*b2+a2+6))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))
a3= 0.16
b3= 9800.5
p3= ((a3^2/(2*b3+a3+6))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))
a4= 1.0
b4= 2050.0
p4= ((a4^2/(2*b4+a4+6))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))
a5= 0.3
b5= 9515.0
p5= ((a5^2/(2*b5+a5+6))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))
plot(x,p4,type="l",col="red",ylab="g(x)",pch="*")
lines(x,p1,type="l",col="blue")
lines(x,p3,type="l",col="green")
lines(x,p5,type="l",col="black")
lines(x,p2,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==0.18,","~phi=="2000.0")),expression(paste(eta==0.5,","~phi=="1100.7")),expression(paste(eta==0.16,","~phi=="9800.5")),expression(paste(eta==1.0,","~phi=="2050.0")),expression(paste(eta==0.3,","~phi=="9515.0"))),col=c("red","blue","green","black","gold"),lty=1:1:1:1:2,cex=0.8)

x<-seq(0,50)
a1= 0.32
b1= 424.0
p1= ((a1^2/(2*b1+a1+6))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))
a2= 0.15
b2= 131.0
p2= ((a2^2/(2*b2+a2+6))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))
a3= 0.66
b3= 148.0
p3= ((a3^2/(2*b3+a3+6))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))
a4= 1.49
b4= 438.0
p4= ((a4^2/(2*b4+a4+6))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))
a5= 0.45
b5= 445.0
p5= ((a5^2/(2*b5+a5+6))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))
plot(x,p3,type="l",col="red",ylab="g(x)",pch="*")
lines(x,p5,type="l",col="blue")
lines(x,p2,type="l",col="green")
lines(x,p4,type="l",col="black")
lines(x,p1,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==0.32,","~phi=="424.0")),expression(paste(eta==0.15,","~phi=="131.0")),expression(paste(eta==0.66,","~phi=="148.0")),expression(paste(eta==1.49,","~phi=="438.0")),expression(paste(eta==0.45,","~phi=="445.0"))),col=c("red","blue","green","black","gold"),lty=1:1:1:1:2,cex=0.8)


####################################################################################
##Plots for the cdf of the New Remkan distribution
par(mfrow=c(2,2))
x<-seq(0,50)
a1= 1.07
b1= 200.4
q1= 1-(1+(a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1))/(a1+2*b1+6))*exp(-a1*x)
a2= 0.16
b2= 500.7
q2= 1-(1+(a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2))/(a2+2*b2+6))*exp(-a2*x)
a3= 0.7
b3= 400.1
q3= 1-(1+(a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3))/(a3+2*b3+6))*exp(-a3*x)
a4= 0.5
b4= 600.20
q4= 1-(1+(a1^4*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4))/(a4+2*b4+6))*exp(-a4*x)
a5= 0.4
b5= 320.2
q5= 1-(1+(a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5))/(a5+2*b5+6))*exp(-a5*x)
plot(x,q2,type="l",col="red",ylab="G(x)",pch="*")
lines(x,q1,type="l",col="blue")
lines(x,q3,type="l",col="green")
lines(x,q5,type="l",col="black")
lines(x,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==1.07,","~phi=="200.4")),expression(paste(eta==0.16,","~phi=="500.7")),expression(paste(eta==0.7,","~phi=="400.1")),expression(paste(eta==0.5,","~phi=="600.20")),expression(paste(eta==0.4,","~phi=="320.2"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,35)
a1= 0.8
b1= 70.4
q1= 1-(1+(a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1))/(a1+2*b1+6))*exp(-a1*x)
a2= 0.9
b2= 60.7
q2= 1-(1+(a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2))/(a2+2*b2+6))*exp(-a2*x)
a3= 0.3
b3= 80.1
q3= 1-(1+(a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3))/(a3+2*b3+6))*exp(-a3*x)
a4= 0.6
b4= 90.20
q4= 1-(1+(a1^4*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4))/(a4+2*b4+6))*exp(-a4*x)
a5= 0.2
b5= 55.0
q5= 1-(1+(a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5))/(a5+2*b5+6))*exp(-a5*x)
plot(x,q2,type="l",col="red",ylab="G(x)",pch="*")
lines(x,q1,type="l",col="blue")
lines(x,q3,type="l",col="green")
lines(x,q5,type="l",col="black")
lines(x,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.28,","~phi=="100.4")),expression(paste(eta==0.45,","~phi=="114.7")),expression(paste(eta==0.37,","~phi=="110.1")),expression(paste(eta==0.64,","~phi=="120.0")),expression(paste(eta==0.59,","~phi=="115.0"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,35)
a1= 0.28
b1= 100.4
q1= 1-(1+(a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1))/(a1+2*b1+6))*exp(-a1*x)
a2= 0.45
b2= 114.7
q2= 1-(1+(a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2))/(a2+2*b2+6))*exp(-a2*x)
a3= 0.37
b3= 110.1
q3= 1-(1+(a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3))/(a3+2*b3+6))*exp(-a3*x)
a4= 0.64
b4= 120.0
q4= 1-(1+(a1^4*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4))/(a4+2*b4+6))*exp(-a4*x)
a5= 0.59
b5= 115.0
q5= 1-(1+(a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5))/(a5+2*b5+6))*exp(-a5*x)
plot(x,q2,type="l",col="red",ylab="G(x)",pch="*")
lines(x,q1,type="l",col="blue")
lines(x,q3,type="l",col="green")
lines(x,q5,type="l",col="black")
lines(x,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.8,","~phi=="70.4")),expression(paste(eta==0.9,","~phi=="60.7")),expression(paste(eta==0.3,","~phi=="80.1")),expression(paste(eta==0.6,","~phi=="90.20")),expression(paste(eta==0.2,","~phi=="55.0"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,35)
a1= 0.79
b1= 130.4
q1= 1-(1+(a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1))/(a1+2*b1+6))*exp(-a1*x)
a2= 0.84
b2= 144.5
q2= 1-(1+(a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2))/(a2+2*b2+6))*exp(-a2*x)
a3= 0.73
b3= 130.3
q3= 1-(1+(a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3))/(a3+2*b3+6))*exp(-a3*x)
a4= 0.23
b4= 150.0
q4= 1-(1+(a1^4*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4))/(a4+2*b4+6))*exp(-a4*x)
a5= 0.39
b5= 165.0
q5= 1-(1+(a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5))/(a5+2*b5+6))*exp(-a5*x)
plot(x,q2,type="l",col="red",ylab="G(x)",pch="*")
lines(x,q1,type="l",col="blue")
lines(x,q3,type="l",col="green")
lines(x,q5,type="l",col="black")
lines(x,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.79,","~phi=="130.4")),expression(paste(eta==0.84,","~phi=="144.5")),expression(paste(eta==0.73,","~phi=="130.3")),expression(paste(eta==0.23,","~phi=="150.0")),expression(paste(eta==0.39,","~phi=="165.0"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)



####################################################################################
##Plots for the Hazard Rate Function of the New Remkan distribution
par(mfrow=c(2,2))
x<-seq(0,150)
a1= 0.7
b1= 600.4
d1= ((a1^2/(2*b1+a1+6))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))
c1= (1+(a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1))/(a1+2*b1+6))*exp(-a1*x)
w1= d1/c1
a2= 0.6
b2= 500.7
d2= ((a2^2/(2*b2+a2+6))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))
c2= (1+(a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2))/(a2+2*b2+6))*exp(-a2*x)
w2= d2/c2
a3= 0.87
b3= 400.1
d3= ((a3^2/(2*b3+a3+6))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))
c3= (1+(a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3))/(a3+2*b3+6))*exp(-a3*x)
w3= d3/c3
a4= 0.5
b4= 700.20
d4= ((a4^2/(2*b4+a4+6))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))
c4= (1+(a1^4*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4))/(a4+2*b4+6))*exp(-a4*x)
w4= d4/c4
a5= 0.4
b5= 300.2
d5= ((a5^2/(2*b5+a5+6))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))
c5= (1+(a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5))/(a5+2*b5+6))*exp(-a5*x)
w5= d5/c5
plot(x,w3,type="l",col="red",ylab="h(x)",pch="*")
lines(x,w1,type="l",col="blue")
lines(x,w2,type="l",col="green")
lines(x,w5,type="l",col="black")
lines(x,w4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.7,","~phi=="600.4")),expression(paste(eta==0.6,","~phi=="500.7")),expression(paste(eta==0.87,","~phi=="400.1")),expression(paste(eta==0.5,","~phi=="700.20")),expression(paste(eta==0.4,","~phi=="300.2"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,150)
a1= 0.85
b1= 780.4
d1= ((a1^2/(2*b1+a1+6))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))
c1= (1+(a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1))/(a1+2*b1+6))*exp(-a1*x)
w1= d1/c1
a2= 0.91
b2= 820.4
d2= ((a2^2/(2*b2+a2+6))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))
c2= (1+(a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2))/(a2+2*b2+6))*exp(-a2*x)
w2= d2/c2
a3= 0.83
b3= 620.6
d3= ((a3^2/(2*b3+a3+6))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))
c3= (1+(a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3))/(a3+2*b3+6))*exp(-a3*x)
w3= d3/c3
a4= 0.57
b4= 840.30
d4= ((a4^2/(2*b4+a4+6))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))
c4= (1+(a1^4*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4))/(a4+2*b4+6))*exp(-a4*x)
w4= d4/c4
a5= 0.64
b5= 390.2
d5= ((a5^2/(2*b5+a5+6))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))
c5= (1+(a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5))/(a5+2*b5+6))*exp(-a5*x)
w5= d5/c5
plot(x,w1,type="l",col="red",ylab="h(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w5,type="l",col="green")
lines(x,w4,type="l",col="black")
lines(x,w3,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.85,","~phi=="780.4")),expression(paste(eta==0.91,","~phi=="820.4")),expression(paste(eta==0.83,","~phi=="620.6")),expression(paste(eta==0.57,","~phi=="840.30")),expression(paste(eta==0.64,","~phi=="390.2"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,150)
a1= 0.71
b1= 580.4
d1= ((a1^2/(2*b1+a1+6))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))
c1= (1+(a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1))/(a1+2*b1+6))*exp(-a1*x)
w1= d1/c1
a2= 0.54
b2= 720.3
d2= ((a2^2/(2*b2+a2+6))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))
c2= (1+(a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2))/(a2+2*b2+6))*exp(-a2*x)
w2= d2/c2
a3= 0.93
b3= 610.6
d3= ((a3^2/(2*b3+a3+6))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))
c3= (1+(a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3))/(a3+2*b3+6))*exp(-a3*x)
w3= d3/c3
a4= 0.86
b4= 930.0
d4= ((a4^2/(2*b4+a4+6))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))
c4= (1+(a1^4*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4))/(a4+2*b4+6))*exp(-a4*x)
w4= d4/c4
a5= 0.64
b5= 770.1
d5= ((a5^2/(2*b5+a5+6))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))
c5= (1+(a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5))/(a5+2*b5+6))*exp(-a5*x)
w5= d5/c5
plot(x,w1,type="l",col="red",ylab="h(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w5,type="l",col="green")
lines(x,w4,type="l",col="black")
lines(x,w3,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.71,","~phi=="580.4")),expression(paste(eta==0.54,","~phi=="720.3")),expression(paste(eta==0.93,","~phi=="610.6")),expression(paste(eta==0.86,","~phi=="930.0")),expression(paste(eta==0.64,","~phi=="770.1"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)



x<-seq(0,150)
a1= 0.49
b1= 993.4
d1= ((a1^2/(2*b1+a1+6))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))
c1= (1+(a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1))/(a1+2*b1+6))*exp(-a1*x)
w1= d1/c1
a2= 0.38
b2= 870.9
d2= ((a2^2/(2*b2+a2+6))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))
c2= (1+(a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2))/(a2+2*b2+6))*exp(-a2*x)
w2= d2/c2
a3= 0.97
b3= 970.8
d3= ((a3^2/(2*b3+a3+6))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))
c3= (1+(a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3))/(a3+2*b3+6))*exp(-a3*x)
w3= d3/c3
a4= 0.61
b4= 793.2
d4= ((a4^2/(2*b4+a4+6))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))
c4= (1+(a1^4*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4))/(a4+2*b4+6))*exp(-a4*x)
w4= d4/c4
a5= 0.87
b5= 866.7
d5= ((a5^2/(2*b5+a5+6))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))
c5= (1+(a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5))/(a5+2*b5+6))*exp(-a5*x)
w5= d5/c5
plot(x,w3,type="l",col="red",ylab="h(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w4,type="l",col="green")
lines(x,w1,type="l",col="black")
lines(x,w5,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.49,","~phi=="993.4")),expression(paste(eta==0.38,","~phi=="870.9")),expression(paste(eta==0.97,","~phi=="970.8")),expression(paste(eta==0.61,","~phi=="793.2")),expression(paste(eta==0.87,","~phi=="866.7"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)

####################################################################################
##Plots for the survival function of the New distribution
par(mfrow=c(2,2))
x<-seq(0,35)
a1= 0.7
b1= 20.4
s1= (1+(a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1))/(a1+2*b1+6))*exp(-a1*x)
a2= 0.6
b2= 50.7
s2= (1+(a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2))/(a2+2*b2+6))*exp(-a2*x)
a3= 0.7
b3= 40.1
s3= (1+(a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3))/(a3+2*b3+6))*exp(-a3*x)
a4= 0.5
b4= 60.20
s4= (1+(a1^4*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4))/(a4+2*b4+6))*exp(-a4*x)
a5= 0.4
b5= 32.2
s5= (1+(a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5))/(a5+2*b5+6))*exp(-a5*x)
plot(x,s2,type="l",col="red",ylab="S(x)",pch="*")
lines(x,s1,type="l",col="blue")
lines(x,s3,type="l",col="green")
lines(x,s5,type="l",col="black")
lines(x,s4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.7,","~phi=="20.4")),expression(paste(eta==0.6,","~phi=="50.7")),expression(paste(eta==0.7,","~phi=="40.1")),expression(paste(eta==0.5,","~phi=="60.20")),expression(paste(eta==0.4,","~phi=="32.2"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.7)


x<-seq(0,35)
a1= 0.8
b1= 70.4
s1= (1+(a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1))/(a1+2*b1+6))*exp(-a1*x)
a2= 0.9
b2= 60.7
s2= (1+(a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2))/(a2+2*b2+6))*exp(-a2*x)
a3= 0.3
b3= 80.1
s3= (1+(a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3))/(a3+2*b3+6))*exp(-a3*x)
a4= 0.6
b4= 90.20
s4= (1+(a1^4*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4))/(a4+2*b4+6))*exp(-a4*x)
a5= 0.2
b5= 55.0
s5= (1+(a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5))/(a5+2*b5+6))*exp(-a5*x)
plot(x,s2,type="l",col="red",ylab="S(x)",pch="*")
lines(x,s1,type="l",col="blue")
lines(x,s3,type="l",col="green")
lines(x,s5,type="l",col="black")
lines(x,s4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.28,","~phi=="100.4")),expression(paste(eta==0.45,","~phi=="114.7")),expression(paste(eta==0.37,","~phi=="110.1")),expression(paste(eta==0.64,","~phi=="120.0")),expression(paste(eta==0.59,","~phi=="115.0"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,35)
a1= 0.28
b1= 100.4
s1= (1+(a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1))/(a1+2*b1+6))*exp(-a1*x)
a2= 0.45
b2= 114.7
s2= (1+(a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2))/(a2+2*b2+6))*exp(-a2*x)
a3= 0.37
b3= 110.1
s3= (1+(a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3))/(a3+2*b3+6))*exp(-a3*x)
a4= 0.64
b4= 120.0
s4= (1+(a1^4*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4))/(a4+2*b4+6))*exp(-a4*x)
a5= 0.59
b5= 115.0
s5= (1+(a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5))/(a5+2*b5+6))*exp(-a5*x)
plot(x,s2,type="l",col="red",ylab="S(x)",pch="*")
lines(x,s1,type="l",col="blue")
lines(x,s3,type="l",col="green")
lines(x,s5,type="l",col="black")
lines(x,s4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.8,","~phi=="70.4")),expression(paste(eta==0.9,","~phi=="60.7")),expression(paste(eta==0.3,","~phi=="80.1")),expression(paste(eta==0.6,","~phi=="90.20")),expression(paste(eta==0.2,","~phi=="55.0"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,35)
a1= 0.79
b1= 130.4
s1= (1+(a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1))/(a1+2*b1+6))*exp(-a1*x)
a2= 0.84
b2= 144.5
s2= (1+(a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2))/(a2+2*b2+6))*exp(-a2*x)
a3= 0.73
b3= 130.3
s3= (1+(a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3))/(a3+2*b3+6))*exp(-a3*x)
a4= 0.23
b4= 150.0
s4= (1+(a1^4*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4))/(a4+2*b4+6))*exp(-a4*x)
a5= 0.39
b5= 165.0
s5= (1+(a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5))/(a5+2*b5+6))*exp(-a5*x)
plot(x,s2,type="l",col="red",ylab="S(x)",pch="*")
lines(x,s1,type="l",col="blue")
lines(x,s3,type="l",col="green")
lines(x,s5,type="l",col="black")
lines(x,s4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.79,","~phi=="130.4")),expression(paste(eta==0.84,","~phi=="144.5")),expression(paste(eta==0.73,","~phi=="130.3")),expression(paste(eta==0.23,","~phi=="150.0")),expression(paste(eta==0.39,","~phi=="165.0"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)

####################################################################################
##Plots for the Mean Residual life Function of the New Remkan distribution
####################################################################################
par(mfrow=c(2,2))
x<-seq(0,50)
a1= 2.7
b1= 60.4
d1= (a1^3*x^3+a1^2*x^2*(3+3*a1+a1*b1)+a1*x*(6+12*a1+4*a1*b1)+(6+18*a1+6*a1*b1+a1^2))
c1= (a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1)+(a1+2*b1+6))
w1= d1/c1
a2= 2.6
b2= 50.7
d2= (a2^3*x^3+a2^2*x^2*(3+3*a1+a2*b2)+a2*x*(6+12*a2+4*a2*b2)+(6+18*a2+6*a2*b2+a2^2))
c2= (a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2)+(a2+2*b2+6))
w2= d2/c2
a3= 1.8
b3= 40.1
d3= (a3^3*x^3+a3^2*x^2*(3+3*a3+a3*b3)+a3*x*(6+12*a3+4*a3*b3)+(6+18*a3+6*a3*b3+a3^2))
c3= (a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3)+(a3+2*b3+6))
w3= d3/c3
a4= 1.2
b4= 70.20
d4= (a4^3*x^3+a4^2*x^2*(3+3*a4+a4*b4)+a4*x*(6+12*a4+4*a4*b4)+(6+18*a4+6*a4*b4+a4^2))
c4= (a4^3*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4)+(a4+2*b4+6))
w4= d4/c4
a5= 1.1
b5= 30.2
d5= (a5^3*x^3+a5^2*x^2*(3+3*a5+a5*b5)+a5*x*(6+12*a5+4*a5*b5)+(6+18*a5+6*a5*b5+a5^2))
c5= (a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5)+(a5+2*b5+6))
w5= d5/c5
plot(x,w3,type="l",col="red",ylab="m(x)",pch="*")
lines(x,w1,type="l",col="blue")
lines(x,w2,type="l",col="green")
lines(x,w5,type="l",col="black")
lines(x,w4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==2.7,","~phi=="60.4")),expression(paste(eta==2.6,","~phi=="50.7")),expression(paste(eta==1.8,","~phi=="40.1")),expression(paste(eta==1.2,","~phi=="70.20")),expression(paste(eta==1.1,","~phi=="30.2"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,50)
a1= 4.85
b1= 78.4
d1= (a1^3*x^3+a1^2*x^2*(3+3*a1+a1*b1)+a1*x*(6+12*a1+4*a1*b1)+(6+18*a1+6*a1*b1+a1^2))
c1= (a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1)+(a1+2*b1+6))
w1= d1/c1
a2= 5.09
b2= 820.7
d2= (a2^3*x^3+a2^2*x^2*(3+3*a1+a2*b2)+a2*x*(6+12*a2+4*a2*b2)+(6+18*a2+6*a2*b2+a2^2))
c2= (a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2)+(a2+2*b2+6))
w2= d2/c2
a3= 4.06
b3= 620.1
d3= (a3^3*x^3+a3^2*x^2*(3+3*a3+a3*b3)+a3*x*(6+12*a3+4*a3*b3)+(6+18*a3+6*a3*b3+a3^2))
c3= (a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3)+(a3+2*b3+6))
w3= d3/c3
a4= 6.08
b4= 840.20
d4= (a4^3*x^3+a4^2*x^2*(3+3*a4+a4*b4)+a4*x*(6+12*a4+4*a4*b4)+(6+18*a4+6*a4*b4+a4^2))
c4= (a4^3*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4)+(a4+2*b4+6))
w4= d4/c4
a5= 5.364
b5= 390.2
d5= (a5^3*x^3+a5^2*x^2*(3+3*a5+a5*b5)+a5*x*(6+12*a5+4*a5*b5)+(6+18*a5+6*a5*b5+a5^2))
c5= (a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5)+(a5+2*b5+6))
w5= d5/c5
plot(x,w1,type="l",col="red",ylab="m(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w5,type="l",col="green")
lines(x,w4,type="l",col="black")
lines(x,w3,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==4.85,","~phi=="78.4")),expression(paste(eta==5.09,","~phi=="820.7")),expression(paste(eta==4.06,","~phi=="620.1")),expression(paste(eta==6.08,","~phi=="840.20")),expression(paste(eta==5.364,","~phi=="390.2"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,50)
a1= 1.71
b1= 580.4
d1= (a1^3*x^3+a1^2*x^2*(3+3*a1+a1*b1)+a1*x*(6+12*a1+4*a1*b1)+(6+18*a1+6*a1*b1+a1^2))
c1= (a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1)+(a1+2*b1+6))
w1= d1/c1
a2= 3.54
b2= 720.3
d2= (a2^3*x^3+a2^2*x^2*(3+3*a1+a2*b2)+a2*x*(6+12*a2+4*a2*b2)+(6+18*a2+6*a2*b2+a2^2))
c2= (a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2)+(a2+2*b2+6))
w2= d2/c2
a3= 3.93
b3= 610.6
d3= (a3^3*x^3+a3^2*x^2*(3+3*a3+a3*b3)+a3*x*(6+12*a3+4*a3*b3)+(6+18*a3+6*a3*b3+a3^2))
c3= (a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3)+(a3+2*b3+6))
w3= d3/c3
a4= 2.1
b4= 930.0
d4= (a4^3*x^3+a4^2*x^2*(3+3*a4+a4*b4)+a4*x*(6+12*a4+4*a4*b4)+(6+18*a4+6*a4*b4+a4^2))
c4= (a4^3*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4)+(a4+2*b4+6))
w4= d4/c4
a5= 2.36
b5= 120.1
d5= (a5^3*x^3+a5^2*x^2*(3+3*a5+a5*b5)+a5*x*(6+12*a5+4*a5*b5)+(6+18*a5+6*a5*b5+a5^2))
c5= (a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5)+(a5+2*b5+6))
w5= d5/c5
plot(x,w1,type="l",col="red",ylab="m(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w3,type="l",col="green")
lines(x,w4,type="l",col="black")
lines(x,w5,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==1.71,","~phi=="580.4")),expression(paste(eta==3.54,","~phi=="720.3")),expression(paste(eta==3.93,","~phi=="610.6")),expression(paste(eta==2.1,","~phi=="930.0")),expression(paste(eta==2.36,","~phi=="120.1"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,50)
a1= 0.49
b1= 993.8
d1= (a1^3*x^3+a1^2*x^2*(3+3*a1+a1*b1)+a1*x*(6+12*a1+4*a1*b1)+(6+18*a1+6*a1*b1+a1^2))
c1= (a1^3*x^3+a1^2*x^2*(3+b1)+a1*x*(6+2*b1)+(a1+2*b1+6))
w1= d1/c1
a2= 0.38
b2= 87.9
d2= (a2^3*x^3+a2^2*x^2*(3+3*a1+a2*b2)+a2*x*(6+12*a2+4*a2*b2)+(6+18*a2+6*a2*b2+a2^2))
c2= (a2^3*x^3+a2^2*x^2*(3+b2)+a2*x*(6+2*b2)+(a2+2*b2+6))
w2= d2/c2
a3= 0.27
b3= 970.3
d3= (a3^3*x^3+a3^2*x^2*(3+3*a3+a3*b3)+a3*x*(6+12*a3+4*a3*b3)+(6+18*a3+6*a3*b3+a3^2))
c3= (a3^3*x^3+a3^2*x^2*(3+b3)+a3*x*(6+2*b3)+(a3+2*b3+6))
w3= d3/c3
a4= 0.51
b4= 733.7
d4= (a4^3*x^3+a4^2*x^2*(3+3*a4+a4*b4)+a4*x*(6+12*a4+4*a4*b4)+(6+18*a4+6*a4*b4+a4^2))
c4= (a4^3*x^3+a4^2*x^2*(3+b4)+a4*x*(6+2*b4)+(a4+2*b4+6))
w4= d4/c4
a5= 0.21
b5= 860.7
d5= (a5^3*x^3+a5^2*x^2*(3+3*a5+a5*b5)+a5*x*(6+12*a5+4*a5*b5)+(6+18*a5+6*a5*b5+a5^2))
c5= (a5^3*x^3+a5^2*x^2*(3+b5)+a5*x*(6+2*b5)+(a5+2*b5+6))
w5= d5/c5
plot(x,w3,type="l",col="red",ylab="m(x)",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w4,type="l",col="green")
lines(x,w1,type="l",col="black")
lines(x,w5,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.49,","~phi=="993.8")),expression(paste(eta==0.38,","~phi=="87.9")),expression(paste(eta==0.27,","~phi=="970.3")),expression(paste(eta==0.51,","~phi=="733.7")),expression(paste(eta==0.21,","~phi=="860.7"))),col=c("red","blue","green","black","orange"),lty=1:1:1:1:2,cex=0.6)

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




