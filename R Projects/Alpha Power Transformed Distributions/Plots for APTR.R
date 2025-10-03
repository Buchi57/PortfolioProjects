##Plots for the pdf of the Alpha Power Transformed Remkan distribution
par(mfrow=c(2,2))
x<-seq(0,50)
a1= 0.5
b1= 142.0
c1= 100
p1= ((a1^2*log(c1)/(c1-1)*(6+2*b1+a1))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))*c1^(1-(1+(a1^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
a2= 0.4
b2= 143.7
c2= 400
p2= ((a2^2*log(c2)/(c2-1)*(6+2*b2+a2))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))*c2^(1-(1+(a2^3*x^2*b2+(3+b2)*a2^2*x^3+(2*b2+6)*a2*x)/(6+2*b2+a2))*exp(-a2*x))
a3= 0.6
b3= 148.5
c3= 350
p3= ((a3^2*log(c3)/(c3-1)*(6+2*b3+a3))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))*c3^(1-(1+(a3^3*x^2*b3+(3+b3)*a3^2*x^3+(2*b3+6)*a3*x)/(6+2*b3+a3))*exp(-a3*x))
a4= 0.9
b4= 140.0
c4= 520
p4= ((a4^2*log(c4)/(c4-1)*(6+2*b4+a4))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))*c4^(1-(1+(a4^3*x^2*b4+(3+b4)*a4^2*x^3+(2*b4+6)*a4*x)/(6+2*b4+a4))*exp(-a4*x))
a5= 0.3
b5= 145.0
c5= 540
p5= ((a5^2*log(c5)/(c5-1)*(6+2*b5+a5))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))*c5^(1-(1+(a5^3*x^2*b5+(3+b5)*a5^2*x^3+(2*b5+6)*a5*x)/(6+2*b5+a5))*exp(-a5*x))
plot(x,p1,type="l",col="red",ylab="f(x,"~eta~","~phi~","~tau~")",pch="*")
lines(x,p5,type="l",col="blue")
lines(x,p3,type="l",col="green")
lines(x,p4,type="l",col="black")
lines(x,p2,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==0.5,","~phi=="142.0",","~tau=="100.0")),expression(paste(eta==0.4,","~phi=="143.7",","~tau=="400.0")),expression(paste(eta==0.6,","~phi=="148.5",","~tau=="350.0")),expression(paste(eta==0.9,","~phi=="140.0",","~tau=="520.0")),expression(paste(eta==0.3,","~phi=="145.0",","~tau=="540.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.8)

x<-seq(0,50)
a1= 1.1
b1= 294.0
c1= 200
p1= ((a1^2*log(c1)/(c1-1)*(6+2*b1+a1))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))*c1^(1-(1+(a1^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
a2= 0.4
b2= 291.7
c2= 240
p2= ((a2^2*log(c2)/(c2-1)*(6+2*b2+a2))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))*c2^(1-(1+(a2^3*x^2*b2+(3+b2)*a2^2*x^3+(2*b2+6)*a2*x)/(6+2*b2+a2))*exp(-a2*x))
a3= 0.56
b3= 298.5
c3= 230
p3= ((a3^2*log(c3)/(c3-1)*(6+2*b3+a3))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))*c3^(1-(1+(a3^3*x^2*b3+(3+b3)*a3^2*x^3+(2*b3+6)*a3*x)/(6+2*b3+a3))*exp(-a3*x))
a4= 0.7
b4= 293.0
c4= 320
p4= ((a4^2*log(c4)/(c4-1)*(6+2*b4+a4))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))*c4^(1-(1+(a4^3*x^2*b4+(3+b4)*a4^2*x^3+(2*b4+6)*a4*x)/(6+2*b4+a4))*exp(-a4*x))
a5= 0.9
b5= 295.0
c5= 340
p5= ((a5^2*log(c5)/(c5-1)*(6+2*b5+a5))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))*c5^(1-(1+(a5^3*x^2*b5+(3+b5)*a5^2*x^3+(2*b5+6)*a5*x)/(6+2*b5+a5))*exp(-a5*x))
plot(x,p1,type="l",col="red",ylab="f(x,"~eta~","~phi~","~tau~")",pch="*")
lines(x,p5,type="l",col="blue")
lines(x,p3,type="l",col="green")
lines(x,p4,type="l",col="black")
lines(x,p2,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==1.1,","~phi=="294.0",","~tau=="200.0")),expression(paste(eta==0.4,","~phi=="291.7",","~tau=="240.0")),expression(paste(eta==0.56,","~phi=="298.5",","~tau=="230.0")),expression(paste(eta==0.7,","~phi=="293.0",","~tau=="320.0")),expression(paste(eta==0.9,","~phi=="295.0",","~tau=="340.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.8)

x<-seq(0,50)
a1= 0.8
b1= 200.0
c1= 200
p1= ((a1^2*log(c1)/(c1-1)*(6+2*b1+a1))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))*c1^(1-(1+(a1^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
a2= 0.5
b2= 210.7
c2= 440
p2= ((a2^2*log(c2)/(c2-1)*(6+2*b2+a2))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))*c2^(1-(1+(a2^3*x^2*b2+(3+b2)*a2^2*x^3+(2*b2+6)*a2*x)/(6+2*b2+a2))*exp(-a2*x))
a3= 0.6
b3= 198.5
c3= 230
p3= ((a3^2*log(c3)/(c3-1)*(6+2*b3+a3))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))*c3^(1-(1+(a3^3*x^2*b3+(3+b3)*a3^2*x^3+(2*b3+6)*a3*x)/(6+2*b3+a3))*exp(-a3*x))
a4= 0.4
b4= 200.0
c4= 310
p4= ((a4^2*log(c4)/(c4-1)*(6+2*b4+a4))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))*c4^(1-(1+(a4^3*x^2*b4+(3+b4)*a4^2*x^3+(2*b4+6)*a4*x)/(6+2*b4+a4))*exp(-a4*x))
a5= 0.3
b5= 195.0
c5= 340
p5= ((a5^2*log(c5)/(c5-1)*(6+2*b5+a5))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))*c5^(1-(1+(a5^3*x^2*b5+(3+b5)*a5^2*x^3+(2*b5+6)*a5*x)/(6+2*b5+a5))*exp(-a5*x))
plot(x,p1,type="l",col="red",ylab="f(x,"~eta~","~phi~","~tau~")",pch="*")
lines(x,p5,type="l",col="blue")
lines(x,p3,type="l",col="green")
lines(x,p4,type="l",col="black")
lines(x,p2,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==0.8,","~phi=="200.0",","~tau=="200.0")),expression(paste(eta==0.5,","~phi=="210.7",","~tau=="440.0")),expression(paste(eta==0.6,","~phi=="198.5",","~tau=="230.0")),expression(paste(eta==0.4,","~phi=="200.0",","~tau=="310.0")),expression(paste(eta==0.3,","~phi=="195.0",","~tau=="340.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.8)

x<-seq(0,50)
a1= 0.32
b1= 324.0
c1= 200
p1= ((a1^2*log(c1)/(c1-1)*(6+2*b1+a1))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))*c1^(1-(1+(a1^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
a2= 0.15
b2= 331.0
c2= 270
p2= ((a2^2*log(c2)/(c2-1)*(6+2*b2+a2))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))*c2^(1-(1+(a2^3*x^2*b2+(3+b2)*a2^2*x^3+(2*b2+6)*a2*x)/(6+2*b2+a2))*exp(-a2*x))
a3= 0.35
b3= 198.0
c3= 230
p3= ((a3^2*log(c3)/(c3-1)*(6+2*b3+a3))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))*c3^(1-(1+(a3^3*x^2*b3+(3+b3)*a3^2*x^3+(2*b3+6)*a3*x)/(6+2*b3+a3))*exp(-a3*x))
a4= 0.32
b4= 238.0
c4= 310
p4= ((a4^2*log(c4)/(c4-1)*(6+2*b4+a4))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))*c4^(1-(1+(a4^3*x^2*b4+(3+b4)*a4^2*x^3+(2*b4+6)*a4*x)/(6+2*b4+a4))*exp(-a4*x))
a5= 0.45
b5= 245.0
c5= 340
p5= ((a5^2*log(c5)/(c5-1)*(6+2*b5+a5))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))*c5^(1-(1+(a5^3*x^2*b5+(3+b5)*a5^2*x^3+(2*b5+6)*a5*x)/(6+2*b5+a5))*exp(-a5*x))
plot(x,p1,type="l",col="red",ylab="f(x,"~eta~","~phi~","~tau~")",pch="*")
lines(x,p3,type="l",col="blue")
lines(x,p2,type="l",col="green")
lines(x,p4,type="l",col="black")
lines(x,p5,type="l",col="gold")
legend("topright",legend=c(expression(paste(eta==0.32,","~phi=="324.0",","~tau=="200.0")),expression(paste(eta==0.15,","~phi=="331.0",","~tau=="270.0")),expression(paste(eta==0.35,","~phi=="198.0",","~tau=="230.0")),expression(paste(eta==0.32,","~phi=="238.0",","~tau=="310.0")),expression(paste(eta==0.45,","~phi=="245.0",","~tau=="340.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.8)


####################################################################################
##Plots for the cdf of the the Alpha Power Transformed Remkan distribution
par(mfrow=c(2,2))
x<-seq(0,40)
a1= 0.7
b1= 20.4
c1= 25.0
q1= (c1^(1-(1+(a1^3*x^3*b1+(3+b1)*a1^2*x^3+(6+2*b1)*a1*x)/(6+2*b1+a1))*exp(-a1*x))-1)/(c1-1)
a2= 0.6
b2= 50.7
c2= 42.5
q2= (c2^(1-(1+(a2^3*x^3*b2+(3+b2)*a2^2*x^3+(6+2*b2)*a2*x)/(6+2*b2+a2))*exp(-a2*x))-1)/(c2-1)
a3= 0.7
b3= 40.1
c3= 45.1
q3= (c3^(1-(1+(a3^3*x^3*b3+(3+b3)*a3^2*x^3+(6+2*b3)*a3*x)/(6+2*b3+a3))*exp(-a3*x))-1)/(c3-1)
a4= 0.5
b4= 60.20
c4= 55.2
q4= (c4^(1-(1+(a4^3*x^3*b4+(3+b4)*a4^2*x^3+(6+2*b4)*a4*x)/(6+2*b4+a4))*exp(-a4*x))-1)/(c4-1)
a5= 0.4
b5= 32.2
c5= 40.5
q5= (c5^(1-(1+(a5^3*x^3*b5+(3+b5)*a5^2*x^3+(6+2*b5)*a5*x)/(6+2*b5+a5))*exp(-a5*x))-1)/(c5-1)
plot(x,q2,type="l",col="red",ylab="F(x,"~eta~","~phi~","~tau~")",pch="*")
lines(x,q1,type="l",col="blue")
lines(x,q3,type="l",col="green")
lines(x,q5,type="l",col="black")
lines(x,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.7,","~phi=="20.4",","~tau=="25.0")),expression(paste(eta==0.6,","~phi=="50.7",","~tau=="42.5")),expression(paste(eta==0.7,","~phi=="40.1",","~tau=="45.1")),expression(paste(eta==0.5,","~phi=="60.20",","~tau=="55.2")),expression(paste(eta==0.4,","~phi=="32.2",","~tau=="40.5"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,40)
a1= 0.8
b1= 70.4
c1= 25.0
q1= (c1^(1-(1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x))-1)/(c1-1)
a2= 0.9
b2= 60.7
c2= 42.5
q2= (c2^(1-(1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x))-1)/(c2-1)
a3= 0.3
b3= 80.1
c3= 45.1
q3= (c3^(1-(1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x))-1)/(c3-1)
a4= 0.6
b4= 90.20
c4= 55.2
q4= (c4^(1-(1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x))-1)/(c4-1)
a5= 0.2
b5= 55.0
c5= 40.5
q5= (c5^(1-(1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x))-1)/(c5-1)
plot(x,q2,type="l",col="red",ylab="F(x,"~eta~","~phi~","~tau~")",pch="*")
lines(x,q1,type="l",col="blue")
lines(x,q3,type="l",col="green")
lines(x,q5,type="l",col="black")
lines(x,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.8,","~phi=="70.4",","~tau=="25.0")),expression(paste(eta==0.9,","~phi=="60.7",","~tau=="42.5")),expression(paste(eta==0.3,","~phi=="80.1",","~tau=="45.1")),expression(paste(eta==0.6,","~phi=="90.20",","~tau=="55.2")),expression(paste(eta==0.2,","~phi=="55.0",","~tau=="40.5"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,40)
a1= 0.28
b1= 100.4
c1= 78.0
q1= (c1^(1-(1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x))-1)/(c1-1)
a2= 0.45
b2= 114.7
c2= 82.5
q2= (c2^(1-(1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x))-1)/(c2-1)
a3= 0.37
b3= 110.1
c3= 75.1
q3= (c3^(1-(1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x))-1)/(c3-1)
a4= 0.64
b4= 120.0
c4= 95.2
q4= (c4^(1-(1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x))-1)/(c4-1)
a5= 0.59
b5= 115.0
c5= 80.5
q5= (c5^(1-(1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x))-1)/(c5-1)
plot(x,q2,type="l",col="red",ylab="F(x,"~eta~","~phi~","~tau~")",pch="*")
lines(x,q1,type="l",col="blue")
lines(x,q3,type="l",col="green")
lines(x,q5,type="l",col="black")
lines(x,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.28,","~phi=="100.4",","~tau=="78.0")),expression(paste(eta==0.45,","~phi=="114.7",","~tau=="82.5")),expression(paste(eta==0.37,","~phi=="110.1",","~tau=="75.1")),expression(paste(eta==0.64,","~phi=="120.0",","~tau=="95.2")),expression(paste(eta==0.59,","~phi=="115.0",","~tau=="80.5"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,40)
a1= 0.49
b1= 130.4
c1= 178.0
q1= (c1^(1-(1+(a1^3*x^3*b1+3*a1^2*x^2*b1+6*a1*x*b1)/(6*(b1+a1)))*exp(-a1*x))-1)/(c1-1)
a2= 0.34
b2= 144.5
c2= 182.5
q2= (c2^(1-(1+(a2^3*x^3*b2+3*a2^2*x^2*b2+6*a2*x*b2)/(6*(b2+a2)))*exp(-a2*x))-1)/(c2-1)
a3= 0.43
b3= 130.3
c3= 175.1
q3= (c3^(1-(1+(a3^3*x^3*b3+3*a3^2*x^2*b3+6*a3*x*b3)/(6*(b3+a3)))*exp(-a3*x))-1)/(c3-1)
a4= 0.37
b4= 150.0
c4= 195.2
q4= (c4^(1-(1+(a4^3*x^3*b4+3*a4^2*x^2*b4+6*a4*x*b4)/(6*(b4+a4)))*exp(-a4*x))-1)/(c4-1)
a5= 0.39
b5= 165.0
c5= 180.5
q5= (c5^(1-(1+(a5^3*x^3*b5+3*a5^2*x^2*b5+6*a5*x*b5)/(6*(b5+a5)))*exp(-a5*x))-1)/(c5-1)
plot(x,q1,type="l",col="red",ylab="F(x,"~eta~","~phi~","~tau~")",pch="*")
lines(x,q2,type="l",col="blue")
lines(x,q3,type="l",col="green")
lines(x,q5,type="l",col="black")
lines(x,q4,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.49,","~phi=="130.4",","~tau=="178.0")),expression(paste(eta==0.34,","~phi=="144.5",","~tau=="182.5")),expression(paste(eta==0.43,","~phi=="130.3",","~tau=="175.1")),expression(paste(eta==0.37,","~phi=="150.0",","~tau=="195.2")),expression(paste(eta==0.39,","~phi=="165.0",","~tau=="180.5"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


####################################################################################
##Plots for the Hazard Rate Function of the the Alpha Power Transformed Remkan distribution
par(mfrow=c(2,2))
x<-seq(0,45)
a1= 0.7
b1= 2.4
c1= 1.2
d1= ((a1^2*log(c1)/(c1-1)*(6+2*b1+a1))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))*c1^(1-(1+(a1^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e1= (c1^(1-(1+(a1^3*x^3*b1+(3+b1)*a1^2*x^3+(6+2*b1)*a1*x)/(6+2*b1+a1))*exp(-a1*x))-c1)/(1-c1)
w1= d1/e1
a2= 1.6
b2= 9.7
c2= 2.3
d2= ((a2^2*log(c2)/(c2-1)*(6+2*b2+a2))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))*c2^(1-(1+(a2^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e2= (c2^(1-(1+(a2^3*x^3*b2+(3+b2)*a2^2*x^3+(6+2*b2)*a2*x)/(6+2*b2+a2))*exp(-a2*x))-c2)/(1-c2)
w2= d2/e2
a3= 0.87
b3= 2.1
c3= 6.0
d3= ((a3^2*log(c3)/(c3-1)*(6+2*b3+a3))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))*c3^(1-(1+(a3^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e3= (c3^(1-(1+(a3^3*x^3*b3+(3+b3)*a3^2*x^3+(6+2*b3)*a3*x)/(6+2*b3+a3))*exp(-a3*x))-c3)/(1-c3)
w3= d3/e3
a4= 0.5
b4= 6.2
c4= 5.0
d4= ((a4^2*log(c4)/(c4-1)*(6+2*b4+a4))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))*c4^(1-(1+(a4^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e4= (c4^(1-(1+(a4^3*x^3*b4+(3+b4)*a4^2*x^3+(6+2*b4)*a4*x)/(6+2*b4+a4))*exp(-a4*x))-c4)/(1-c4)
w4= d4/e4
a5= 0.45
b5= 5.2
c5= 10.0
d5= ((a5^2*log(c5)/(c5-1)*(6+2*b5+a5))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))*c5^(1-(1+(a5^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e5= (c5^(1-(1+(a5^3*x^3*b5+(3+b5)*a5^2*x^3+(6+2*b5)*a5*x)/(6+2*b5+a5))*exp(-a5*x))-c5)/(1-c5)
w5= d5/e5
plot(x,w3,type="l",col="red",ylab="h(x,"~eta~","~phi~","~tau~")",pch="*")
lines(x,w1,type="l",col="blue")
lines(x,w2,type="l",col="green")
lines(x,w5,type="l",col="black")
lines(x,w4,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.7,","~phi=="2.4",","~tau=="1.2")),expression(paste(eta==1.6,","~phi=="9.7",","~tau=="2.3")),expression(paste(eta==0.87,","~phi=="2.1",","~tau=="6.0")),expression(paste(eta==0.5,","~phi=="6.2",","~tau=="5.0")),expression(paste(eta==0.45,","~phi=="5.2",","~tau=="10.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,45)
a1= 0.85
b1= 1.4
c1= 19.0
d1= ((a1^2*log(c1)/(c1-1)*(6+2*b1+a1))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))*c1^(1-(1+(a1^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e1= (c1^(1-(1+(a1^3*x^3*b1+(3+b1)*a1^2*x^3+(6+2*b1)*a1*x)/(6+2*b1+a1))*exp(-a1*x))-c1)/(1-c1)
w1= d1/e1
a2= 0.91
b2= 7.4
c2= 11.0
d2= ((a2^2*log(c2)/(c2-1)*(6+2*b2+a2))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))*c2^(1-(1+(a2^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e2= (c2^(1-(1+(a2^3*x^3*b2+(3+b2)*a2^2*x^3+(6+2*b2)*a2*x)/(6+2*b2+a2))*exp(-a2*x))-c2)/(1-c2)
w2= d2/e2
a3= 0.73
b3= 6.6
c3= 10.0
d3= ((a3^2*log(c3)/(c3-1)*(6+2*b3+a3))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))*c3^(1-(1+(a3^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e3= (c3^(1-(1+(a3^3*x^3*b3+(3+b3)*a3^2*x^3+(6+2*b3)*a3*x)/(6+2*b3+a3))*exp(-a3*x))-c3)/(1-c3)
w3= d3/e3
a4= 0.57
b4= 2.30
c4= 16.0
d4= ((a4^2*log(c4)/(c4-1)*(6+2*b4+a4))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))*c4^(1-(1+(a4^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e4= (c4^(1-(1+(a4^3*x^3*b4+(3+b4)*a4^2*x^3+(6+2*b4)*a4*x)/(6+2*b4+a4))*exp(-a4*x))-c4)/(1-c4)
w4= d4/e4
a5= 0.64
b5= 1.2
c5= 12.0
d5= ((a5^2*log(c5)/(c5-1)*(6+2*b5+a5))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))*c5^(1-(1+(a5^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e5= (c5^(1-(1+(a5^3*x^3*b5+(3+b5)*a5^2*x^3+(6+2*b5)*a5*x)/(6+2*b5+a5))*exp(-a5*x))-c5)/(1-c5)
w5= d5/e5
plot(x,w3,type="l",col="red",ylab="h(x,"~eta~","~phi~","~tau~")",pch="*")
lines(x,w2,type="l",col="blue")
lines(x,w5,type="l",col="green")
lines(x,w4,type="l",col="black")
lines(x,w1,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.85,","~phi=="190.4",","~tau=="290.0")),expression(paste(eta==0.91,","~phi=="170.4",","~tau=="150.0")),expression(paste(eta==0.83,","~phi=="180.6",","~tau=="190.0")),expression(paste(eta==0.57,","~phi=="240.30",","~tau=="265.0")),expression(paste(eta==0.64,","~phi=="190.2",","~tau=="120.0"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


x<-seq(0,30)
a1= 0.7
b1= 3.4
c1= 0.2
d1= ((a1^2*log(c1)/(c1-1)*(6+2*b1+a1))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))*c1^(1-(1+(a1^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e1= (c1^(1-(1+(a1^3*x^3*b1+(3+b1)*a1^2*x^3+(6+2*b1)*a1*x)/(6+2*b1+a1))*exp(-a1*x))-c1)/(1-c1)
w1= d1/e1
a2= 0.64
b2= 2.3
c2= 0.4
d2= ((a2^2*log(c2)/(c2-1)*(6+2*b2+a2))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))*c2^(1-(1+(a2^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e2= (c2^(1-(1+(a2^3*x^3*b2+(3+b2)*a2^2*x^3+(6+2*b2)*a2*x)/(6+2*b2+a2))*exp(-a2*x))-c2)/(1-c2)
w2= d2/e2
a3= 0.47
b3= 4.8
c3= 0.5
d3= ((a3^2*log(c3)/(c3-1)*(6+2*b3+a3))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))*c3^(1-(1+(a3^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e3= (c3^(1-(1+(a3^3*x^3*b3+(3+b3)*a3^2*x^3+(6+2*b3)*a3*x)/(6+2*b3+a3))*exp(-a3*x))-c3)/(1-c3)
w3= d3/e3
a4= 0.8
b4= 2.0
c4= 0.7
d4= ((a4^2*log(c4)/(c4-1)*(6+2*b4+a4))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))*c4^(1-(1+(a4^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e4= (c4^(1-(1+(a4^3*x^3*b4+(3+b4)*a4^2*x^3+(6+2*b4)*a4*x)/(6+2*b4+a4))*exp(-a4*x))-c4)/(1-c4)
w4= d4/e4
a5= 0.56
b5= 3.1
c5= 0.3
d5= ((a5^2*log(c5)/(c5-1)*(6+2*b5+a5))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))*c5^(1-(1+(a5^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e5= (c5^(1-(1+(a5^3*x^3*b5+(3+b5)*a5^2*x^3+(6+2*b5)*a5*x)/(6+2*b5+a5))*exp(-a5*x))-c5)/(1-c5)
w5= d5/e5
plot(x,w2,type="l",col="red",ylab="h(x,"~eta~","~phi~","~tau~")",pch="*")
lines(x,w3,type="l",col="blue")
lines(x,w4,type="l",col="green")
lines(x,w5,type="l",col="black")
lines(x,w1,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.7,","~phi=="3.4",","~tau=="0.2")),expression(paste(eta==0.34,","~phi=="2.3",","~tau=="0.4")),expression(paste(eta==0.97,","~phi=="4.8",","~tau=="0.5")),expression(paste(eta==0.8,","~phi=="80.0",","~tau=="60.7")),expression(paste(eta==0.86,","~phi=="3.1",","~tau=="0.3"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)



x<-seq(0,80)
a1= 0.49
b1= 5.4
c1= 9.0
d1= ((a1^2*log(c1)/(c1-1)*(6+2*b1+a1))*(1+b1*a1*x^2+a1^2*x^3)*exp(-a1*x))*c1^(1-(1+(a1^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e1= (c1^(1-(1+(a1^3*x^3*b1+(3+b1)*a1^2*x^3+(6+2*b1)*a1*x)/(6+2*b1+a1))*exp(-a1*x))-c1)/(1-c1)
w1= d1/e1
a2= 0.38
b2= 8.9
c2= 5.0
d2= ((a2^2*log(c2)/(c2-1)*(6+2*b2+a2))*(1+b2*a2*x^2+a2^2*x^3)*exp(-a2*x))*c2^(1-(1+(a2^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e2= (c2^(1-(1+(a2^3*x^3*b2+(3+b2)*a2^2*x^3+(6+2*b2)*a2*x)/(6+2*b2+a2))*exp(-a2*x))-c2)/(1-c2)
w2= d2/e2
a3= 0.97
b3= 7.8
c3= 9.0
d3= ((a3^2*log(c3)/(c3-1)*(6+2*b3+a3))*(1+b3*a3*x^2+a3^2*x^3)*exp(-a3*x))*c3^(1-(1+(a3^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e3= (c3^(1-(1+(a3^3*x^3*b3+(3+b3)*a3^2*x^3+(6+2*b3)*a3*x)/(6+2*b3+a3))*exp(-a3*x))-c3)/(1-c3)
w3= d3/e3
a4= 0.2
b4= 3.2
c4= 14.5
d4= ((a4^2*log(c4)/(c4-1)*(6+2*b4+a4))*(1+b4*a4*x^2+a4^2*x^3)*exp(-a4*x))*c4^(1-(1+(a4^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e4= (c4^(1-(1+(a4^3*x^3*b4+(3+b4)*a4^2*x^3+(6+2*b4)*a4*x)/(6+2*b4+a4))*exp(-a4*x))-c4)/(1-c4)
w4= d4/e4
a5= 0.3
b5= 5.7
c5= 10.6
d5= ((a5^2*log(c5)/(c5-1)*(6+2*b5+a5))*(1+b5*a5*x^2+a5^2*x^3)*exp(-a5*x))*c5^(1-(1+(a5^3*x^2*b1+(3+b1)*a1^2*x^3+(2*b1+6)*a1*x)/(6+2*b1+a1))*exp(-a1*x))
e5= (c5^(1-(1+(a5^3*x^3*b5+(3+b5)*a5^2*x^3+(6+2*b5)*a5*x)/(6+2*b5+a5))*exp(-a5*x))-c5)/(1-c5)
w5= d5/e5
plot(x,w4,type="l",col="red",ylab="h(x,"~eta~","~phi~","~tau~")",pch="*")
lines(x,w3,type="l",col="blue")
lines(x,w5,type="l",col="green")
lines(x,w1,type="l",col="black")
lines(x,w2,type="l",col="orange")
legend("topright",legend=c(expression(paste(eta==0.49,","~phi=="3.4",","~tau=="9.0")),expression(paste(eta==0.38,","~phi=="8.9",","~tau=="5.0")),expression(paste(eta==0.97,","~phi=="70.8",","~tau=="90.0")),expression(paste(eta==0.2,","~phi=="3.2",","~tau=="0.5")),expression(paste(eta==0.3,","~phi=="5.7",","~tau=="1.6"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)

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




