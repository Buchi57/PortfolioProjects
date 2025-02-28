##############################################################################
### Main result for The proposed Inverse Copoun distribution
####################################################################################################
library(dplyr)
library(maxLik)

datas11 <- datas11 %>%
  na.omit()
n <- length(datas16)
x <- datas16
llf <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   return(2*n*log(theta) - n*log(6*(alpha+theta))+sum(log((alpha*theta^2+6*x^3)/x^5))-theta*sum(1/x)
)
}
mean_x <- mean(datas16)
sd_x <- sd(datas16)
starts <- c(theta = 0.1*sd_x, alpha = mean_x/10)
ml <- maxLik(llf, start = starts, method = "BFGS" )
summary(ml)
AIC(ml)
ll <- -2*logLik(ml)
ll
BIC <- ll+(2*log(n))
BIC
A <- AIC(ml)
AICc <- A+(12/(n-3))
AICc
HQIC<- ll+(2*log(log(n)))
HQIC
k <- 2
CAIC <- A + (2*k*(k+1))/(n-k-1)
CAIC

##############################################################################
### Main result for The T-P. Rama distribution
####################################################################################################
n <- length(datas16)
x <- datas16
llfe <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   return(4*n*log(theta) - n*log(alpha*theta^3+6)+sum(log(alpha+x^3)-theta*sum(x)
)
}
mean_x <- mean(datas16)
sd_x <- sd(datas16)
starts <- c(theta = 0.1*sd_x, alpha = mean_x/10)
mle <- maxLik(llfe, start = starts, method = "BFGS" )
summary(mle)
AIC(mle)
lle <- -2*logLik(mle)
lle
BICe <- lle+(2*log(n))
BICe
A <- AIC(ml)
AICc <- A+(12/(n-3))
AICc
HQIC<- ll+(2*log(log(n)))
HQIC
k <- 2
CAIC <- A + (2*k*(k+1))/(n-k-1)
CAIC

##############################################################################
### Main result for The Inverse Akash distribution
####################################################################################################
n <- length(datas16)
x <- datas16
llfc <- function( param ) {
   theta <- param[ 1 ]
     return(3*n*log(theta) - n*log(theta^2+2)+sum(log((1+x^2)/x^4))-theta*sum(1/x)
)
}
mean_x <- mean(datas16)
starts <- c(theta = 0.1*mean_x)
mlc <- maxLik(llfc, start = starts, method = "BFGS" )
summary(mlc)
AIC(mlc)
llc <- -2*logLik(mlc)
llc
BICc <- llc+(2*log(n))
BICc
HQICc<- llc+(2*log(log(n)))
HQICc
k <- 2
Ac <- AIC(mlc)
CAICc <- Ac + (2*k*(k+1))/(n-k-1)
CAICc

##############################################################################
### Main result for The Inverse Suja distribution
####################################################################################################
n <- length(datas16)
x <- datas16
llfd <- function( param ) {
   theta <- param[ 1 ]
     return(5*n*log(theta) - n*log(theta^4+24)+sum(log((1+x^4)/x^6))-theta*sum(1/x)
)
}
mean_x <- mean(datas16)
starts <- c(theta = 0.1*mean_x)
mld <- maxLik(llfd, start = starts, method = "BFGS" )
summary(mld)
AIC(mld)
lld <- -2*logLik(mld)
lld
BICd <- lld+(2*log(n))
BICd
HQICd<- lld+(2*log(log(n)))
HQICd
k <- 2
Ad <- AIC(mld)
CAICd <- Ad + (2*k*(k+1))/(n-k-1)
CAICd

##############################################################################
### Main result for The Inverse Lindley distribution
####################################################################################################
n <- length(datas16)
x <- datas16
llfg <- function( param ) {
   theta <- param[ 1 ]
     return(2*n*log(theta) - n*log(1+theta)+sum(log((1+x^2)/x^4))-theta*sum(1/x)
)
}
mean_x <- mean(datas16)
starts <- c(theta = 0.01*mean_x)
mlg <- maxLik(llfg, start = starts, method = "BFGS" )
summary(mlg)
AIC(mlg)
llg <- -2*logLik(mlg)
llg
BICg <- llg+(2*log(n))
BICg
HQICg<- llg+(2*log(log(n)))
HQICg
k <- 2
Ag <- AIC(mlg)
CAICg <- Ag + (2*k*(k+1))/(n-k-1)
CAICg



##############################################################################
### Main result for The proposed new distribution
####################################################################################################
datas12 <- c(0.040, 1.866, 2.385, 3.443, 0.301, 1.876, 2.481, 3.467, 0.309, 1.899, 2.610, 3.478, 0.557, 1.911, 2.625, 3.578, 0.943, 1.912, 2.632, 3.595, 1.070, 1.914, 2.646, 3.699, 1.124, 1.981, 2.661, 3.779, 1.248, 2.010, 2.688, 3.924, 1.281, 2.038, 2.823, 4.035, 1.281, 2.085, 2.890, 4.121, 1.303, 2.089, 2.902, 4.167, 1.432, 2.097, 2.934, 4.240, 1.480, 2.135, 2.962, 4.255, 1.505, 2.154, 2.964, 4.278, 1.506, 2.190, 3.000, 4.305, 1.568, 2.194, 3.103, 4.376, 1.615, 2.223, 3.114, 4.449, 1.619, 2.224, 3.117, 4.485, 1.652, 2.229, 3.166, 4.570, 1.652, 2.300, 3.344, 4.602, 1.757, 2.324, 3.376, 4.663)
datas12 <- datas12 %>%
  na.omit()
n <- length(datas16)
x <- datas16
llfe <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   return(3*n*log(theta) - n*log(theta^2+alpha*theta+2)+sum(log(1+alpha*x+x^2))-theta*sum(x)
)
}
mean_x <- mean(datas16)
sd_x <- sd(datas16)
starts <- c(theta = 0.1*sd_x, alpha = mean_x/10)
mle <- maxLik(llfe, start = starts, method = "BFGS" )
summary(mle)
AIC(mle)
lle <- -2*logLik(mle)
lle
BICe <- lle+(2*log(n))
BICe
HQICe<- lle+(2*log(log(n)))
HQICe
k <- 2
Ae <- AIC(mle)
CAICe <- Ae + (2*k*(k+1))/(n-k-1)
CAICe

Ae <- AIC(mle)
AICce <- Ae+(12/(n-3))
AICce




