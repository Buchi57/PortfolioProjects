##############################################################################
### Main result for The Alpha Power Transformed Inverse Remkan distribution
####################################################################################################
library(dplyr)
library(maxLik)

n <- length(datas25)
x <- datas25
llf <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   beta  <- param[ 3 ]
   return(n*(log(log(alpha))-log(alpha-1)+log(alpha)+2*log(theta)-log(2*beta+theta+6))+sum(log(1+beta*theta*x^2+theta^2*x^3)-theta*x-log(alpha)*sum(1+(beta*theta^3*x^2+(3+beta)*theta^2*x^3+(6+2*beta)*theta*x)/(6+2*beta+theta))*exp(-(theta*x))
)
}
mean_x <- mean(datas25)
sd_x <- sd(datas25)
var_x <- var(datas25)
starts <- c(theta = 0.31*sd_x, alpha = 0.8*mean_x, beta = 0.9*var_x)
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

##datas 11;7;17
############################################################
n <- length(datas26)
x <- datas26
llf <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   beta  <- param[ 3 ]
   return(n*(log(log(alpha))-log(alpha-1)+log(alpha)+2*log(theta)-log(2*beta+theta+6))+sum(log(1+beta*theta*x^2+theta^2*x^3)-theta*x-log(alpha)*sum(1+(beta*theta^3*x^2+(3+beta)*theta^2*x^3+(6+2*beta)*theta*x)/(6+2*beta+theta))*exp(-(theta*x))
)
}
mean_x <- mean(datas26)
sd_x <- sd(datas26)
var_x <- var(datas26)
starts <- c(theta = 1.02*sd_x, alpha = 1.02*mean_x, beta = 1.02*var_x)
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
########################################################################
##############################################################################
### Main result for The Alpha Power within Weibull Quantile Distribution (APWQ)
####################################################################################################
n <- length(datas26)
x <- datas26
llfe <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   beta  <- param[ 3 ]
   return(n*log(alpha-1)-n*log(log(alpha))+n*log(theta*beta)+(beta-1)*sum(log(x))-theta*sum(x^beta)-sum(log(1+(alpha-1)*(1-exp(-theta*x^beta))))
)
}
mean_x <- mean(datas26)
sd_x <- sd(datas26)
var_x <- var(datas26)
starts <- c(theta = 2.2*sd_x, alpha = 2.2*mean_x, beta = 2.5*var_x)
mle <- maxLik(llfe, start = starts, method = "BFGS" )
summary(mle)
AIC(mle)
lle <- -2*logLik(mle)
lle
BICe <- lle+(2*log(n))
BICe
A <- AIC(mle)
AICce <- A+(12/(n-3))
AICce
HQICe<- lle+(2*log(log(n)))
HQICe
k <- 3
CAICe <- A + (2*k*(k+1))/(n-k-1)
CAICe
############################
n <- length(datas25)
x <- datas25
llfd <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   beta  <- param[ 3 ]
   return(n*log(alpha-1)-n*log(log(alpha))+n*log(theta*beta)+(beta-1)*sum(log(x))-theta*sum(x^beta)-sum(log(1+(alpha-1)*(1-exp(-theta*x^beta))))
)
}
mean_x <- mean(datas25)
sd_x <- sd(datas25)
var_x <- var(datas25)
starts <- c(theta = 2.2*sd_x, alpha = 2.2*mean_x, beta = 2.2*var_x)
mld <- maxLik(llfd, start = starts, method = "BFGS" )
summary(mld)
AIC(mld)
lld <- -2*logLik(mld)
lld
BICd <- lld+(2*log(n))
BICd
A <- AIC(mld)
AICcd <- A+(12/(n-3))
AICcd
HQICd<- lld+(2*log(log(n)))
HQICd
k <- 3
CAICd <- A + (2*k*(k+1))/(n-k-1)
CAICd

##############################################################################
### Main result for The Alpha Power of the Power Ailamujia distribution
####################################################################################################
n <- length(datas25)
x <- datas25
llfc <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   beta  <- param[ 3 ]
   return(n*log(log(alpha)/(alpha-1))+2*n*log(2*alpha)+n*log(beta)+(2*beta-1)*sum(log(x))-2*alpha*sum(x^beta)+(n-(1+(2*alpha)*sum(x^beta))*exp(-2*alpha*sum(x^beta))*log(theta))
)
}
mean_x <- mean(datas25)
sd_x <- sd(datas25)
var_x <- var(datas25)
starts <- c(theta = 5.3*sd_x, alpha = 6.8*mean_x, beta = 3.98*var_x)
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
### Main result for The Alpha Power Transformed Copoun distribution
####################################################################################################
n <- length(datas25)
x <- datas25
llg <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   beta  <- param[ 3 ]
   return(n*(log(log(alpha))-log(alpha-1)+log(alpha)+2*log(theta)-log(beta+theta))+sum(log((beta*theta^2*x^3+6)/6))-theta*x-log(alpha)*sum(log((beta*theta^3*x^3+3*beta*theta^2*x^2+6*beta*theta*x+6*(beta+theta))/6*(beta+theta)))*exp(-(theta*x))
)
}
mean_x <- mean(datas25)
sd_x <- sd(datas25)
var_x <-var(datas25)
starts <- c(theta = 0.77*sd_x, alpha = 0.8*mean_x, beta = 0.98*var_x)
mlg <- maxLik(llg, start = starts, method = "BFGS" )
summary(mlg)
AIC(mlg)
llg <- -2*logLik(mlg)
llg
BICg <- llg+(2*log(n))
BICg
HQICg<- llg+(2*log(log(n)))
HQICg
k <- 3
Ag <- AIC(mlg)
CAICg <- Ag + (2*k*(k+1))/(n-k-1)
CAICg

#####################################
n <- length(datas26)
x <- datas26
llf <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   beta  <- param[ 3 ]
   return(n*(log(log(alpha))-log(alpha-1)+log(alpha)+2*log(theta)-log(beta+theta))+sum(log((beta*theta^2*x^3+6)/6))-theta*x-log(alpha)*sum(log((beta*theta^3*x^3+3*beta*theta^2*x^2+6*beta*theta*x+6*(beta+theta))/6*(beta+theta)))*exp(-(theta*x))
)
}
mean_x <- mean(datas26)
sd_x <- sd(datas26)
var_x <-var(datas26)
starts <- c(theta = 1.02*sd_x, alpha = 1.02*mean_x, beta = 1.04*var_x)
mlf <- maxLik(llf, start = starts, method = "BFGS" )
summary(mlf)
AIC(mlf)
llf <- -2*logLik(mlf)
llf
BICf <- llf+(2*log(n))
BICf
HQICf<- llf+(2*log(log(n)))
HQICf
k <- 2
Af <- AIC(mlf)
CAICf <- Af + (2*k*(k+1))/(n-k-1)
CAICf

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




