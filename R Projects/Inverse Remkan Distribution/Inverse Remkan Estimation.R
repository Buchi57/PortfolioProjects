##############################################################################
### Main result for The proposed Inverse Remkan distribution (DATA I)
####################################################################################################
library(dplyr)
library(maxLik)

datas11 <- datas11 %>%
  na.omit()
n <- length(datas20)
x <- datas20
llf <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   return(2*n*log(theta) - n*log(theta+2*alpha+6)+sum(log((x^3+alpha*theta*x+theta^2)/x^5))-theta*sum(1/x)
)
}
mean_x <- mean(datas20)
sd_x <- sd(datas20)
starts <- c(theta = 0.2*sd_x, alpha = mean_x/30)
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
n <- length(datas20)
x <- datas20
llfe <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   return(4*n*log(theta) - n*log(alpha*theta^3+6)+sum(log(alpha+x^3)-theta*sum(x)
)
}
mean_x <- mean(datas20)
sd_x <- sd(datas20)
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
n <- length(datas20)
x <- datas20
llfc <- function( param ) {
   theta <- param[ 1 ]
     return(3*n*log(theta) - n*log(theta^2+2)+sum(log((1+x^2)/x^4))-theta*sum(1/x)
)
}
mean_x <- mean(datas20)
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
n <- length(datas20)
x <- datas20
llfd <- function( param ) {
   theta <- param[ 1 ]
     return(5*n*log(theta) - n*log(theta^4+24)+sum(log((1+x^4)/x^6))-theta*sum(1/x)
)
}
mean_x <- mean(datas20)
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
n <- length(datas20)
x <- datas20
llfg <- function( param ) {
   theta <- param[ 1 ]
     return(2*n*log(theta) - n*log(1+theta)+sum(log((1+x^2)/x^4))-theta*sum(1/x)
)
}
mean_x <- mean(datas20)
starts <- c(theta = 0.1*mean_x)
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
### Data II
####################################################################################################
library(dplyr)
library(maxLik)

n <- length(datas21)
x <- datas21
llf <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   return(2*n*log(theta) - n*log(theta+2*alpha+6)+sum(log((x^3+alpha*theta*x+theta^2)/x^5))-theta*sum(1/x)
)
}
mean_x <- mean(datas21)
sd_x <- sd(datas21)
starts <- c(theta = 0.1*sd_x, alpha = mean_x/60)
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
n <- length(datas21)
x <- datas21
llfe <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   return(4*n*log(theta) - n*log(alpha*theta^3+6)+sum(log(alpha+x^3)-theta*sum(x)
)
}
mean_x <- mean(datas21)
sd_x <- sd(datas21)
starts <- c(theta = 0.1*sd_x, alpha = mean_x/40)
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
n <- length(datas21)
x <- datas21
llfc <- function( param ) {
   theta <- param[ 1 ]
     return(3*n*log(theta) - n*log(theta^2+2)+sum(log((1+x^2)/x^4))-theta*sum(1/x)
)
}
mean_x <- mean(datas21)
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
n <- length(datas21)
x <- datas21
llfd <- function( param ) {
   theta <- param[ 1 ]
     return(5*n*log(theta) - n*log(theta^4+24)+sum(log((1+x^4)/x^6))-theta*sum(1/x)
)
}
mean_x <- mean(datas21)
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
n <- length(datas21)
x <- datas21
llfg <- function( param ) {
   theta <- param[ 1 ]
     return(2*n*log(theta) - n*log(1+theta)+sum(log((1+x^2)/x^4))-theta*sum(1/x)
)
}
mean_x <- mean(datas21)
starts <- c(theta = 0.1*mean_x)
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

