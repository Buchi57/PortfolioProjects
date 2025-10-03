##############################################################################
### Main result for The proposed new Xgamma distribution
####################################################################################################
library(dplyr)
library(maxLik)

n <- length(datas25)
x <- datas25
llf <- function( param ) {
   theta <- param[ 1 ]
   return(2*n*log(theta) - n*log(1+theta)+sum(log(1+((theta^2*x^3)/6)))-theta*sum(x)
)
}
mean_x <- mean(datas25)
sd_x <- sd(datas25)
starts <- c(theta = 0.2*sd_x)
starts <- c(theta = mean_x/5)
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
### Main result for The Xgamma distribution
####################################################################################################
library(dplyr)
library(maxLik)

n <- length(datas27)
x <- datas27
llfc <- function( param ) {
   theta <- param[ 1 ]
   return(2*n*log(theta) - n*log(1+theta)+sum(log(1+((theta*x^2)/2)))-theta*sum(x)
)
}
mean_x <- mean(datas27)
sd_x <- sd(datas27)
startsa <- c(theta = 0.1*sd_x)
startsa <- c(theta = mean_x/50)
mlc <- maxLik(llfc, start = startsa, method = "BFGS" )
summary(mlc)
AIC(mlc)
llc <- -2*logLik(mlc)
llc
BIC <- llc+(2*log(n))
BIC
A <- AIC(mlc)
AICc <- A+(12/(n-3))
AICc
HQIC<- llc+(2*log(log(n)))
HQIC
k <- 2
A <- AIC(mlc)
CAIC <- A + (2*k*(k+1))/(n-k-1)
CAIC
##############################################################################
### Main result for The Exponential distribution
####################################################################################################
library(dplyr)
library(maxLik)

n <- length(datas27)
x <- datas27
llfe <- function( param ) {
   theta <- param[ 1 ]
   return(n*log(theta)-theta*sum(x)
)
}
mean_x <- mean(datas27)
sd_x <- sd(datas27)
startse <- c(theta = 0.1*sd_x)
mle <- maxLik(llfe, start = startse, method = "BFGS" )
summary(mle)
AIC(mle)
lle <- -2*logLik(mle)
lle
BICe <- ll+(2*log(n))
BICe
A <- AIC(mle)
AICc <- A+(12/(n-3))
AICc
HQICe<- lle+(2*log(log(n)))
HQICe
k <- 2
A <- AIC(mle)
CAICe <- A + (2*k*(k+1))/(n-k-1)
CAICe

##############################################################################
### Main result for The Logarithmic Transformed Lindley (LoTL) distribution
####################################################################################################
library(dplyr)
library(maxLik)

n <- length(datas25)
x <- datas25
llfd <- function( param ) {
   theta <- param[ 1 ]
   return(n*log((theta^2)/(log(2*(theta+1))))-theta*sum(x)+sum (log(1+x))+sum(log(2-(1+theta+theta*x)*(exp(-theta*x))/(1+theta)))
)
}
mean_x <- mean(datas25)
sd_x <- sd(datas25)
startsd <- c(theta = 0.1*sd_x)
startsd <- c(theta = mean_x/50)
mld <- maxLik(llfd, start = startsd, method = "BFGS" )
summary(mld)
AIC(mld)
lld <- -2*logLik(mld)
lld
BIC <- lld+(2*log(n))
BIC
A <- AIC(mld)
AICc <- A+(12/(n-3))
AICc
HQICd<- lld+(2*log(log(n)))
HQICd
k <- 2
A <- AIC(mle)
CAICd <- A + (2*k*(k+1))/(n-k-1)
CAICd


##############################################################################
### Main result for The Logarithmic Transformed Lindley (LoTL) distribution
####################################################################################################
library(dplyr)
library(maxLik)

n <- length(datas25)
x <- datas25
llff <- function( param ) {
   theta <- param[ 1 ]
   return(sum(log(1+theta*x+x^2))-sum(log(1+theta*x+x^2))-sum (x)+(theta-1)*sum((log(1+x+x^2)/2)-()/())
)
}
mean_x <- mean(datas25)
sd_x <- sd(datas25)
startsf <- c(theta = 0.1*sd_x)
startsf <- c(theta = mean_x/50)
mld <- maxLik(llfd, start = startsf, method = "BFGS" )
summary(mlf)
AIC(mlf)
llf <- -2*logLik(mlf)
llf
BICf <- llf+(2*log(n))
BICf
A <- AIC(mlf)
AICc <- A+(12/(n-3))
AICc
HQICf<- llf+(2*log(log(n)))
HQICf
k <- 2
A <- AIC(mlf)
CAICf <- A + (2*k*(k+1))/(n-k-1)
CAICf





