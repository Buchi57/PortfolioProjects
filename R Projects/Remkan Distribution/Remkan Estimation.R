##############################################################################
### Main result for The proposed new Remkan distribution
####################################################################################################
library(dplyr)
library(maxLik)

datas11 <- datas11 %>%
  na.omit()
n <- length(datas14)
x <- datas14
llf <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   return(2*n*log(theta) - n*log(6+2*alpha+theta)+sum(log(1+alpha*theta*x^2+theta^2*x^3))-theta*sum(x)
)
}
mean_x <- mean(datas14)
sd_x <- sd(datas14)
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
 
x = seq(datas11)
hist(datas11, probability = TRUE)
lines(x, y, col = "blue")


x = seq(0,ceiling(max(datas11)), length.out = 500)
hist(datas11, probability = TRUE)
lines(x, ml$estimate, col = "blue")


# x vector from min to max of data 
x <- seq(min(datas11), max(datas11), length.out = length(datas11))

# Extract fitted values from model  
y <- fitted(ml)

# Separate histogram 
hist(datas11, probability = TRUE)
lines(x, y, col = "blue", type = "l")

# Separate plot of fitted values     
plot(x, y, col = "blue", type = "l")
##############################################################################
### Main result for The Samade distribution
####################################################################################################
library(dplyr)
library(maxLik)

datas11 <- datas11 %>%
  na.omit()
n <- length(datas14)
x <- datas14
llfc <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   return(4*n*log(theta)- n*log(theta^2+6*alpha)+sum(log(theta+alpha^2*x^3))-theta*sum(x)
)
}
mean_x <- mean(datas14)
sd_x <- sd(datas14)
starts <- c(theta = 0.1*sd_x, alpha = mean_x/10)
mlc <- maxLik(llfc, start = starts, method = "BFGS" )
summary(mlc)
AIC(mlc)
llc <- -2*logLik(mlc)
llc
BIC <- llc+(2*log(n))
BIC
HQIC<- llc+(2*log(log(n)))
HQIC
k <- 2
A <- AIC(mlc)
CAIC <- A + (2*k*(k+1))/(n-k-1)
CAIC
##############################################################################
### Main result for The Samade distribution
####################################################################################################
library(dplyr)
library(maxLik)

datas14 <- datas14 %>%
  na.omit()
n <- length(datas14)
x <- datas14
llfc <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   return(5*n*log(theta)- 2*n*log(alpha*theta^5+theta^3+24)+sum(log(theta*x^2+2*alpha*theta+2*x^4))-theta*sum(x)
)
}
mean_x <- mean(datas14)
sd_x <- sd(datas14)
starts <- c(theta = 0.1*sd_x, alpha = mean_x/10)
mlc <- maxLik(llfc, start = starts, method = "BFGS" )
summary(mlc)
AIC(mlc)
llc <- -2*logLik(mlc)
llc
BIC <- llc+(2*log(n))
BIC
HQIC<- llc+(2*log(log(n)))
HQIC
k <- 2
A <- AIC(mlc)
CAIC <- A + (2*k*(k+1))/(n-k-1)
CAIC



##############################################################################
### Main result for The proposed new distribution
####################################################################################################
datas12 <- c(0.040, 1.866, 2.385, 3.443, 0.301, 1.876, 2.481, 3.467, 0.309, 1.899, 2.610, 3.478, 0.557, 1.911, 2.625, 3.578, 0.943, 1.912, 2.632, 3.595, 1.070, 1.914, 2.646, 3.699, 1.124, 1.981, 2.661, 3.779, 1.248, 2.010, 2.688, 3.924, 1.281, 2.038, 2.823, 4.035, 1.281, 2.085, 2.890, 4.121, 1.303, 2.089, 2.902, 4.167, 1.432, 2.097, 2.934, 4.240, 1.480, 2.135, 2.962, 4.255, 1.505, 2.154, 2.964, 4.278, 1.506, 2.190, 3.000, 4.305, 1.568, 2.194, 3.103, 4.376, 1.615, 2.223, 3.114, 4.449, 1.619, 2.224, 3.117, 4.485, 1.652, 2.229, 3.166, 4.570, 1.652, 2.300, 3.344, 4.602, 1.757, 2.324, 3.376, 4.663)
datas12 <- datas12 %>%
  na.omit()
n <- length(datas12)
x <- datas12
llf <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   return(2*n*log(theta) - n*log(alpha+theta)+sum(log(1+(alpha*theta^2*x^3)/(6)))-theta*sum(x)
)
}
mean_x <- mean(datas12)
sd_x <- sd(datas12)
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




