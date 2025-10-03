data1 = c(195.0, 190.7, 186.6, 182.4, 178.2, 173.7, 169.0, 164.2,159.0, 154.0, 149.0, 144.1, 139.5, 135.4, 131.9, 128.8, 126.4, 
          124.7, 123.7, 123.3, 123.3, 123.7, 124.2, 124.7, 125.0, 125.2,125.1, 124.9, 124.5, 124.0, 123.4, 122.3, 120.9, 118.9, 116.5, 
          113.7, 110.9, 107.8, 104.8, 101.6, 98.6, 95.6, 92.8, 90.2, 87.9,85.9, 84.1, 82.7, 81.5, 80.5, 79.6, 78.7, 77.9, 76.9, 75.7)
library(readxl)
dataset <- read_excel(NULL)
View(dataset)
x <- datas8
##############################################################################
###The proposed new distribution
####################################################################################################
data1 = c(195.0, 190.7, 186.6, 182.4, 178.2, 173.7, 169.0, 164.2,159.0, 154.0, 149.0, 144.1, 139.5, 135.4, 131.9, 128.8, 126.4, 
          124.7, 123.7, 123.3, 123.3, 123.7, 124.2, 124.7, 125.0, 125.2,125.1, 124.9, 124.5, 124.0, 123.4, 122.3, 120.9, 118.9, 116.5, 
          113.7, 110.9, 107.8, 104.8, 101.6, 98.6, 95.6, 92.8, 90.2, 87.9,85.9, 84.1, 82.7, 81.5, 80.5, 79.6, 78.7, 77.9, 76.9, 75.7)
n <- length(datas8)
x <- datas8
llf <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   return(2*n*log(theta) - n*log(alpha+theta)+sum(log(1+(alpha*theta^2*x^3)/(6*(alpha+theta))))-theta*sum(x)
)
}
mean_x <- mean(datas8)
sd_x <- sd(datas8)
starts <- c(theta = 0.1*sd_x, alpha = mean_x/10)
ml <- maxLik(llf, start = starts, method = "BFGS" )
summary(ml)
coef(ml)
stdEr(ml)
AIC(ml)

##############################################################################
##The proposed new distribution II
#########################################################################
data1 = c(195.0, 190.7, 186.6, 182.4, 178.2, 173.7, 169.0, 164.2,159.0, 154.0, 149.0, 144.1, 139.5, 135.4, 131.9, 128.8, 126.4, 
          124.7, 123.7, 123.3, 123.3, 123.7, 124.2, 124.7, 125.0, 125.2,125.1, 124.9, 124.5, 124.0, 123.4, 122.3, 120.9, 118.9, 116.5, 
          113.7, 110.9, 107.8, 104.8, 101.6, 98.6, 95.6, 92.8, 90.2, 87.9,85.9, 84.1, 82.7, 81.5, 80.5, 79.6, 78.7, 77.9, 76.9, 75.7)
total(data1)
n <- length(datas5)
x <- datas5
llf2 <- function( param ) {
   theta <- param[ 1 ]
   alpha <- param[ 2 ]
   return(2*n*log(theta) - n*log(2*alpha+theta+6)+sum(log(1+theta*x^2+theta^2*x^3))-theta*sum(x)
)
}
mean_x <- mean(datas5)
sd_x <- sd(datas5)
starts <- c(theta = 0.1*sd_x, alpha = mean_x/10)
ml1 <- maxLik(llf2, start = starts, method = "SANN" )
summary(ml1)

theta_grid <- seq(0, 1, length = 100)
alpha_grid <- seq(0, 10, length = 100)

llf1 <- function(param) {
  
  return(ifelse(any(is.infinite(
                         2*n*log(theta) - 
                           n*log(2*alpha+theta+6) + 
                           sum(log(1+theta*x^2+theta^2*x^3)) - 
                           theta*sum(x)
                          )), 
               -1e10, 
               2*n*log(theta) - 
                 n*log(2*alpha+theta+6) + 
                 sum(log(1+theta*x^2+theta^2*x^3)) - 
                 theta*sum(x)
               )           
        )    
}
theta_moment <- (2*n)/(sum(log(1 + theta*x^2 + theta^2*x^3)))
alpha_moment <- mean(1/(2*alpha + theta + 6))
starts <- c(theta = theta_moment, alpha = alpha_moment)
ml1 <- maxLik(llf1, start = starts, method = "SANN" )
summary(ml1)
##################
theta <- 0.1 * sd(datas3)
# theta = 9.01

alpha <- mean(datas3)/10  
# alpha = 57.5

starts <- c(theta = theta, alpha = alpha)
###############
theta_start <- runif(1, 0, 500)
alpha_start <- runif(1, 0, 500)

starts <- c(theta_start, alpha_start)
###############
theta_grid <- seq(0, 50, len = 100) 
alpha_grid <- seq(0, 500, len = 100)

ll_grid <- outer(theta_grid, alpha_grid, function(t, a) {
  llf1(c(t,a))
})

starts <- c(theta = theta_grid[which.max(ll_grid)],
            alpha = alpha_grid[which.max(ll_grid)])
############################
starts1 <- c(runif(1,0,50), runif(1,0,500))
starts2 <- c(runif(1,0,50), runif(1,0,500))

fits <- list(optim(starts1, llf2), optim(starts2, llf1))

best_fit <- fits[[which.max(sapply(fits, function(f) f$value))]]
starts <- best_fit$par # Use parameters from best fit
#########################
# Initial estimates 
starts <- c(theta = sd(x), alpha = mean(x))

# Refine estimates
starts <- optim(starts, llf1)

# Further refine     
starts <- brute.force(starts)

# Final optimization 
ml_estimates <- optim(starts, llf1)

ll_val <- llf1(starts)
if(any(is.nan(ll_val)) | any(is.infinite(ll_val))) {
  print("Got NaN or Inf result!")
}
ml1 <- maxLik(llf1, start = starts )
summary(ml1)

