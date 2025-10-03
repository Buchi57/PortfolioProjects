####
Datasets <- read_excel("C:/Users/DELL/Desktop/Working Papers/Buchi Research/Main/Datasets.xlsx")
View(Datasets)
dat1 <- Datasets$`Dataset 10`
dat1 <- dat1 %>%
  na.omit()
n <- length(dat1)
x <- dat1
llf <- function( param ) {
  theta <- param[ 1 ]
  alpha <- param[ 2 ]
  return(2*n*log(theta) - n*log(alpha+theta)+sum(log(1+(alpha*theta^2*x^3)/(6*(alpha+theta))))-theta*sum(x)
  )
}
mean_x <- mean(dat1)
sd_x <- sd(dat1)
starts <- c(theta = 0.1*sd_x, alpha = mean_x/10)
ml <- maxLik(llf, start = starts, method = "BFGS" )
summary(ml)
coef(ml)
stdEr(ml)
AIC(ml)
estfun(ml)