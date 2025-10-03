data1 = c(195.0, 190.7, 186.6, 182.4, 178.2, 173.7, 169.0, 164.2,159.0, 154.0, 149.0, 144.1, 139.5, 135.4, 131.9, 128.8, 126.4, 
          124.7, 123.7, 123.3, 123.3, 123.7, 124.2, 124.7, 125.0, 125.2,125.1, 124.9, 124.5, 124.0, 123.4, 122.3, 120.9, 118.9, 116.5, 
          113.7, 110.9, 107.8, 104.8, 101.6, 98.6, 95.6, 92.8, 90.2, 87.9,85.9, 84.1, 82.7, 81.5, 80.5, 79.6, 78.7, 77.9, 76.9, 75.7)

# A NEW LIFETIME DISTRIBUTION PROPOSED - Probability density function.
pdf_New <- function(par,x){
  a = par[1]
  b = par[2]
  ((a^2/(b+a))*((6+b*a^2*x^3)/6)*exp(-a*x))
}

# A NEW LIFETIME DISTRIBUTION PROPOSED - Cumulative distribution function.
cdf_New <- function(par,x){
  a = par[1]
  b = par[2]
  (1-((6*(b+a))+((a^3*x^3)*b+(3*a^2*x^2)*b+(6*a*x)*b)*exp(-a*x)/(6*(b+a))))
}

set.seed(0)
resnew = goodness.fit(pdf = pdf_New, cdf = cdf_New, 
                        starts = c(-0.0011,-1.0012), data = data1, method = "PSO",
                        domain = c(0,Inf),mle = NULL, lim_inf = c(0,0),
                        lim_sup = c(-100,100), S = 5550, prop=0.1, N=50)
result_1$mle
x = seq(0, 6, length.out = 500)
hist(data1, probability = TRUE)
lines(x, pdf_expweibull(x, par = result_1$mle), col = "blue")
##############################################################################################################################
Datasets <- read_excel("C:/Users/DELL/Desktop/Working Papers/Buchi Research/Main/Datasets.xlsx")
View(Datasets)
datas1 <- Datasets$`Dataset 1`
datas1
# A NEW LIFETIME DISTRIBUTION PROPOSED I - Probability density function.
pdf_New1 <- function(par,x){
  a = par[1]
  b = par[2]
  ((a^2/(b+a))*((6+b*a^2*x^3)/6)*exp(-a*x))
}

# A NEW LIFETIME DISTRIBUTION PROPOSED I - Cumulative distribution function.
cdf_New1 <- function(par,x){
  a = par[1]
  b = par[2]
  (1-(1+(a^3*x^3*b+3*a^2*x^2*b+6*a*x*b)/(6*(b+a)))*exp(-a*x))
}

set.seed(0)
resnew1 = goodness.fit(pdf = pdf_New1, cdf = cdf_New1, 
                      starts = c(-0.0011,-1.0012), data = data1, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0,0),
                      lim_sup = c(-100,-100), S = 5550, prop=0.1, N=50)
result_1$mle
x = seq(0, 6, length.out = 500)
hist(carbone, probability = TRUE)
lines(x, pdf_expweibull(x, par = result_1$mle), col = "blue")
##############################################################################################################################
data1 = c(195.0, 190.7, 186.6, 182.4, 178.2, 173.7, 169.0, 164.2,159.0, 154.0, 149.0, 144.1, 139.5, 135.4, 131.9, 128.8, 126.4, 
          124.7, 123.7, 123.3, 123.3, 123.7, 124.2, 124.7, 125.0, 125.2,125.1, 124.9, 124.5, 124.0, 123.4, 122.3, 120.9, 118.9, 116.5, 
          113.7, 110.9, 107.8, 104.8, 101.6, 98.6, 95.6, 92.8, 90.2, 87.9,85.9, 84.1, 82.7, 81.5, 80.5, 79.6, 78.7, 77.9, 76.9, 75.7)

# Cubic Rank Transmuted Inverse Exponential Distribution - Probability density function.
pdf_Ne <- function(par,x){
  a = par[1]
  b = par[2]
  c = par[3]
  ((a^2/((b*c)+a))*(c+(b*x))*exp(-a*x))
}

# Cubic Rank Transmuted Inverse Exponential Distribution - Cumulative distribution function.
cdf_Ne <- function(par,x){
  a = par[1]
  b = par[2]
  c = par[3]
  (1+(1+((a*b*x)/((a*c)+b)))*exp(-a*x))
}

set.seed(0)
resnew = goodness.fit(pdf = pdf_New, cdf = cdf_New, 
                        starts = c(1,1,1), data = data1, method = "PSO", domain = c(0,1), lim_inf = c(0,0,0),
              lim_sup = c(10,10,10), S = 200, prop = 0.1, N = 50)


resnew$mle
x = seq(0, 6, length.out = 500)
hist(data1, probability = TRUE)
lines(x, pdf_expweibull(x, par = result_1$mle), col = "blue")

#########################################################################################
# Kumaraswamy Beta - Probability density function.
pdf_kwbeta <- function(par,x){
 beta = par[1]
 a = par[2]
 alpha = par[3]
 b = par[4]
 (a*b*x^(alpha-1)*(1-x)^(beta-1)*(pbeta(x,alpha,beta))^(a-1)*
 (1-pbeta(x,alpha,beta)^a)^(b-1))/beta(alpha,beta)
}
# Kumaraswamy Beta - Cumulative distribution function.
cdf_kwbeta <- function(par,x){
 beta = par[1]
 a = par[2]
 alpha = par[3]
 b = par[4]
 1 - (1 - pbeta(x,alpha,beta)^a)^b
}
set.seed(0)
random_data3 = rbeta(150,2,2.2)

t= goodness.fit(pdf = pdf_kwbeta, cdf = cdf_kwbeta, starts = c(1,1,1,1),
 data = data1, method = "PSO", domain = c(0,1), lim_inf = c(0,0,0,0),
 lim_sup = c(10,10,10,10), S = 200, prop = 0.1, N = 40)

t$mle
x = seq(0, 6, length.out = 500)
hist(carbone, probability = TRUE)
lines(x, pdf_expweibull(x, par = result_1$mle), col = "blue")
#############################################################################

data(carbone)
# Exponentiated Weibull - Probability density function.
pdf_expweibull <- function(par,x){
beta = par[1]
c = par[2]
a = par[3]
a * beta * c * exp(-(beta*x)^c) * (beta*x)^(c-1) * (1 - exp(-(beta*x)^c))^(a-1)
}
# Exponentiated Weibull - Cumulative distribution function.
cdf_expweibull <- function(par,x){
beta = par[1]
c = par[2]
a = par[3]
(1 - exp(-(beta*x)^c))^a
}
set.seed(0)
result_1 = goodness.fit(pdf = pdf_expweibull, cdf = cdf_expweibull,
starts = c(1,1,1), data = carbone, method = "PSO",
domain = c(0,Inf),mle = NULL, lim_inf = c(0,0,0),
lim_sup = c(2,2,2), S = 250, prop=0.1, N=50)

result_1$mle

x = seq(0, 6, length.out = 500)
hist(carbone, probability = TRUE)
lines(x, pdf_expweibull(x, par = result_1$mle), col = "blue")

#################################################################################################################
# A NEW LIFETIME DISTRIBUTION I - Probability density function.
pdf_New1 <- function(par,x){
  a = par[1]
  b = par[2]
  ((a^2/(a+(2*b)+6))*((1+a*x^2+a^2*x^3))*exp(-a*x))
}

# A NEW LIFETIME DISTRIBUTION I - Cumulative distribution function.
cdf_New1 <- function(par,x){
  a = par[1]
  b = par[2]
  (1+(a^3*x^3+4*a^2*x^2+8*a^2*x+8+a)*exp(-a*x)/((2*b)+a+6))
}

set.seed(0)
resnew1 = goodness.fit(pdf = pdf_New1, cdf = cdf_New1, 
                        starts = c(-11199.11,-2990.11), data = data1, method = "PSO",
                        domain = c(0,Inf),mle = NULL, lim_inf = c(0,0,0),
                        lim_sup = c(12,15,15), S = 250, prop=0.1, N=50)
result_1$mle
x = seq(0, 6, length.out = 500)
hist(carbone, probability = TRUE)
lines(x, pdf_expweibull(x, par = result_1$mle), col = "blue")

