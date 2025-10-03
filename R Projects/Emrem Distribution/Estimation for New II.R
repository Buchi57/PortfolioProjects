##############################################################################################################################
Datasets <- read_excel("C:/Users/DELL/Desktop/Working Papers/Buchi Research/Main/Datasets.xlsx")
View(Datasets)
datas1 <- Datasets$`Dataset 1`
datas1
datas1 <- datas1[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas1 <- datas1 %>%
  na.omit()

# A NEW LIFETIME DISTRIBUTION II - Probability density function.
pdf_New2 <- function(par,x){
  a = par[1]
  b = par[2]
  ((a^2/(a+(2*b)+6))*((1+a*x^2+a^2*x^3))*exp(-a*x))
}

# A NEW LIFETIME DISTRIBUTION II - Cumulative distribution function.
cdf_New2 <- function(par,x){
  a = par[1]
  b = par[2]
  (1+(a^3*x^3+4*a^2*x^2+8*a^2*x+8+a)*exp(-a*x)/((2*b)+a+6))
}

set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas1, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
resnew2$mle
x = seq(0, 6, length.out = 500)
hist(datas1, probability = TRUE)
lines(x, pdf_New2(x, par = resnew2$mle), col = "blue")
##############################################################################################################################
datas2 <- Datasets$`Dataset 2`
datas2
datas2 <- datas2[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas2 <- datas2 %>%
  na.omit()
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas2, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas3 <- Datasets$`Dataset 3`
datas3
datas3 <- datas3[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas3 <- datas3 %>%
  na.omit()
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas3, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas4 <- Datasets$`Dataset 4`
datas4
datas4 <- datas4[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas4 <- datas4 %>%
  na.omit()
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas4, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas5 <- Datasets$`Dataset 5`
datas5
datas5 <- datas5[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas5 <- datas5 %>%
  na.omit()
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas5, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas6 <- Datasets$`Dataset 6`
datas6
datas6 <- datas6[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas6 <- datas6 %>%
  na.omit()
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas6, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas7 <- Datasets$`Dataset 7`
datas7
datas7 <- datas7[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas7 <- datas7 %>%
  na.omit()
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas7, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas8 <- Datasets$`Dataset 8`
datas8
datas8 <- datas8[!is.na(datas1)]
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas8, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas9 <- Datasets$`Dataset 9`
datas9
datas9 <- datas9[!is.na(datas1)]

set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas9, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas10 <- Datasets$`Dataset 10`
datas10
datas10 <- datas10[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas10 <- datas10 %>%
  na.omit()
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas10, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas11 <- Datasets$`Dataset 11`
datas11
datas11 <- datas11[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas11 <- datas11 %>%
  na.omit()
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas11, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas12 <- Datasets$`Dataset 12`
datas12
datas12 <- datas12[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas12 <- datas12 %>%
  na.omit()
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas12, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas13 <- Datasets$`Dataset 13`
datas13
datas13 <- datas13[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas13 <- datas13 %>%
  na.omit()
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas13, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas14 <- Datasets$`Dataset 14`
datas14
datas14 <- datas14[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas14 <- datas14 %>%
  na.omit()
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas14, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas15 <- Datasets$`Dataset 15`
datas15
datas15 <- datas15[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas15 <- datas15 %>%
  na.omit()
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas15, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################
datas16 <- Datasets$`Dataset 16`
datas16
datas16 <- datas16[!is.na(datas1)]

# Or using 'dplyr' 
library(dplyr)
datas16 <- datas16 %>%
  na.omit()
set.seed(0)
resnew2 = goodness.fit(pdf = pdf_New2, cdf = cdf_New2, 
                      starts = c(0.003,0.02), data = datas16, method = "PSO",
                      domain = c(0,Inf),mle = NULL, lim_inf = c(0.1,0.1),
                      lim_sup = c(0.5,10), S = 5550, prop=0.1, N=50)
##############################################################################################################################

