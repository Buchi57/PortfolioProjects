####################################################################################
###Mean: where a is theta, and b is alpha
##########################################################
a <- 3
t <- (a+4)  
p <-(a(1+a))
mean <- t/p
mean

####################################################################################
### Standard deviation
##########################################################
a <- 3
t <- (a^2+16*a*b+12*b^2+84*a+96*b+144)
p <-(a^2*(a+2*b+6)^2)
sd <- t/p
sqrt(sd)

####################################################################################
### Coefficient of variation
##########################################################
a <- 3
t <- sqrt((a^2+14*a+4))
p <-(a+4)
cv <- t/p
sqrt(cv)


####################################################################################
###coefficient of skewness
##########################################################
a <- 0.5
t <- 2*(124+51*a-84*a^2-41*a^3-3*a^4)  
p <-((a^2+14*a+4)^(3/2))
sk <- t/p
sk

####################################################################################
###Index of dispersion
##########################################################
a <- 3
t <- (a^2+14*a+14)  
p <- (a*(a+1)*(a+4))
Id <- t/p
Id


####################################################################################
###coefficient of Kurtosis
##########################################################
a= 5
kurt = 3*(4*a^5+75*a^4+536*a^3+760*a^2-240*a-616)/(a^2+14*a+4)^2
kurt

###################################################################################################################################
# Load results from BB.docx calculations
source("BB.docx") 

# Create a data frame to store results
results <- data.frame(
  statistic = character(),
  value = double()
)

# Add mean result
results <- rbind(results, c("Mean", mean))

# Add standard deviation result 
results <- rbind(results, c("Standard Deviation", sqrt(sd)))

.0Add coefficient of variation result
results <- rbind(results, c("Coefficient of Variation", sqrt(cv))) 

# Add coefficient of skewness result
results <- rbind(results, c("Coefficient of Skewness", sk))

# Add coefficient of kurtosis result  
results <- rbind(results, c("Coefficient of Kurtosis", kurt))

# Print results table
print(results)
#########################################################################################
## Function to calculate and tabulate statistics measures
######################################################################################################
calc_stats <- function(a) {

  t1 <- (a+4)  
  p1 <-(a*(1+a))
  mean <- t1/p1


  t2 <- (a^2+16*a*b+12*b^2+84*a+96*b+144)
  p2 <- (a^2*(a+2*b+6)^2) 
  sd <- t2/p2

  t3 <- sqrt((a^2+14*a+4))
  p3 <- (a+4)
  cv <- t3/p3

  t4 <- 2*(124+51*a-84*a^2-41*a^3-3*a^4)  
  p4 <-((a^2+14*a+4)^(3/2))
  sk <- t4/p4

  kurt = 3*(4*a^5+75*a^4+536*a^3+760*a^2-240*a-616)/(a^2+14*a+4)^2

  t5 <- (a^2+14*a+14)  
  p5 <- (a*(a+1)*(a+4))
  Id <- t5/p5

  
  out <- data.frame(
    statistic = c("Mean", "SD", "CV","Skewness", "Kurtosis","Index of Dispersion"), 
    value = c(mean, sqrt(sd), sqrt(cv),sk, kurt, Id)
  )

}

# Run function for different a values
results <- cbind(
  calc_stats(1.1),
  calc_stats(1.3), 
  calc_stats(1.0),
  calc_stats(1.7)
)

print(results)
# Run function for different a, b values
results <- cbind(
  calc_stats(1.5,2.0),
  calc_stats(1.5,2.1), 
  calc_stats(1.5,2.2),
  calc_stats(1.5,2.3)
)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(1.5,0.4),
  calc_stats(1.5,0.6), 
  calc_stats(1.5,0.8),
  calc_stats(1.5,0.9)
)

print(results)

#########################################################################################
## Function to calculate and tabulate Individual measures (CV)
######################################################################################################
calc_stats <- function(a, b) {

  t1<- sqrt((144*a^2+684*a*b+11*b^2))
  p1<- (24*a*b+a)
  cv <- t1/p1


  out <- data.frame(
    statistic = c("CV"), 
    value = c( sqrt(cv))
  )

}

# Run function for different a, b values
results <- cbind(
  calc_stats(0.2,0.2),
  calc_stats(0.2,0.5), 
  calc_stats(0.2,1.0),
  calc_stats(0.2,2.0),
  calc_stats(0.2,3.0),
  calc_stats(0.2,4.0)

)

print(results)
# Run function for different a, b values
results <- cbind(
  calc_stats(0.5,0.2),
  calc_stats(0.5,0.5), 
  calc_stats(0.5,1.0),
  calc_stats(0.5,2.0),
  calc_stats(0.5,3.0),
  calc_stats(0.5,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(1,0.2),
  calc_stats(1,0.5), 
  calc_stats(1,1.0),
  calc_stats(1,2.0),
  calc_stats(1,3.0),
  calc_stats(1,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(2,0.2),
  calc_stats(2,0.5), 
  calc_stats(2,1.0),
  calc_stats(2,2.0),
  calc_stats(2,3.0),
  calc_stats(2,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(3,0.2),
  calc_stats(3,0.5), 
  calc_stats(3,1.0),
  calc_stats(3,2.0),
  calc_stats(3,3.0),
  calc_stats(3,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(4,0.2),
  calc_stats(4,0.5), 
  calc_stats(4,1.0),
  calc_stats(4,2.0),
  calc_stats(4,3.0),
  calc_stats(4,4.0)

)

print(results)
########################################
b<-seq(0,5)
a1=0.2
t1<- sqrt((144*a1^2+684*a1*b+11*b^2)
p1<- (24*a1*b+a1)
cv1<- t1/p1
a2=0.5
t2<- sqrt((144*a2^2+684*a2*b+11*b^2))
p2<- (24*a2*b+a2)
cv2<- t2/p2
a3=1
t3<- sqrt((144*a3^2+684*a3*b+11*b^2))
p3<- (24*a3*b+a3)
cv3<- t3/p3
a4=0.2
t4<- sqrt((144*a4^2+684*a4*b+11*b^2))
p4<- (24*a4*b+a4)
cv4<- t4/p4
a5=0.4
t5<- sqrt((144*a5^2+684*a5*b+11*b^2))
p5<- (24*a5*b+a5)
cv5<- t5/p5

plot(b,cv5,type="l",col="red",ylab="CV",pch="*")
lines(b,cv4,type="l",col="blue")
lines(b,cv3,type="l",col="green")
lines(b,cv1,type="l",col="black")
lines(b,cv2,type="l",col="orange")
legend("bottomright",legend=c(expression(paste(eta==0.85,","~phi=="780.4")),expression(paste(eta==0.09,","~phi=="820.7")),expression(paste(eta==0.06,","~phi=="620.1")),expression(paste(eta==0.08,","~phi=="840.30")),expression(paste(eta==0.364,","~phi=="390.2"))),col=c("red","blue","green","black","purple"),lty=1:1:1:1:2,cex=0.6)


#########################################################################################
## Function to calculate and tabulate Individual measures (Skewness)
######################################################################################################
calc_stats <- function(a, b) {

  t4 <- (182*a^2+648*a^2*b+23436*a^2*b+172*b^3)  
  p4 <-((144*a^2+684*a*b+11*b^2)*(144*a^2+684*a*b+11*b^2)^(3/2))
  sk <- t4/p4


  out <- data.frame(
    statistic = c("sk"),
    value = c(sk)
  )

}

# Run function for different a, b values
results <- cbind(
  calc_stats(0.2,0.2),
  calc_stats(0.2,0.5), 
  calc_stats(0.2,1.0),
  calc_stats(0.2,2.0),
  calc_stats(0.2,3.0),
  calc_stats(0.2,4.0)

)

print(results)
# Run function for different a, b values
results <- cbind(
  calc_stats(0.5,0.2),
  calc_stats(0.5,0.5), 
  calc_stats(0.5,1.0),
  calc_stats(0.5,2.0),
  calc_stats(0.5,3.0),
  calc_stats(0.5,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(1,0.2),
  calc_stats(1,0.5), 
  calc_stats(1,1.0),
  calc_stats(1,2.0),
  calc_stats(1,3.0),
  calc_stats(1,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(2,0.2),
  calc_stats(2,0.5), 
  calc_stats(2,1.0),
  calc_stats(2,2.0),
  calc_stats(2,3.0),
  calc_stats(2,4.0)
)
print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(3,0.2),
  calc_stats(3,0.5), 
  calc_stats(3,1.0),
  calc_stats(3,2.0),
  calc_stats(3,3.0),
  calc_stats(3,4.0)
)
print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(4,0.2),
  calc_stats(4,0.5), 
  calc_stats(4,1.0),
  calc_stats(4,2.0),
  calc_stats(4,3.0),
  calc_stats(4,4.0)

)

print(results)
#########################################################################################
## Function to calculate and tabulate Individual measures (Kurtosis)
######################################################################################################
calc_stats <- function(a, b) {

kurt = (4389*a^4+741312*a*b^3+789696*a^2*b^2+93312*b^4+985608*a^3*b)/(20736*a^4+196992*a^3*b+471024*a^2*b^2+15048*a*b^3+121*b^4)

  out <- data.frame(
    statistic = c("kurt"),
    value = c(kurt)
  )

}

# Run function for different a, b values
results <- cbind(
  calc_stats(0.2,0.2),
  calc_stats(0.2,0.5), 
  calc_stats(0.2,1.0),
  calc_stats(0.2,2.0),
  calc_stats(0.2,3.0),
  calc_stats(0.2,4.0)
)
print(results)
# Run function for different a, b values
results <- cbind(
  calc_stats(0.5,0.2),
  calc_stats(0.5,0.5), 
  calc_stats(0.5,1.0),
  calc_stats(0.5,2.0),
  calc_stats(0.5,3.0),
  calc_stats(0.5,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(1,0.2),
  calc_stats(1,0.5), 
  calc_stats(1,1.0),
  calc_stats(1,2.0),
  calc_stats(1,3.0),
  calc_stats(1,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(2,0.2),
  calc_stats(2,0.5), 
  calc_stats(2,1.0),
  calc_stats(2,2.0),
  calc_stats(2,3.0),
  calc_stats(2,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(3,0.2),
  calc_stats(3,0.5), 
  calc_stats(3,1.0),
  calc_stats(3,2.0),
  calc_stats(3,3.0),
  calc_stats(3,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(4,0.2),
  calc_stats(4,0.5), 
  calc_stats(4,1.0),
  calc_stats(4,2.0),
  calc_stats(4,3.0),
  calc_stats(4,4.0)

)

print(results)

#########################################################################################
## Function to calculate and tabulate Individual measures (Index of Dispersion)
######################################################################################################
calc_stats <- function(a, b) {

  t5 <- (a^2+a*(84+16*b)+12*b^2+96*b+144)  
  p5 <- (a*(a+2*b+6)*(a+6*b+24))
  Id <- t5/p5

  out <- data.frame(
    statistic = c("Id"), 
    value = c( Id)
  )

}

# Run function for different a, b values
results <- cbind(
  calc_stats(0.2,0.2),
  calc_stats(0.2,0.5), 
  calc_stats(0.2,1.0),
  calc_stats(0.2,2.0),
  calc_stats(0.2,3.0),
  calc_stats(0.2,4.0)

)

print(results)
# Run function for different a, b values
results <- cbind(
  calc_stats(0.5,0.2),
  calc_stats(0.5,0.5), 
  calc_stats(0.5,1.0),
  calc_stats(0.5,2.0),
  calc_stats(0.5,3.0),
  calc_stats(0.5,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(1,0.2),
  calc_stats(1,0.5), 
  calc_stats(1,1.0),
  calc_stats(1,2.0),
  calc_stats(1,3.0),
  calc_stats(1,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(2,0.2),
  calc_stats(2,0.5), 
  calc_stats(2,1.0),
  calc_stats(2,2.0),
  calc_stats(2,3.0),
  calc_stats(2,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(3,0.2),
  calc_stats(3,0.5), 
  calc_stats(3,1.0),
  calc_stats(3,2.0),
  calc_stats(3,3.0),
  calc_stats(3,4.0)

)

print(results)

# Run function for different a, b values
results <- cbind(
  calc_stats(4,0.2),
  calc_stats(4,0.5), 
  calc_stats(4,1.0),
  calc_stats(4,2.0),
  calc_stats(4,3.0),
  calc_stats(4,4.0)

)

print(results)
