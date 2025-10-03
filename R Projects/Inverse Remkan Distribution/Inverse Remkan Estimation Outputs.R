####################################################################
##  Datas 20
################################################################
##IRD
 summary(ml)
--------------------------------------------
Maximum Likelihood estimation
BFGS maximization, 242 iterations
Return code 1: iteration limit exceeded 
Log-Likelihood: -58.84861 
2  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta   4.5655     0.3316   13.77  <2e-16 ***
alpha 121.4492     2.9705   40.89  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(ml)
[1] 121.6972
attr(,"df")
[1] 2
 ll
[1] 117.6972
attr(,"df")
[1] 2
> BIC <- ll+(2*log(n))
> BIC
[1] 125.9835
attr(,"df")
[1] 2
> A <- AIC(ml)
> AICc <- A+(12/(n-3))
> AICc
[1] 121.8972
attr(,"df")
[1] 2
> HQIC<- ll+(2*log(log(n)))
> HQIC
[1] 120.5401
attr(,"df")
[1] 2
> k <- 2
> CAIC <- A + (2*k*(k+1))/(n-k-1)
> CAIC
[1] 121.8972

##IAD
Maximum Likelihood estimation
BFGS maximization, 32 iterations
Return code 0: successful convergence 
Log-Likelihood: -96.22506 
1  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta    2.342      0.194   12.07  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(mlc)
[1] 194.4501
attr(,"df")
[1] 1
> llc <- -2*logLik(mlc)
llc
[1] 192.4501
attr(,"df")
[1] 1
> BICc <- llc+(2*log(n))
> BICc
[1] 200.7364
attr(,"df")
[1] 1
> HQICc<- llc+(2*log(log(n)))
> HQICc
[1] 195.293
attr(,"df")
[1] 1
> k <- 2
> Ac <- AIC(mlc)
> CAICc <- Ac + (2*k*(k+1))/(n-k-1)
> CAICc
[1] 194.6501

#Inverse Suja
--------------------------------------------
Maximum Likelihood estimation
BFGS maximization, 29 iterations
Return code 0: successful convergence 
Log-Likelihood: -114.8847 
1  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta   2.9680     0.1699   17.47  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(mld)
[1] 231.7694
attr(,"df")
[1] 1
> lld <- -2*logLik(mld)
lld
[1] 229.7694
attr(,"df")
[1] 1
> BICd <- lld+(2*log(n))
> BICd
[1] 238.0557
attr(,"df")
[1] 1
> HQICd<- lld+(2*log(log(n)))
> HQICd
[1] 232.6123
attr(,"df")
[1] 1
> k <- 2
> Ad <- AIC(mld)
> CAICd <- Ad + (2*k*(k+1))/(n-k-1)
> CAICd
[1] 231.9694

## ILD
--------------------------------------------
Maximum Likelihood estimation
BFGS maximization, 21 iterations
Return code 0: successful convergence 
Log-Likelihood: -97.99894 
1  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta   2.0297     0.2054   9.882  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(mlg)
[1] 197.9979
attr(,"df")
[1] 1
> llg <- -2*logLik(mlg)
llg
[1] 195.9979
attr(,"df")
[1] 1
> BICg <- llg+(2*log(n))
> BICg
[1] 204.2841
attr(,"df")
[1] 1
> HQICg<- llg+(2*log(log(n)))
> HQICg
[1] 198.8408
attr(,"df")
[1] 1
> k <- 2
> Ag <- AIC(mlg)
> CAICg <- Ag + (2*k*(k+1))/(n-k-1)
> CAICg
[1] 198.1979


####################################################################
##  Datas 21
################################################################
##IRD
--------------------------------------------
Maximum Likelihood estimation
BFGS maximization, 282 iterations
Return code 1: iteration limit exceeded 
Log-Likelihood: -97.80332 
2  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta    4.601      0.268   17.17  <2e-16 ***
alpha  114.748      4.228   27.14  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(ml)
[1] 199.6066
attr(,"df")
[1] 2
> ll <- -2*logLik(ml)
> ll
[1] 195.6066
attr(,"df")
[1] 2
> BIC <- ll+(2*log(n))
BIC
[1] 204.817
attr(,"df")
[1] 2
> HQIC<- ll+(2*log(log(n)))
> HQIC
[1] 198.661
attr(,"df")
[1] 2
> k <- 2
> CAIC <- A + (2*k*(k+1))/(n-k-1)
> CAIC
[1] 199.7304

##	IAD
--------------------------------------------
Maximum Likelihood estimation
BFGS maximization, 25 iterations
Return code 0: successful convergence 
Log-Likelihood: -154.8316 
1  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta    2.345      0.154   15.22  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(mlc)
[1] 311.6631
attr(,"df")
[1] 1
> llc <- -2*logLik(mlc)
> llc
[1] 309.6631
attr(,"df")
[1] 1
> BICc <- llc+(2*log(n))
 BICc
[1] 318.8735
attr(,"df")
[1] 1
> HQICc<- llc+(2*log(log(n)))
> HQICc
[1] 312.7175
attr(,"df")
[1] 1
> CAICc <- Ac + (2*k*(k+1))/(n-k-1)
> CAICc
[1] 311.7868

##	ISD
--------------------------------------------
Maximum Likelihood estimation
BFGS maximization, 24 iterations
Return code 0: successful convergence 
Log-Likelihood: -183.3388 
1  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta   2.9706     0.1354   21.93  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(mld)
[1] 368.6776
attr(,"df")
[1] 1
> lld <- -2*logLik(mld)
> lld
[1] 366.6776
attr(,"df")
[1] 1
> BICd <- lld+(2*log(n))
> BICd
[1] 375.888
attr(,"df")
[1] 1
> HQICd<- lld+(2*log(log(n)))
> HQICd
[1] 369.732
attr(,"df")
[1] 1
> CAICd <- Ad + (2*k*(k+1))/(n-k-1)
> CAICd
[1] 368.8013

### ILD
--------------------------------------------
Maximum Likelihood estimation
BFGS maximization, 16 iterations
Return code 0: successful convergence 
Log-Likelihood: -157.6916 
1  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta   2.0335     0.1634   12.45  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(mlg)
[1] 317.3832
> llg <- -2*logLik(mlg)
> llg
[1] 315.3832
> BICg <- llg+(2*log(n))
> BICg
[1] 324.5936
attr(,"df")
[1] 1
> HQICg<- llg+(2*log(log(n)))
> HQICg
[1] 318.4376
attr(,"df")
[1] 1
> k <- 2
> Ag <- AIC(mlg)
> CAICg <- Ag + (2*k*(k+1))/(n-k-1)
> CAICg
[1] 317.5069

