####################################################################
##  Datas 16
################################################################
##ICD
Log-Likelihood: -532.2033 
2  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta  16.1872     1.5470  10.464  <2e-16 ***
alpha   0.8882     0.5687   1.562   0.118    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(ml)
[1] 1068.407
> ll <- -2*logLik(ml)
> ll
[1] 1064.407
BIC
[1] 1073.965
attr(,"df")
[1] 2
> A <- AIC(ml)
> AICc <- A+(12/(n-3))
> AICc
[1] 1068.51
attr(,"df")
[1] 2
> HQIC<- ll+(2*log(log(n)))
> HQIC
[1] 1067.535
attr(,"df")
[1] 2
> k <- 2
> CAIC <- A + (2*k*(k+1))/(n-k-1)
> CAIC
[1] 1068.51

##IVD
Log-Likelihood: -535.3027 
1  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta   14.275      1.326   10.76  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(mlc)
[1] 1072.605
attr(,"df")
[1] 1
> llc <- -2*logLik(mlc)
> llc
[1] 1070.605
attr(,"df")
[1] 1
> BICc <- llc+(2*log(n))
> BICc
[1] 1080.164
attr(,"df")
[1] 1
> HQICc<- llc+(2*log(log(n)))
> HQICc
[1] 1073.734
attr(,"df")
[1] 1
> k <- 2
> Ac <- AIC(mlc)
> CAICc <- Ac + (2*k*(k+1))/(n-k-1)
> CAICc
[1] 1072.709

#Inverse Suja
Log-Likelihood: -535.1981 
1  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta   14.037      1.211   11.59  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(mld)
[1] 1072.396
attr(,"df")
[1] 1
> lld <- -2*logLik(mld)
> lld
[1] 1070.396
attr(,"df")
[1] 1
> BICd <- lld+(2*log(n))
> BICd
[1] 1079.954
attr(,"df")
[1] 1
> HQICd<- lld+(2*log(log(n)))
> HQICd
[1] 1073.525
attr(,"df")
[1] 1
> k <- 2
> Ad <- AIC(mld)
> CAICd <- Ad + (2*k*(k+1))/(n-k-1)
> CAICd
[1] 1072.5

## ILD
Log-Likelihood: -542.0826 
1  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta   14.884      1.326   11.22  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(mlg)
[1] 1086.165
attr(,"df")
[1] 1
> llg <- -2*logLik(mlg)
> llg
[1] 1084.165
attr(,"df")
[1] 1
> BICg <- llg+(2*log(n))
> BICg
[1] 1093.723
attr(,"df")
[1] 1
> HQICg<- llg+(2*log(log(n)))
> HQICg
[1] 1087.294
attr(,"df")
[1] 1
> k <- 2
> Ag <- AIC(mlg)
> CAICg <- Ag + (2*k*(k+1))/(n-k-1)
> CAICg
[1] 1086.269


####################################################################
##  Datas 17
################################################################
##ICD
Log-Likelihood: -381.3487 
2  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta   74.431      4.194   17.75  <2e-16 ***
alpha    7.029      2.966    2.37  0.0178 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(ml)
[1] 766.6974
attr(,"df")
[1] 2
> ll <- -2*logLik(ml)
> ll
[1] 762.6974
attr(,"df")
[1] 2
> BIC <- ll+(2*log(n))
> BIC
[1] 770.8183
attr(,"df")
[1] 2
> A <- AIC(ml)
> AICc <- A+(12/(n-3))
> AICc
[1] 766.9156
attr(,"df")
[1] 2
> HQIC<- ll+(2*log(log(n)))
> HQIC
[1] 765.5
attr(,"df")
[1] 2
> k <- 2
> CAIC <- A + (2*k*(k+1))/(n-k-1)
> CAIC
[1] 766.9156

##	IAD
Log-Likelihood: -385.6517 
1  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta   59.193      2.966   19.96  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(mlc)
[1] 773.3033
attr(,"df")
[1] 1
> llc <- -2*logLik(mlc)
> llc
[1] 771.3033
attr(,"df")
[1] 1
> BICc <- llc+(2*log(n))
> BICc
[1] 779.4242
attr(,"df")
[1] 1
> HQICc<- llc+(2*log(log(n)))
> HQICc
[1] 774.1059
attr(,"df")
[1] 1
> k <- 2
> Ac <- AIC(mlc)
> CAICc <- Ac + (2*k*(k+1))/(n-k-1)
> CAICc
[1] 773.5215

##	ISD
Log-Likelihood: -385.6861 
1  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta   59.126      2.422   24.42  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(mld)
[1] 773.3722
attr(,"df")
[1] 1
> lld <- -2*logLik(mld)
> lld
[1] 771.3722
attr(,"df")
[1] 1
> BICd <- lld+(2*log(n))
> BICd
[1] 779.4931
attr(,"df")
[1] 1
> HQICd<- lld+(2*log(log(n)))
> HQICd
[1] 774.1748
attr(,"df")
[1] 1
> k <- 2
> Ad <- AIC(mld)
> CAICd <- Ad + (2*k*(k+1))/(n-k-1)
> CAICd
[1] 773.5904

### ILD
Log-Likelihood: -386.5834 
1  free parameters
Estimates:
      Estimate Std. error t value Pr(> t)    
theta   60.094      4.194   14.33  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> AIC(mlg)
[1] 775.1669
attr(,"df")
[1] 1
> llg <- -2*logLik(mlg)
> llg
[1] 773.1669
attr(,"df")
[1] 1
> BICg <- llg+(2*log(n))
> BICg
[1] 781.2878
attr(,"df")
[1] 1
> HQICg<- llg+(2*log(log(n)))
> HQICg
[1] 775.9695
attr(,"df")
[1] 1
> k <- 2
> Ag <- AIC(mlg)
> CAICg <- Ag + (2*k*(k+1))/(n-k-1)
> CAICg
[1] 775.3851






























