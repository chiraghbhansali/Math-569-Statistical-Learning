#
# Duplicates Figure 3.8 from the book
#
# Written by:
# -- 
# John L. Weatherwax                2009-04-21
# 
# email: wax@alum.mit.edu
# 
# Please send comments and especially bug reports to the
# above email address.
# 
#-----

library(pracma)
library(far)


res = load_prostate_data(globalScale=FALSE,trainingScale=TRUE)
XTraining = res[[1]]
XTesting = res[[2]]

nrow = dim( XTraining )[1]
p = dim( XTraining )[2] - 1 # the last column is the response 

DO = XTraining[,1:p] # get the predictor data
#D = XTraining[,1:p] # get the predictor data


# Standardize the data.
#
# Note that one can get the centering value (the mean) with the command attr(D,'scaled:center')
#                   and the scaling value (the sample standard deviation) with the command attr(D,'scaled:scale')
# 


DO = scale( DO )
means = attr(DO,"scaled:center")
stds = attr(DO,"scaled:scale")


###Create Orthogonal basis for the given data set

D <- orthonormalization(DO, basis=FALSE, norm=TRUE)

#D1 <- gramSchmidt(DO)$Q


lpsa = XTraining[,p+1]
lpsaMean = mean(lpsa)
lpsa = lpsa - lpsaMean # demean the response and don't estimate \beta_0 (since it should be zero)

# ridge-regression is also found in the MASS package
#
library(MASS)
#help(lm.ridge)

numComplexity = 10
lambda = seq( from=0.0, to = 10.0, length=numComplexity )

for( ii in 1:numComplexity ){
  lmbda = lambda[ii]
  #lmbda=0

  # compute the ridge-regression beta hat estimates:
  #
  #div = ginv( t(D) %*% D ) %*% t(D)
  #divfin = div %*% as.matrix(lpsa)
  #absdivfin = abs(divfin)
  #M = ginv( t(D) %*% D + lmbda*diag(p) ) %*% t(D)
  #M = ginv( diag(p) + lmbda*diag(p) ) %*% t(D)
  #betaHat = M %*% as.matrix(lpsa)
  
  
  #### Calculate the Betahat for leastsquare
  betaHatLs= t(D) %*% as.matrix(lpsa)
  
  ### Calculate the sign of BetaHatLasso

  #### calculate the Betahat for lasso as sing(betahatLs)(absolutevalue(betahatLs - lambda))
  #dim (betaHatLs)
  BetaHatLasso1=betaHatLs - diag(lmbda*diag(p))
  
    if(sum(BetaHatLasso1) > 0 )
  {
  sn=-1
  betaHatLasso = sn*(abs(betaHatLs - diag(lmbda*diag(p))))
  }else{
  sn=1
  betaHatLasso = sn*(abs(betaHatLs + diag(lmbda*diag(p))))
  }
  
  
  #betaHatLasso = sn*(abs(betaHatLs - diag(lmbda*diag(p))))
  #dim (betaHatLasso)
  
  betaHatRidge = ginv(diag(p) + lmbda*diag(p)) %*% betaHatLs 
  #dim (betaHatRidge)
  
  
  
  
  betaHat = betaHatLasso
  #betaHat = betaHatRidge
  
  #M = ginv( diag(p) + lmbda*diag(p) ) %*% t(D)
  #M = ginv( t(D) %*% D ) %*% t(D)
  #M = t(D)
  #betaHat = M %*% as.matrix(lpsa)
  #betaHat1 = betaHat %/% absdivfin
  

  # compute the degrees of freedom for this values of lambda:
  #
  #dof = abs(M)
  #dof = abs(sum( diag( D %*% M ) ) )
  ### plot against lambda
  dof=lmbda
  
  # each column is a different value of the complexity parameter ...
  if( ii==1 ){
    betaRes = betaHat
    dofRes  = dof 
  }else{
    betaRes = cbind( betaRes, betaHat )
    dofRes  = cbind( dofRes, dof ) 
  }
}

# plot everything:
# 
#postscript("../../WriteUp/Graphics/Chapter3/fig_3_8_dup.eps", onefile=FALSE, horizontal=FALSE)

minX = min(dofRes)
maxX = max(dofRes) 
minY = min(betaRes)
maxY = max(betaRes)
for( ki in 1:p ){
  if( ki==1 ){ 
    plot( dofRes, betaRes[ki,], xlim=c(minX,1.1*maxX), ylim=c(minY,maxY), type="l",
          xlab="Lambda", ylab="Beta Hat (Lasso regression)" )
  }else{
    points( dofRes, betaRes[ki,], ylim=c(minY,maxY), type="l" )
  }
  if( names(XTraining)[ki]=="svi" ){
    text( 1.075*dofRes[1], 1.02*betaRes[ki,1], names(XTraining)[ki] )
  }else if( names(XTraining)[ki]=="lweight" ){
    text( 1.075*dofRes[1], betaRes[ki,1], names(XTraining)[ki] )
  }else if( names(XTraining)[ki]=="pgg45" ){
    text( 1.075*dofRes[1], 0.925*betaRes[ki,1], names(XTraining)[ki] )
  }else{
    text( 1.075*dofRes[1], betaRes[ki,1], names(XTraining)[ki] )
  }
}

#dev.off()

