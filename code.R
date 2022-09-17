library(readxl)
library(timeSeries)
library(fGarch)
library(MASS)
library(quadprog)
library(ggplot2)  



# load data
funds = read_excel("US_FUND_DATA.xlsx", sheet=1)
factors = read_excel("US_FUND_DATA.xlsx", sheet=2)

# keep series 51-90
funds = funds[,c(52:91)]
colnames(funds) = c(1:40)  # rename the columns

# convert the datasets to time series objects
Time = seq(as.Date("1998-01-01"), as.Date("2019-07-01"), by="1 month")
Time = timeLastDayInMonth(Time)
dataReturns = timeSeries(funds, Time)
head(dataReturns)

factors = factors[,-1]
factorReturns = timeSeries(factors, Time)
head(factorReturns)


# ============================================================================
#                           PERFORMANCE EVALUATION
# ============================================================================

t = dim(dataReturns)[1]
k = dim(dataReturns)[2]


# in-sample & out-of-sample period, top performing funds
outofsample = 36           
insample = t - outofsample
topFunds = 0.2
numTopFunds = round(topFunds*k)   # number of top funds = 8


# in-sample & out-of-sample returns
Returns = dataReturns[1:insample,]          # in-sample returns
dim(Returns)
ReturnsOOS = dataReturns[(insample+1):t,]   # out-of-sample returns
dim(ReturnsOOS)
Factors = factorReturns[1:insample,]
dim(Factors)





# SHARPE RATIO ------------------------------------------------------------

meanVec = apply(Returns, 2, mean)
varVec = apply(Returns, 2, var)
stdVec = sqrt(varVec)

SharpeRatio = meanVec/stdVec

SRsorted = sort(SharpeRatio, index.return=T)
SharpeRatio_Sorted = SRsorted$x
IndexSharpeRatio_Sorted = SRsorted$ix
IndexSharpeRatio_Top = IndexSharpeRatio_Sorted[(k-numTopFunds+1):k]

# equally-weighted portfolios
Returns_SharpeRatio = ReturnsOOS[,IndexSharpeRatio_Top]
dim(Returns_SharpeRatio)
meanRet_SharpeRatio = apply(Returns_SharpeRatio,1,mean)
cumRet_SharpeRatio = cumsum(meanRet_SharpeRatio)

plot(cumRet_SharpeRatio, type="l", ylab="cumulative returns")






# TREYNOR RATIO -----------------------------------------------------------

betas = NULL
for(i in 1:k){
  y = Returns[,i]
  x = Factors[,1]
  yres = lm(y ~ x)
  beta = coef(yres)[2]
  betas = cbind(betas, beta)
}
betas

meanVec = apply(Returns, 2, mean)

TreynorRatio = meanVec/betas

TRsorted = sort(TreynorRatio, index.return=T)
TreynorRatio_Sorted = TRsorted$x
IndexTreynorRatio_Sorted = TRsorted$ix
IndexTreynorRatio_Top = IndexTreynorRatio_Sorted[(k-numTopFunds+1):k]

# equally-weighted portfolios
Returns_TreynorRatio = ReturnsOOS[,IndexTreynorRatio_Top]
dim(Returns_TreynorRatio)
meanRet_TreynorRatio = apply(Returns_TreynorRatio,1,mean)
cumRet_TreynorRatio = cumsum(meanRet_TreynorRatio)

plot(cumRet_TreynorRatio, type="l", ylab="cumulative returns")






# SORTINO RATIO -----------------------------------------------------------

deltas = NULL
for(i in 1:k){
  y = Returns[,i]
  mvalue = mean(y)
  #mvalue = Factors[,1]
  minvec = NULL
  for(j in 1:length(y)){
    minvechelp = min(0, y[j]-mvalue)
    minvec[j] = minvechelp
  }
  delta = sqrt(sum(minvec^2)/length(y))
  deltas = cbind(deltas, delta)
}
deltas

mVec = apply(Returns, 2, mean)

SortinoRatio = mVec/deltas

SOsorted = sort(SortinoRatio, index.return=T)
SortinoRatio_sorted = SOsorted$x
IndexSortinoRatio_Sorted = SOsorted$ix
IndexSortinoRatio_Top = IndexSortinoRatio_Sorted[(k-numTopFunds+1):k]

# equally-weighted portfolios
Returns_SortinoRatio = ReturnsOOS[,IndexSortinoRatio_Top]
dim(Returns_SortinoRatio)
meanRet_SortinoRatio = apply(Returns_SortinoRatio,1,mean)
cumRet_SortinoRatio = cumsum(meanRet_SortinoRatio)

plot(cumRet_SortinoRatio, type="l", ylab="cumulative returns")





# JENSEN' ALPHA -----------------------------------------------------------

alphas = NULL
for(i in 1:k){
  y = Returns[,i]
  x = Factors[,1]
  yres = lm(y ~ x)
  alpha = coef(yres)[1]
  alphas = cbind(alphas,alpha)
}
alphas

JAsorted = sort(alphas, index.return=T)
JensensAlpha_sorted = JAsorted$x
IndexJensensAlpha_Sorted = JAsorted$ix
IndexJensensAlpha_Top = IndexJensensAlpha_Sorted[(k-numTopFunds+1):k]

# equally-weighted portfolios
Returns_JensensAlpha = ReturnsOOS[,IndexJensensAlpha_Top]
dim(Returns_JensensAlpha)
meanRet_JensensAlpha = apply(Returns_JensensAlpha,1,mean)
cumRet_JensensAlpha = cumsum(meanRet_JensensAlpha)

plot(cumRet_JensensAlpha, type="l", ylab="cumulative returns")





# JENSEN'S ALPHA MULTIPLE -------------------------------------------------

x1 = Factors$`Mkt-RF`
x2 = Factors$SMB
x3 = Factors$HML
x4 = Factors$RMW
x5 = Factors$CMA
x6 = Factors$MOM
x7 = Factors$BAB
x8 = Factors$CAR

coefs = NULL
alphasMult = NULL
for(i in 1:k){
  y = Returns[,i]
  full.model = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8)
  step.model = stepAIC(full.model, direction = "backward",
                       trace = FALSE)
  coefs[i] = list(step.model$coefficients) 
  alphasMult[i] = c(round(step.model$coefficients[1],9))
}
alphasMult
coefs

JAsortedMult = sort(alphasMult, index.return=T)
JensensAlphaMult_sorted = JAsortedMult$x
IndexJensensAlphaMult_Sorted = JAsortedMult$ix
IndexJensensAlphaMult_Top = IndexJensensAlphaMult_Sorted[(k-numTopFunds+1):k]

# equally-weighted portfolios
Returns_JensensAlphaMult = ReturnsOOS[,IndexJensensAlphaMult_Top]
dim(Returns_JensensAlphaMult)
meanRet_JensensAlphaMult = apply(Returns_JensensAlphaMult,1,mean)
cumRet_JensensAlphaMult = cumsum(meanRet_JensensAlphaMult)

plot(cumRet_JensensAlphaMult, type="l", ylab="cumulative returns")






# ALL PLOTS TOGETHER ------------------------------------------------------



# Cumulative Returns
plot(cumRet_SharpeRatio, main="Cumulative Portfolio Weights", type="l", col="black",ylab=NA,ylim=c(0,0.45))
lines(cumRet_SortinoRatio, type="l", col="blue")
lines(cumRet_TreynorRatio, type="l", col="magenta")
lines(cumRet_JensensAlpha, type="l", col="orange")
lines(cumRet_JensensAlphaMult, type="l", col="chartreuse4")
legend("topleft",legend=c("Sharpe", "Sortino","Treynor","Jensen", "Jensen Multiple"),
       lty=c(1,1), pch=c(NA, 16), col=c("black","blue","magenta","orange","chartreuse4"))









#################################################################################################################




# ============================================================================
#                           PORTFOLIO CONSTRUCTION
# ============================================================================



# Sample Estimates --------------------------------------------------------


t = dim(dataReturns)[1]
k = dim(dataReturns)[2]


# in-sample & out-of-sample period, target return
outofsample = 36           
insample = t - outofsample
target = 0.005


# Empty matrices for the Results of MINIMUM VARIANCE PORTFOLIO
keep_PortRisk_static_MINVAR = NULL
keepPortWeights_static_MINVAR = NULL
Realized_Ret_static_MINVAR = NULL
Cum_Realized_Ret_static_MINVAR = NULL
keep_CSR_static_MINVAR = NULL

# Empty matrices for the Results of MEAN VARIANCE PORTFOLIO
keep_PortRisk_static_MEANVAR = NULL
keepPortWeights_static_MEANVAR = NULL
Realized_Ret_static_MEANVAR = NULL
Cum_Realized_Ret_static_MEANVAR = NULL
keep_CSR_static_MEANVAR = NULL

# Start the out of sample performance of the models
keep_meanvec_static = keep_covmat_static = NULL
for (i in 1:outofsample){
  print(i)
  dataanalyse = NULL
  dataanalyse = dataReturns[1:(insample+i-1),]
  
  # Calculate the corresponding mean and covariance matrices
  keep_meanvec_static[[i]] = apply(dataanalyse, 2, mean)
  keep_covmat_static[[i]] = cov(dataanalyse)
  m_vec = keep_meanvec_static[[i]]
  cov_mat = keep_covmat_static[[i]]
  
  #=====================================================================
  # Find optimal Minimum Variance portfolio
  #=====================================================================
  # Set matrices with constraints
  D.mat = 2*cov_mat
  d.vec = rep(0, k)
  A.mat = cbind(rep(1,k), diag(k))
  b.vec = c(1, rep(0,k))
  # Solve the Quadratic Programming Problem
  qp.Result = solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=1)
  x_static_MINVAR = as.matrix(round(qp.Result$solution,5), k, 1)   # optimal portfolio weights
  
  # keep portfolio weights across time
  keepPortWeights_static_MINVAR[[i]] = x_static_MINVAR
  
  # calculate Out of Sample Returns
  RR_static_MINVAR = dataReturns[insample+i,]%*%x_static_MINVAR
  Realized_Ret_static_MINVAR[i] = RR_static_MINVAR
  
  # calculate Port Risk (portfolio standard deviation)
  PR_static_MINVAR = sqrt(t(x_static_MINVAR)%*%cov_mat%*%x_static_MINVAR)   
  keep_PortRisk_static_MINVAR[i] = PR_static_MINVAR
  
  # calculate Conditional Sharp Ratio (CSR)
  CSR_static_MINVAR = RR_static_MINVAR/PR_static_MINVAR
  keep_CSR_static_MINVAR[i] = CSR_static_MINVAR
  
  #=====================================================================
  # Find optimal Mean Variance portfolio
  #=====================================================================
  # Set matrices with constraints
  D.mat = 2*cov_mat
  d.vec = rep(0, k)
  A.mat = cbind(rep(1,k), m_vec, diag(k))
  b.vec = c(1, target, rep(0,k))
  # Solve the Quadratic Programming Problem
  qp.Result = solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=1)
  x_static_MEANVAR = as.matrix(round(qp.Result$solution,5), k, 1)     # optimal portfolio weights
  
  # keep portfolio weights across time
  keepPortWeights_static_MEANVAR[[i]] = x_static_MEANVAR
  
  # calculate Out of Sample Returns
  RR_static_MEANVAR = dataReturns[insample+i,]%*%x_static_MEANVAR 
  Realized_Ret_static_MEANVAR[i] = RR_static_MEANVAR
  
  # calculate Port Risk (portfolio standard deviation)
  PR_static_MEANVAR = sqrt(t(x_static_MEANVAR)%*%cov_mat%*%x_static_MEANVAR)   
  keep_PortRisk_static_MEANVAR[i] = PR_static_MEANVAR
  
  # calculate Conditional Sharp Ratio (CSR)
  CSR_static_MEANVAR = RR_static_MEANVAR/PR_static_MEANVAR
  keep_CSR_static_MEANVAR[i] = CSR_static_MEANVAR
}

# calculate Cumulative Returns
Cum_Realized_Ret_static_MINVAR = cumsum(Realized_Ret_static_MINVAR)
Cum_Realized_Ret_static_MEANVAR = cumsum(Realized_Ret_static_MEANVAR)

MINVAR.SAMPLE = cbind(mean(Realized_Ret_static_MINVAR), 
                      mean(keep_PortRisk_static_MINVAR), 
                      Cum_Realized_Ret_static_MINVAR[outofsample],
                      mean(keep_CSR_static_MINVAR))

MEANVAR.SAMPLE = cbind(mean(Realized_Ret_static_MEANVAR), 
                       mean(keep_PortRisk_static_MEANVAR), 
                       Cum_Realized_Ret_static_MEANVAR[outofsample],
                       mean(keep_CSR_static_MEANVAR))

Res.SAMPLE = data.frame(round(rbind(MINVAR.SAMPLE, MEANVAR.SAMPLE), 4))
names(Res.SAMPLE) = c("Mean Return", "Volatility", "Cumulative Return", "Conditional Sharpe Ratio")
row.names(Res.SAMPLE) = c("Minimum Variance", "Mean Variance")
Res.SAMPLE

keepPortWeights_static_MINVAR = do.call(cbind, keepPortWeights_static_MINVAR)
dim(keepPortWeights_static_MINVAR)
keepPortWeights_static_MINVAR

keepPortWeights_static_MEANVAR = do.call(cbind, keepPortWeights_static_MEANVAR)
dim(keepPortWeights_static_MEANVAR)
keepPortWeights_static_MEANVAR

round(apply(keepPortWeights_static_MINVAR, 1, mean),2)
round(apply(keepPortWeights_static_MEANVAR, 1, mean),2)


# Cumulative Portfolio Weights
plot(Cum_Realized_Ret_static_MEANVAR, main="Cumulative Portfolio Weights", type="l", 
     col="blue", ylab=NA, ylim=c(-0.032, 0.30))
lines(Cum_Realized_Ret_static_MINVAR, type="l", col="red")
legend("topleft",
       legend=c("MEANVAR", "MINVAR"),
       lty=c(1,1), pch=c(NA, 16), col=c("blue","red"))






#==================================================================================

#  SIM  --------------------------------------------------------------------


t = dim(dataReturns)[1]
k = dim(dataReturns)[2]


# in-sample & out-of-sample period, target return
outofsample = 36           
insample = t - outofsample

# Empty matrices for the Results of MINIMUM VARIANCE PORTFOLIO
keep_PortRisk_SIM_MINVAR = NULL
keepPortWeights_SIM_MINVAR = NULL
Realized_Ret_SIM_MINVAR = NULL
Cum_Realized_Ret_SIM_MINVAR = NULL
keep_CSR_SIM_MINVAR = NULL

# Empty matrices for the Results of MEAN VARIANCE PORTFOLIO
keep_PortRisk_SIM_MEANVAR = NULL
keepPortWeights_SIM_MEANVAR = NULL
Realized_Ret_SIM_MEANVAR = NULL
Cum_Realized_Ret_SIM_MEANVAR = NULL
keep_CSR_SIM_MEANVAR = NULL

# Start the out of sample performance of the models
keep_meanvec_SIM = keep_covmat_SIM = NULL
for (i in 1:outofsample){
  print(i)
  dataanalyse = NULL
  dataanalyse = dataReturns[1:(insample+i-1),]
  datafactoranalyse = factorReturns[1:(insample+i-1)]
  
  
  #==========================================================================
  # Estimate Single Index Model model parameters
  #==========================================================================
  # Model: Rit   =   alpha_i   +   beta_i * R_Mt   +   et
  #==========================================================================
  T = dim(dataanalyse)[1]
  N = dim(dataanalyse)[2]
  ones = rep(1,T)
  DM = cbind(ones,datafactoranalyse) # construct design matrix
  alphas = NULL
  betas = NULL
  sigmas2 = NULL
  ehat = NULL
  for (j in 1:N){
    bhat = solve(t(DM)%*%DM)%*%t(DM)%*%dataanalyse[,j]     # estimate model parameters (ai,bi)
    alphas[j] = bhat[1]
    betas[j] = bhat[2]
    ehat = dataanalyse[,j] - DM%*%bhat    # estimate residuals
    sigmas2[j] = t(ehat)%*%ehat/(T) # sigmas2_i[i] = t(ehat)%*%ehat/(T-2)
  }
  alphas
  betas
  sigmas2
  
  ExpRet = alphas + betas * mean(datafactoranalyse)
  CovRet = var(datafactoranalyse) * betas%*%t(betas) + diag(sigmas2,nrow=length(sigmas2))
  
  #==========================================================================
  
  
  # Calculate the corresponding mean and covariance matrices
  keep_meanvec_SIM[[i]] = ExpRet
  keep_covmat_SIM[[i]] = CovRet
  m_vec = keep_meanvec_SIM[[i]]
  cov_mat = keep_covmat_SIM[[i]]
  
  #=====================================================================
  # Find optimal Minimum Variance portfolio
  #=====================================================================
  # Set matrices with constraints
  D.mat = 2*cov_mat
  d.vec = rep(0, k)
  A.mat = cbind(rep(1,k), diag(k))
  b.vec = c(1, rep(0,k))
  # Solve the Quadratic Programming Problem
  qp.Result = solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=1)
  # x_SIM_MINVAR are the optimal portfolio weights (k x 1 vector)
  x_SIM_MINVAR = as.matrix(round(qp.Result$solution,5), k, 1)
  
  #keep portfolio weights across time
  keepPortWeights_SIM_MINVAR[[i]] = x_SIM_MINVAR
  
  #calculate Out of Sample Returns
  RR_SIM_MINVAR = dataReturns[insample+i,]%*%x_SIM_MINVAR
  Realized_Ret_SIM_MINVAR[i] = RR_SIM_MINVAR
  
  #calculate Port Risk (portfolio standard deviation)
  PR_SIM_MINVAR = sqrt(t(x_SIM_MINVAR)%*%cov_mat%*%x_SIM_MINVAR)   
  keep_PortRisk_SIM_MINVAR[i] = PR_SIM_MINVAR
  
  #calculate Conditional Sharp Ratio (CSR)
  CSR_SIM_MINVAR = RR_SIM_MINVAR/PR_SIM_MINVAR
  keep_CSR_SIM_MINVAR[i] = CSR_SIM_MINVAR
  
  #=====================================================================
  # Find optimal Mean Variance portfolio
  #=====================================================================
  # Set matrices with constraints
  D.mat = 2*cov_mat
  d.vec = rep(0, k)
  A.mat = cbind(rep(1,k), m_vec, diag(k))
  b.vec = c(1, target, rep(0,k))
  # Solve the Quadratic Programming Problem
  qp.Result = solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=1)
  # x_SIM_MEANVAR are the optimal portfolio weights (k x 1 vector)
  x_SIM_MEANVAR = as.matrix(round(qp.Result$solution,5), k, 1)
  
  #keep portfolio weights across time
  keepPortWeights_SIM_MEANVAR[[i]] = x_SIM_MEANVAR
  
  #calculate Out of Sample Returns
  RR_SIM_MEANVAR = dataReturns[insample+i,]%*%x_SIM_MEANVAR
  Realized_Ret_SIM_MEANVAR[i] = RR_SIM_MEANVAR
  
  #calculate Port Risk (portfolio standard deviation)
  PR_SIM_MEANVAR = sqrt(t(x_SIM_MEANVAR)%*%cov_mat%*%x_SIM_MEANVAR)   
  keep_PortRisk_SIM_MEANVAR[i] = PR_SIM_MEANVAR
  
  #calculate Conditional Sharp Ratio (CSR)
  CSR_SIM_MEANVAR = RR_SIM_MEANVAR/PR_SIM_MEANVAR
  keep_CSR_SIM_MEANVAR[i] = CSR_SIM_MEANVAR
}

#calculate Cumulative Returns
Cum_Realized_Ret_SIM_MINVAR = cumsum(Realized_Ret_SIM_MINVAR)
Cum_Realized_Ret_SIM_MEANVAR = cumsum(Realized_Ret_SIM_MEANVAR)

MINVAR.SIM = cbind(mean(Realized_Ret_SIM_MINVAR), 
                   mean(keep_PortRisk_SIM_MINVAR), 
                   Cum_Realized_Ret_SIM_MINVAR[outofsample],
                   mean(keep_CSR_SIM_MINVAR))

MEANVAR.SIM = cbind(mean(Realized_Ret_SIM_MEANVAR), 
                    mean(keep_PortRisk_SIM_MEANVAR), 
                    Cum_Realized_Ret_SIM_MEANVAR[outofsample],
                    mean(keep_CSR_SIM_MEANVAR))

Res.SIM = data.frame(round(rbind(MINVAR.SIM, MEANVAR.SIM), 4))
names(Res.SIM) = c("Mean Return", "Volatility", "Cumulative Return", "Conditional Sharpe Ratio")
row.names(Res.SIM) = c("Minimum Variance", "Mean Variance")
Res.SIM


keepPortWeights_SIM_MINVAR = do.call(cbind, keepPortWeights_SIM_MINVAR)
dim(keepPortWeights_SIM_MINVAR)
keepPortWeights_SIM_MINVAR

keepPortWeights_SIM_MEANVAR = do.call(cbind, keepPortWeights_SIM_MEANVAR)
dim(keepPortWeights_SIM_MEANVAR)
keepPortWeights_SIM_MEANVAR

round(apply(keepPortWeights_SIM_MINVAR, 1, mean),3)
round(apply(keepPortWeights_SIM_MEANVAR, 1, mean),3)


# Cumulative Portfolio Weights
plot(Cum_Realized_Ret_SIM_MEANVAR, main="Cumulative Portfolio Weights - SIM", type="l", col="black",ylab=NA,ylim=c(-0.031,0.30))
lines(Cum_Realized_Ret_SIM_MINVAR, type="l", col="magenta")
legend("topleft",
       legend=c("MEANVAR-SIM", "MINVAR-SIM"),
       lty=c(1,1), pch=c(NA, 16), col=c("black","magenta"))







#=================================================================================================


# FULL FACTOR MODEL -------------------------------------------------------

t = dim(dataReturns)[1]
k = dim(dataReturns)[2]

# in-sample & out-of-sample period, target return
outofsample = 36           
insample = t - outofsample


# Empty matrices for the Results of MINIMUM VARIANCE PORTFOLIO
keep_PortRisk_FULL_MINVAR = NULL
keepPortWeights_FULL_MINVAR = NULL
Realized_Ret_FULL_MINVAR = NULL
Cum_Realized_Ret_FULL_MINVAR = NULL
keep_CSR_FULL_MINVAR = NULL

# Empty matrices for the Results of MEAN VARIANCE PORTFOLIO
keep_PortRisk_FULL_MEANVAR = NULL
keepPortWeights_FULL_MEANVAR = NULL
Realized_Ret_FULL_MEANVAR = NULL
Cum_Realized_Ret_FULL_MEANVAR = NULL
keep_CSR_FULL_MEANVAR = NULL

# Start the out of sample performance of the models
keep_meanvec_FULL = keep_covmat_FULL = NULL
for (i in 1:outofsample){
  print(i)
  dataanalyse = NULL
  dataanalyse = dataReturns[1:(insample+i-1),]
  datafactoranalyse = factorReturns[1:(insample+i-1),]
  
  
  #==========================================================================
  # Estimate Single Index Model model parameters
  #==========================================================================
  T = dim(dataanalyse)[1]
  N = dim(dataanalyse)[2]
  K = dim(factorReturns)[2]
  # Compute design matrix
  DM = matrix(0,T,K+1)
  ones = rep(1,T)
  DM = cbind(ones,datafactoranalyse) 
  Alphas = matrix(0,N,1)   # (nx1) vector
  Betas = matrix(0,N,K)    # (nxk) matrix
  sigmas2 = NULL
  ehat = matrix(0,T,1)     # (nx1) vector
  
  for (j in 1:N){
    bhat = solve(t(DM)%*%DM)%*%t(DM)%*%dataanalyse[,j]  # estimate model parameters
    Alphas[j] = bhat[1]
    Betas[j,1:K] = bhat[2:(K+1)]
    ehat = dataanalyse[,j] - (DM %*% bhat)   # estimate residuals
    sigmas2[j] = t(ehat) %*% ehat/(T) 
  }
  Alphas
  Betas
  sigmas2
  
  
  ExpRet = Alphas + Betas %*% colMeans(datafactoranalyse)
  CovRet = Betas %*% cov(datafactoranalyse) %*% t(Betas) + diag(sigmas2,N)
  
  #==========================================================================
  
  
  # Calculate the corresponding mean and covariance matrices
  keep_meanvec_FULL[[i]] = ExpRet
  keep_covmat_FULL[[i]] = CovRet
  m_vec = keep_meanvec_FULL[[i]]
  cov_mat = keep_covmat_FULL[[i]]
  
  #=====================================================================
  # Find optimal Minimum Variance portfolio
  #=====================================================================
  # Set matrices with constraints
  D.mat = 2*cov_mat
  d.vec = rep(0, k)
  A.mat = cbind(rep(1,k), diag(k))
  b.vec = c(1, rep(0,k))
  # Solve the Quadratic Programming Problem
  qp.Result = solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=1)
  # x_FULL_MINVAR are the optimal portfolio weights (k x 1 vector)
  x_FULL_MINVAR = as.matrix(round(qp.Result$solution,5), k, 1)
  
  #keep portfolio weights across time
  keepPortWeights_FULL_MINVAR[[i]] = x_FULL_MINVAR
  
  #calculate Out of Sample Returns
  RR_FULL_MINVAR = dataReturns[insample+i,]%*%x_FULL_MINVAR
  Realized_Ret_FULL_MINVAR[i] = RR_FULL_MINVAR
  
  #calculate Port Risk (portfolio standard deviation)
  PR_FULL_MINVAR = sqrt(t(x_FULL_MINVAR)%*%cov_mat%*%x_FULL_MINVAR)   
  keep_PortRisk_FULL_MINVAR[i] = PR_FULL_MINVAR
  
  #calculate Conditional Sharp Ratio (CSR)
  CSR_FULL_MINVAR = RR_FULL_MINVAR/PR_FULL_MINVAR
  keep_CSR_FULL_MINVAR[i] = CSR_FULL_MINVAR
  
  #=====================================================================
  # Find optimal Mean Variance portfolio
  #=====================================================================
  # Set matrices with constraints
  D.mat = 2*cov_mat
  d.vec = rep(0, k)
  A.mat = cbind(rep(1,k), m_vec, diag(k))
  b.vec = c(1, target, rep(0,k))
  # Solve the Quadratic Programming Problem
  qp.Result = solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=1)
  # x_FULL_MEANVAR are the optimal portfolio weights (k x 1 vector)
  x_FULL_MEANVAR = as.matrix(round(qp.Result$solution,5), k, 1)
  
  #keep portfolio weights across time
  keepPortWeights_FULL_MEANVAR[[i]] = x_FULL_MEANVAR
  
  #calculate Out of Sample Returns
  RR_FULL_MEANVAR = dataReturns[insample+i,]%*%x_FULL_MEANVAR
  Realized_Ret_FULL_MEANVAR[i] = RR_FULL_MEANVAR
  
  #calculate Port Risk (portfolio standard deviation)
  PR_FULL_MEANVAR = sqrt(t(x_FULL_MEANVAR)%*%cov_mat%*%x_FULL_MEANVAR)   
  keep_PortRisk_FULL_MEANVAR[i] = PR_FULL_MEANVAR
  
  #calculate Conditional Sharp Ratio (CSR)
  CSR_FULL_MEANVAR = RR_FULL_MEANVAR/PR_FULL_MEANVAR
  keep_CSR_FULL_MEANVAR[i] = CSR_FULL_MEANVAR
}

#calculate Cumulative Returns
Cum_Realized_Ret_FULL_MINVAR = cumsum(Realized_Ret_FULL_MINVAR)
Cum_Realized_Ret_FULL_MEANVAR = cumsum(Realized_Ret_FULL_MEANVAR)

MINVAR.FULL = cbind(mean(Realized_Ret_FULL_MINVAR), 
                    mean(keep_PortRisk_FULL_MINVAR), 
                    Cum_Realized_Ret_FULL_MINVAR[outofsample],
                    mean(keep_CSR_FULL_MINVAR))

MEANVAR.FULL = cbind(mean(Realized_Ret_FULL_MEANVAR), 
                     mean(keep_PortRisk_FULL_MEANVAR), 
                     Cum_Realized_Ret_FULL_MEANVAR[outofsample],
                     mean(keep_CSR_FULL_MEANVAR))

Res.FULL = data.frame(round(rbind(MINVAR.FULL, MEANVAR.FULL), 4))
names(Res.FULL) = c("Mean Return", "Volatility", "Cumulative Return", "Conditional Sharpe Ratio")
row.names(Res.FULL) = c("Minimum Variance", "Mean Variance")
Res.FULL


keepPortWeights_FULL_MINVAR = do.call(cbind, keepPortWeights_FULL_MINVAR)
dim(keepPortWeights_FULL_MINVAR)
keepPortWeights_FULL_MINVAR

keepPortWeights_FULL_MEANVAR = do.call(cbind, keepPortWeights_FULL_MEANVAR)
dim(keepPortWeights_FULL_MEANVAR)
keepPortWeights_FULL_MEANVAR

round(apply(keepPortWeights_FULL_MINVAR, 1, mean),3)
round(apply(keepPortWeights_FULL_MEANVAR, 1, mean),3)


# Cumulative Portfolio Weights
plot(Cum_Realized_Ret_FULL_MEANVAR, main="Cumulative Portfolio Weights - FULL", type="l", col="#2b9348",ylab=NA,ylim=c(-0.031,0.30))
lines(Cum_Realized_Ret_FULL_MINVAR, type="l", col="darkorange1")
legend("topleft",
       legend=c("MEANVAR-FULL", "MINVAR-FULL"),
       lty=c(1,1), pch=c(NA, 16), col=c("#2b9348","darkorange1"))






#====================================================================================================


# CCC MODEL ---------------------------------------------------------------

t = dim(dataReturns)[1]
k = dim(dataReturns)[2]


# in-sample & out-of-sample period, target return
outofsample = 36           
insample = t - outofsample


# Empty matrices for the Results of MINIMUM VARIANCE PORTFOLIO
keep_PortRisk_CCC_MINVAR = NULL
keepPortWeights_CCC_MINVAR = NULL
Realized_Ret_CCC_MINVAR = NULL
Cum_Realized_Ret_CCC_MINVAR = NULL
keep_CSR_CCC_MINVAR = NULL

# Empty matrices for the Results of MEAN VARIANCE PORTFOLIO
keep_PortRisk_CCC_MEANVAR = NULL
keepPortWeights_CCC_MEANVAR = NULL
Realized_Ret_CCC_MEANVAR = NULL
Cum_Realized_Ret_CCC_MEANVAR = NULL
keep_CSR_CCC_MEANVAR = NULL

# Start the out of sample performance of the models
keep_meanvec_CCC = keep_covmat_CCC = NULL
for (i in 1:outofsample){
  print(i)
  dataanalyse = NULL
  dataanalyse = dataReturns[1:(insample+i-1),]
  datafactoranalyse = factorReturns[1:(insample+i-1),]
  
  
  T = dim(dataanalyse)[1]  
  N = dim(dataanalyse)[2] 
  
  #==========================================================================
  # Estimate Univariate GARCH models
  #==========================================================================
  standResid = matrix(0,T,N)   # store matrix of standardized residuals
  ResMod = matrix(0,T,N)       # store matrix of residuals
  CondSdMod = matrix(0,T,N)    # store conditional standard deviation matrix
  Yfor = rep(0,N)              # forecast of Y
  SDfor = rep(0,N)             # standard deviation forecasts
  
  for (j in 1:N){ 
    # Estimate univariate GARCH models
    m1garch = garchFit(~garch(1,1),data=dataanalyse[,j],trace=F) 
    # Compute the estimated residuals
    ResMod[,j] = m1garch@residuals
    CondSdMod[,j] = m1garch@sigma.t   
    # Compute forecasts
    Yfor[j] = predict(m1garch,1)$meanForecast
    SDfor[j] = predict(m1garch,1)$standardDeviation
    # Compute Standardized Residuals
    standResid[,j] = ResMod[,j]/CondSdMod[,j]
  }
  
  # Estimate Correlation matrix
  Rmat = cor(standResid)
  # Construct Covariance matrix of the Returns
  CovRet = diag(SDfor)%*%Rmat%*%diag(SDfor)
  # Compute Expected Returns
  ExpRet = Yfor
  
  
  # Calculate the corresponding mean and covariance matrices
  keep_meanvec_CCC[[i]] = ExpRet
  keep_covmat_CCC[[i]] = CovRet
  m_vec = keep_meanvec_CCC[[i]]
  cov_mat = keep_covmat_CCC[[i]]
  
  #=====================================================================
  # Find optimal Minimum Variance portfolio
  #=====================================================================
  # Set matrices with constraints
  D.mat = 2*cov_mat
  d.vec = rep(0, k)
  A.mat = cbind(rep(1,k), diag(k))
  b.vec = c(1, rep(0,k))
  # Solve the Quadratic Programming Problem
  qp.Result = solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=1)
  # x_CCC_MINVAR are the optimal portfolio weights (k x 1 vector)
  x_CCC_MINVAR = as.matrix(round(qp.Result$solution,5), k, 1)
  
  #keep portfolio weights across time
  keepPortWeights_CCC_MINVAR[[i]] = x_CCC_MINVAR
  
  #calculate Out of Sample Returns
  RR_CCC_MINVAR = dataReturns[insample+i,]%*%x_CCC_MINVAR
  Realized_Ret_CCC_MINVAR[i] = RR_CCC_MINVAR
  
  #calculate Port Risk (portfolio standard deviation)
  PR_CCC_MINVAR = sqrt(t(x_CCC_MINVAR)%*%cov_mat%*%x_CCC_MINVAR)   
  keep_PortRisk_CCC_MINVAR[i] = PR_CCC_MINVAR
  
  #calculate Conditional Sharp Ratio (CSR)
  CSR_CCC_MINVAR = RR_CCC_MINVAR/PR_CCC_MINVAR
  keep_CSR_CCC_MINVAR[i] = CSR_CCC_MINVAR
  
  #=====================================================================
  # Find optimal Mean Variance portfolio
  #=====================================================================
  # Set matrices with constraints
  D.mat = 2*cov_mat
  d.vec = rep(0, k)
  A.mat = cbind(rep(1,k), m_vec, diag(k))
  b.vec = c(1, target, rep(0,k))
  # Solve the Quadratic Programming Problem
  qp.Result = solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=1)
  # x_CCC_MEANVAR are the optimal portfolio weights (k x 1 vector)
  x_CCC_MEANVAR = as.matrix(round(qp.Result$solution,5), k, 1)
  
  #keep portfolio weights across time
  keepPortWeights_CCC_MEANVAR[[i]] = x_CCC_MEANVAR
  
  #calculate Out of Sample Returns
  RR_CCC_MEANVAR = dataReturns[insample+i,]%*%x_CCC_MEANVAR
  Realized_Ret_CCC_MEANVAR[i] = RR_CCC_MEANVAR
  
  #calculate Port Risk (portfolio standard deviation)
  PR_CCC_MEANVAR = sqrt(t(x_CCC_MEANVAR)%*%cov_mat%*%x_CCC_MEANVAR)   
  keep_PortRisk_CCC_MEANVAR[i] = PR_CCC_MEANVAR
  
  #calculate Conditional Sharp Ratio (CSR)
  CSR_CCC_MEANVAR = RR_CCC_MEANVAR/PR_CCC_MEANVAR
  keep_CSR_CCC_MEANVAR[i] = CSR_CCC_MEANVAR
}

#calculate Cumulative Returns
Cum_Realized_Ret_CCC_MINVAR = cumsum(Realized_Ret_CCC_MINVAR)
Cum_Realized_Ret_CCC_MEANVAR = cumsum(Realized_Ret_CCC_MEANVAR)

MINVAR.CCC = cbind(mean(Realized_Ret_CCC_MINVAR), 
                   mean(keep_PortRisk_CCC_MINVAR), 
                   Cum_Realized_Ret_CCC_MINVAR[outofsample],
                   mean(keep_CSR_CCC_MINVAR))

MEANVAR.CCC = cbind(mean(Realized_Ret_CCC_MEANVAR), 
                    mean(keep_PortRisk_CCC_MEANVAR), 
                    Cum_Realized_Ret_CCC_MEANVAR[outofsample],
                    mean(keep_CSR_CCC_MEANVAR))

Res.CCC = data.frame(round(rbind(MINVAR.CCC, MEANVAR.CCC), 4))
names(Res.CCC) = c("Mean Return", "Volatility", "Cumulative Return", "Conditional Sharpe Ratio")
row.names(Res.CCC) = c("Minimum Variance", "Mean Variance")
Res.CCC


keepPortWeights_CCC_MINVAR = do.call(cbind, keepPortWeights_CCC_MINVAR)
dim(keepPortWeights_CCC_MINVAR)
keepPortWeights_CCC_MINVAR

keepPortWeights_CCC_MEANVAR = do.call(cbind, keepPortWeights_CCC_MEANVAR)
dim(keepPortWeights_CCC_MEANVAR)
keepPortWeights_CCC_MEANVAR

round(apply(keepPortWeights_CCC_MINVAR, 1, mean),3)
round(apply(keepPortWeights_CCC_MEANVAR, 1, mean),3)


# Cumulative Portfolio Weights
plot(Cum_Realized_Ret_CCC_MEANVAR, main="Cumulative Portfolio Weights - CCC", type="l", col="#5a189a",ylab=NA,ylim=c(-0.031,0.30))
lines(Cum_Realized_Ret_CCC_MINVAR, type="l", col="#ff4d6d")
legend("topleft",
       legend=c("MEANVAR-CCC", "MINVAR-CCC"),
       lty=c(1,1), pch=c(NA, 16), col=c("#5a189a","#ff4d6d"))










# GGPLOT ------------------------------------------------------------------


# performance evaluation
cumretsSR = data.frame(cumRet_SharpeRatio)
colors = c("cumRet_SharpeRatio" = "black", "cumRet_TreynorRatio" = "magenta", 
           "cumRet_SortinoRatio" = "blue", "cumRet_JensensAlpha" = "orange", 
           "cumRet_JensensAlphaMult" = "chartreuse4")
ggplot(cumretsSR, aes(x = seq(1, length(cumRet_SharpeRatio)))) + 
  geom_line(aes(y = cumRet_SharpeRatio, color="cumRet_SharpeRatio")) +
  geom_line(aes(y = cumRet_TreynorRatio, color = "cumRet_TreynorRatio")) + 
  geom_line(aes(y = cumRet_SortinoRatio, color = "cumRet_SortinoRatio")) +
  geom_line(aes(y = cumRet_JensensAlpha, color = "cumRet_JensensAlpha")) +
  geom_line(aes(y = cumRet_JensensAlphaMult, color = "cumRet_JensensAlphaMult")) +
  labs(x = "Index",
       y = "Cumulative Returns",
       color = "Legend") +
  scale_color_manual(values = colors,
                     labels = c("Jensen's Alpha", "Jensen's Multiple", "Sharpe", "Sortino", "Treynor" )) +
  theme_minimal() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14))



# portfolio construction

# sample estimates
CumRealizedRet.MEANVAR = data.frame(Cum_Realized_Ret_static_MEANVAR)
colors2 = c("Cum_Realized_Ret_static_MEANVAR" = "blue", 
            "Cum_Realized_Ret_static_MINVAR" = "red")
ggplot(CumRealizedRet.MEANVAR, aes(x = seq(1, length(Cum_Realized_Ret_static_MEANVAR)))) + 
  geom_line(aes(y = Cum_Realized_Ret_static_MEANVAR, color="Cum_Realized_Ret_static_MEANVAR")) +
  geom_line(aes(y = Cum_Realized_Ret_static_MINVAR, color="Cum_Realized_Ret_static_MINVAR")) +
  labs(x = "Index",
       y = "Cumulative Returns",
       color = "Legend") +
  scale_color_manual(values = colors2,
                     labels = c("Mean Variance", "Minimum Variance")) +
  theme_minimal() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14))



# sim
CumRealizedRet.SIM.MEANVAR = data.frame(Cum_Realized_Ret_SIM_MEANVAR)
colors3 = c("Cum_Realized_Ret_SIM_MEANVAR" = "black", 
            "Cum_Realized_Ret_SIM_MINVAR" = "magenta")
ggplot(CumRealizedRet.SIM.MEANVAR, aes(x = seq(1, length(Cum_Realized_Ret_SIM_MEANVAR)))) + 
  geom_line(aes(y = Cum_Realized_Ret_SIM_MEANVAR, color="Cum_Realized_Ret_SIM_MEANVAR")) +
  geom_line(aes(y = Cum_Realized_Ret_SIM_MINVAR, color="Cum_Realized_Ret_SIM_MINVAR")) +
  labs(x = "Index",
       y = "Cumulative Returns",
       color = "Legend") +
  scale_color_manual(values = colors3,
                     labels = c("Mean Variance", "Minimum Variance")) +
  theme_minimal() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14))



# full
CumRealizedRet.FULL.MEANVAR = data.frame(Cum_Realized_Ret_FULL_MEANVAR)
colors4 = c("Cum_Realized_Ret_FULL_MEANVAR" = "#2b9348", 
            "Cum_Realized_Ret_FULL_MINVAR" = "darkorange1")
ggplot(CumRealizedRet.FULL.MEANVAR, aes(x = seq(1, length(Cum_Realized_Ret_FULL_MEANVAR)))) + 
  geom_line(aes(y = Cum_Realized_Ret_FULL_MEANVAR, color="Cum_Realized_Ret_FULL_MEANVAR")) +
  geom_line(aes(y = Cum_Realized_Ret_FULL_MINVAR, color="Cum_Realized_Ret_FULL_MINVAR")) +
  labs(x = "Index",
       y = "Cumulative Returns",
       color = "Legend") +
  scale_color_manual(values = colors4,
                     labels = c("Mean Variance", "Minimum Variance")) +
  theme_minimal() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14))



# ccc
CumRealizedRet.CCC.MEANVAR = data.frame(Cum_Realized_Ret_CCC_MEANVAR)
colors5 = c("Cum_Realized_Ret_CCC_MEANVAR" = "#5a189a", 
            "Cum_Realized_Ret_CCC_MINVAR" = "#ff4d6d")
ggplot(CumRealizedRet.CCC.MEANVAR, aes(x = seq(1, length(Cum_Realized_Ret_CCC_MEANVAR)))) + 
  geom_line(aes(y = Cum_Realized_Ret_CCC_MEANVAR, color="Cum_Realized_Ret_CCC_MEANVAR")) +
  geom_line(aes(y = Cum_Realized_Ret_CCC_MINVAR, color="Cum_Realized_Ret_CCC_MINVAR")) +
  labs(x = "Index",
       y = "Cumulative Returns",
       color = "Legend") +
  scale_color_manual(values = colors5,
                     labels = c("Mean Variance", "Minimum Variance")) +
  theme_minimal() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14))


# all
colors5 = c("Cum_Realized_Ret_static_MEANVAR" = "blue", "Cum_Realized_Ret_static_MINVAR" = "red", 
            "Cum_Realized_Ret_SIM_MEANVAR" = "black", "Cum_Realized_Ret_SIM_MINVAR" = "magenta", 
            "Cum_Realized_Ret_FULL_MEANVAR" = "#2b9348", "Cum_Realized_Ret_FULL_MINVAR" = "darkorange1",
            "Cum_Realized_Ret_CCC_MEANVAR" = "#5a189a", "Cum_Realized_Ret_CCC_MINVAR" = "#ff4d6d")

ggplot(CumRealizedRet.MEANVAR, aes(x = seq(1, length(Cum_Realized_Ret_static_MEANVAR)))) + 
  geom_line(aes(y = Cum_Realized_Ret_static_MEANVAR, color="Cum_Realized_Ret_static_MEANVAR")) +
  geom_line(aes(y = Cum_Realized_Ret_static_MINVAR, color="Cum_Realized_Ret_static_MINVAR")) +
  geom_line(aes(y = Cum_Realized_Ret_SIM_MEANVAR, color="Cum_Realized_Ret_SIM_MEANVAR")) +
  geom_line(aes(y = Cum_Realized_Ret_SIM_MINVAR, color="Cum_Realized_Ret_SIM_MINVAR")) +
  geom_line(aes(y = Cum_Realized_Ret_FULL_MEANVAR, color="Cum_Realized_Ret_FULL_MEANVAR")) +
  geom_line(aes(y = Cum_Realized_Ret_FULL_MINVAR, color = "Cum_Realized_Ret_FULL_MINVAR")) + 
  geom_line(aes(y = Cum_Realized_Ret_CCC_MEANVAR, color = "Cum_Realized_Ret_CCC_MEANVAR")) +
  geom_line(aes(y = Cum_Realized_Ret_CCC_MINVAR, color = "Cum_Realized_Ret_CCC_MINVAR")) +
  labs(x = "Index",
       y = "Cumulative Returns",
       color = "Legend") +
  scale_color_manual(values = colors5,
                     labels = c("Mean CCC", "Minimum CCC", "Mean FULL","Minimum FULL",
                                "Mean SIM","Minimum SIM","Mean sample","Minimum sample")) +
  theme_minimal() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14))

