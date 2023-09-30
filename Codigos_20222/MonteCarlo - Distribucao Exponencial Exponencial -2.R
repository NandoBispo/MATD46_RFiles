#______________________________________
# MATD46 - Estatística Computacional A
# Prof. Jalmar Carrasco
#______________________________________

# Comportamento assintótico dos estimadores de máxima verossimilhança

source("Codigos/OPTIM-Distribuicao Exponencial Exponencial.R")

simula <- function(theta,R,n){
  
  mTheta <- matrix(NA, nrow=R,ncol=length(theta))
  mBias <- matrix(NA, nrow=R,ncol=length(theta))
  mMSE <- matrix(NA, nrow=R,ncol=length(theta))
  
  r <- 1
  cont <- 1
  while(r <=R){
    sample <- rEE(n=n,theta=theta)
    op <- optim(par=theta,fn=lvero.EE,method="BFGS",x=sample)
    mTheta[r,] <- op$par
    mBias[r,] <- op$par-theta
    mMSE[r,] <- (op$par-theta)^2
    r <- r+1
    cont <- cont+1
  }
  
  result <- list()
  result$Mean <- apply(mTheta,2,mean)
  result$Bias <- apply(mBias,2,mean)
  result$MSE <- apply(mMSE,2,mean)
  result$TotalReplicas <- cont-1
  return(result)
}

simula(theta=c(5,2),R=1000,n=1000)
