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
  mTC <- matrix(NA, nrow=R,ncol=length(theta))
  
  r <- 1
  cont <- 1
  while(r <=R){
    sample <- rEE(n=n,theta=theta)
    
    op <- optim(par=theta,fn=lvero.EE,method="BFGS",x=sample,hessian = TRUE)
    
    ep <- sqrt(diag(solve(op$hessian)))
    Li <- op$par-1.96*ep
    Ls <- op$par+1.96*ep
    
    mTheta[r,] <- op$par
    mBias[r,] <- op$par-theta
    mMSE[r,] <- (op$par-theta)^2
    
    for(j in 1:length(theta)){
      if(theta[j]>Li[j] & theta[j]<Ls[j]){mTC[r,j]<-1}else{mTC[r,j]<-0}
    }
    
    r <- r+1
    cont <- cont+1
  }
  
  result <- list()
  Mean <- apply(mTheta,2,mean)
  Bias <- apply(mBias,2,mean)
  MSE <- apply(mMSE,2,mean)
  TC <- apply(mTC,2,mean)*100
  
  aux <- rbind(Mean,Bias,MSE,TC)
  colnames(aux) <- c("Alpha","Lambda")
  rownames(aux) <- c("Mean","Bias","MSE","TC")
  
  result$Results <- as.table(round(aux,digits = 2))
  result$TotalReplicas <- cont-1
  return(result)
}

simula(theta=c(5,2),R=1000,n=100)
