#______________________________________
# MATD46 - Estatística Computacional A
# Prof. Jalmar Carrasco
#______________________________________

# Comportamento assintótico dos estimadores de máxima verossimilhança

source("Codigos/OPTIM-Distribuicao Exponencial Exponencial.R")

simula <- function(theta,R,n){
  
  mTheta <- matrix(NA, nrow=R,ncol=length(theta))

  r <- 1
  while(r <=R){
    sample <- rEE(n=n,theta=theta)
    op <- optim(par=theta,fn=lvero.EE,method="BFGS",x=sample)
    mTheta[r,] <- op$par
    r <- r+1
  }
  
  result <- list()
  result$Mean <- apply(mTheta,2,mean)
  return(result)
}

simula(theta=c(5,2),R=100,n=100)
