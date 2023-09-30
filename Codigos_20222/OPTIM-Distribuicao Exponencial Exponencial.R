#______________________________________
# MATD46 - Estatística Computacional A
# Prof. Jalmar Carrasco
#______________________________________

# Maximização da função de verosimilhança

rEE <- function(n,theta){
  alpha <- theta[1]
  lambda <- theta[2]
  u <- runif(n,0,1)
  aux <- log(1-(u^(1/alpha)))
  x <- (-1/lambda)*aux
  return(x)
}

sample <- rEE(n=5000,theta=c(10,1))
hist(sample,prob=T)

dEE <- function(x,theta,log=FALSE){
  alpha <- theta[1]
  lambda <- theta[2]
  aux1 <- exp(-lambda*x)
  aux2 <- (1-aux1)^(alpha-1)
  fun <- alpha*lambda*aux1*aux2
  if(log==TRUE){return(log(fun))}else{return(fun)}
}

lvero.EE <- function(theta,x){
  alpha <- theta[1]
  lambda <- theta[2]
  func <- dEE(x=x,theta=theta,log=TRUE)
  return(-sum(func))
}

op <- optim(par=c(5,2),fn=lvero.EE,method="BFGS",x=sample)
