#______________________________________
# MATD46 - Estatística Computacional A
# Prof. Jalmar Carrasco
#______________________________________

# GENERALIZED EXPONENTIAL DISTRIBUTION: DIFFERENT METHOD
# OF ESTIMATIONS
# J. Statist. Comput. Simul., 2000

# Funções
# Geração de números aleatórios de uma distribução GE

rGE <- function(n,alpha,lambda){
  stopifnot(alpha > 0 && lambda >0)
  u <- runif(n)
  x.aux <- (-1/lambda)*log(1-u^(1/alpha))
  return(x.aux)
}

# Função de densidade

dGE <- function(x,alpha,lambda){
  stopifnot(alpha > 0 && lambda >0)
  aux <- exp(-lambda*x)
  fun <- alpha*lambda*aux*(1-aux)^(alpha-1)
  return(fun)
}

# Função de sobrevivência

sGE <- function(x,alpha,lambda){
  stopifnot(alpha > 0 && lambda >0)
  aux.x <- exp(-lambda*x)
  fun <- 1-(1-aux.x)^alpha
  return(fun)
}

# Função de taxa de falha

hGE <- function(x,alpha,lambda){
  stopifnot(alpha > 0 && lambda >0)
  den <- dGE(x=x,alpha=alpha,lambda=lambda)
  num <- sGE(x=x,alpha=alpha,lambda=lambda)
  fun <- den/num
  return(fun)
}

amostra <- rGE(n=10,alpha=1,lambda=0.3)
head(amostra)
hist(amostra, prob=T)

y <- dGE(amostra,alpha=1,lambda = 0.3)
y

amostra.ord <- sort(amostra)
points(amostra.ord,dGE(amostra.ord,alpha=1,
                       lambda = 0.3),type="l")

xx <- seq(0,10,length=100)
plot(xx,hGE(xx,alpha=1,lambda=0.3),
     ylab=expression(h(x,alpha,lambda)),
     xlab = "x",type="l",ylim=c(0,0.7))
points(xx,hGE(xx,alpha=5,lambda=0.3),type="l")
points(xx,hGE(xx,alpha=10,lambda=0.3),type="l")
points(xx,hGE(xx,alpha=15,lambda=0.3),type="l")
points(xx,hGE(xx,alpha=0.5,lambda=0.3),type="l")
points(xx,hGE(xx,alpha=0.1,lambda=0.3),type="l")
