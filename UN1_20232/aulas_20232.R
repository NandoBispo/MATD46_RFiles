# AULAS ----



# 19/09/23 -----

# Função de densidade
dBetaExp <- function(x,theta,log=FALSE){
  lambda <- theta[1]
  a <- theta[2]
  b <- theta[3]
  
  aux <- exp(-lambda*x)
  
  f <- (lambda/beta(a,b))*(aux^b)*(1-aux)^(a-1)
  
  if(log=="FALSE"){return(f)}else{return(log(f))}
}


# Análise 1

source("funcoes.R")

x = seq(0,6,0.01)
y1 = dBetaExp(x,theta=c(1,0.5,1))
y2 = dBetaExp(x,theta=c(1,2,1))
y3 = dBetaExp(x,theta=c(1,5,1))

graphics::plot(x,y1,type="l",col=2,ylim=c(0,1))
points(x,y2,type="l",col=3)
points(x,y3,type="l",col=4)

# Gerador de números aleatórios

rBetaExp <- function(n,theta){
  lambda <- theta[1]
  a <- theta[2]
  b <- theta[3]
  
  nu <- rbeta(n,shape1=a,shape2=b)
  
  x <- (-1/lambda)*log(1-nu)
}
  

# Análise 2

source("funcoes.R")

amostra <- rBetaExp(n=1000, theta=c(1,2,1))
hist(amostra, freq = T)
hist(amostra, freq = F)


# Caso discreto
rBernoulli <- function(n,pi){
  x <- vector()
  for (i in 1:n) {
    u <- runif(1,0,1)
    if(u < pi){x <- c(1,x)}else{x <- c(0,x)}
  }
  return(x)
}
    

# Análise 3

amostra <- rBernoulli(n=100,pi=0.70)

base::prop.table(table(amostra))

plot(base::prop.table(table(amostra)))


# 26/09/23 -----

# sim1.unif <- function(n,k){
#   x <- base::vector()
#   for(i in 1:n){
#     u <- runif(n=1,min=0,max=1)
#     aux <- 1+floor(x=u*k)
#     x <- c(x,aux)
#   }
#   return(x)
# }

sim1.unif <- function(n,k){
  x <- base::vector() # Vetor vazio.
  for(i in 1:n){
    u <- runif(n=1,min=0,max=1)
    j <- 1+floor(x=u*k)
    x <- c(x,j)
  }
  return(x)
}

amostra1 <- sim1.unif(n=10,k=5)
plot(prop.table(table(amostra1)))
# ___________________________________________________________

sim2.unif <- function(n,k){
  x <- base::vector()
  pcum <- c((1:k)/k)
  for(i in 1:n){
    u <- runif(n=1,min=0,max=1)
    for(j in 1:k){
      if(isTRUE((j-1)/k<u && u<=j/k)){t <- j}
    }
    x <- c(x,t)  
  }
  return(x)
}

amostra2 <- sim2.unif(n=10,k=5)
amostra2
plot(prop.table(table(amostra2)))

# ___________________________________________________________

sim3.Binomial <- function(N,n,p){
  x <- vector()
  for(i in 1:N){
    y <- vector()
    for(j in 1:n){
      u <- runif(1,0,1)
      ifelse(u<p, #se u<p então:
             y <- c(y,1), # y <- sucesso, caso contrário:
             y <- c(y,0) # y <- fracasso
             )
    }
    x <- c(x,sum(y))
  }
  return(x)
}

# E(X) = np
# Var(X) = np(1-p)

amostra3 <- sim3.Binomial(N=100,n=5,p=0.5)
plot(prop.table(table(amostra3)))
# E(amostra3) = 5*0.5 = 2.5

amostra3 <- sim3.Binomial(N=1000,n=25,p=0.5)
plot(prop.table(table(amostra3)))
# E(amostra3) = 25*0.5 = 12.5

# 28/09/23 -----
sim.normal <- function(n){
  x <- vector()
  i = 1
  while(i <= n){
    u <- runif(1)
    z.star <- -log(1-u)
    
    aux1 <- exp(-0.5*z.star^2*z.star)
    aux2 <- sqrt(1/exp(1))
    alpha <- aux2*aux1
    
    nu <- runif(1)
    
    if(alpha<=nu){
      z = z.star
      u.star <- runif(1)
      ifelse(u.star<0.5, x <- c(x,z), x <- c(x,-z))
      i = i+1
    }
  }
  return(x)
}

amostra <- sim.normal(n=1e4)  
hist(amostra, freq=F)
M <- sqrt(2*exp(1)/pi)
1/M
mu = 65
sigma = 5

mu = 65
sigma = 50

amostra2 <- mu+sigma*amostra
hist(amostra2)

# 03/10/23 ----
# random.Poisson

x <- 0:15
y <- dpois(x,lambda=3,log = F)
g <- dexp(x,1/3)

# plot(table(x,y))
plot(y,type = "h",ylim=c(0,0.3))
graphics::lines(x,g, col="red")

?stats::optimize

h <- function(x){
  num <- (3^(x+1))*exp((x/3)-3)
  den <- factorial(x)
  r <- num/den
}

op <- stats::optimize(h,c(0,15),maximum = T)
op
# "maximum" é a raiz da função.
# "objective" é a função h aplicada no ponto de otimização.

M <- op$objective

# Geração d enúmeros aleatórios da dist. Poisson comparâmetro lambda=3

# Funções.R
sim.Poison <- function(n){
  x <- vector()
  op <- stats::optimize(h,c(0,15),maximum = T)
  M <- op$objective
  i=1
  cont=0 # contador
  while (i <= n) {
    cont=cont+1
    u1 <- runif(1,0,1)
    yc <- -3*log(u1)
    alpha <- (1/M)*h(yc)
    u2 <- runif(1,0,1)
    if(u2<=alpha){
      x <- c(x,base::floor(yc))
      i=i+1
    }
  }
  resultados <- base::list()
  resultados$amostra <- x
  resultados$contador <- cont
  resultados$taxa <- n/cont
  return(resultados)
  # return(x)
}

# Análise.R
# source("funcoes.R")

set.seed(123)
amostra <- sim.Poison(100)
amostra

plot(prop.table(table(amostra)),ylab="")


1-100/183

# De 100 amostras, 183 foram geradas, logo a eficiencia empirica = n/contador = 100/183


# EXEMPLO 2:
?dgamma # para verificar a fç de distribuição.

(3/2*gamma(3/2))*sqrt(3/2)*(1/sqrt(exp(1)))

