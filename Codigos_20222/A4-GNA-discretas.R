#______________________________________
# MATD46 - Estatística Computacional A
# Prof. Jalmar Carrasco
#______________________________________

# Seja X com f.d.p f(x)=exp(x)/(e-1), 0<=x<=1
# n = Tamanho da amostra
# x = Vetor que recebe os valores a amostra 

simx <- function(n){
  x <- vector()
  for(i in 1:n){
    u <- runif(1,0,1)
    aux <- log(u*(exp(1)-1)+1)
    x <- c(x,aux)
  }
  x
}

set.seed(123)
x <- simx(10000)
hist(x,freq = F)

dx <- function(x){exp(x)/(exp(1)-1)}
curve(dx,add=T,lwd=3,lty=2)

# Distribuição Uniforme discreta

# Algoritmo 1

simunif <- function(n,k){
  x <- vector()
  pcum <- c((1:k)/k)
  for(i in 1:n){
    u <- runif(1,0,1)
    for(j in 1:length(pcum)){
      if(isTRUE((j-1)/k<=u && u<j/k)){t<-j}
    }
    x <- c(x,t)
  }
  x
}

# Algoritmo 2

simunif2 <- function(n,k){
  x <- vector()
  for(i in 1:n){
    u <- runif(1,0,1)
    x <- c(x,1+floor(k*u))
  }
  x
}

set.seed(123)
simunif(10,9)

set.seed(123)
simunif2(10,9)

library(extraDistr)
set.seed(123)
rdunif(10,1,9)

library(arm)

n <- 1000
x <- simunif(n,9)

# Gráfico de histograma
discrete.histogram(x)

library(extraDistr)
lines(1:9,ddunif(1:9,1,9),lwd=1.5,lty=2)
points(1:9,ddunif(1:9,1,9),lwd=2.5,pch=19)
box(lwd=2)

# Distribuição Bernoulli

simber <- function(n,p){
  x <- vector()
  for(i in 1:n){
    u <- runif(1,0,1)
    ifelse(u<1-p,x<-c(x,0),x<-c(x,1))
  }
  x
}

set.seed(123)

x <- simber(1000,0.3)
discrete.histogram(x)
lines(0:1,dbern(0:1,0.3))
points(0:1,dbern(0:1,0.3),lwd=3,pch=19)
box(lwd=2)

# Distribuição binomial
# N = Tamanho da amostra Binomial
# x = vetor que recebe os valores da amostra

simbin <- function(N,n,p){
  x <- vector()
  for(i in 1:N){
    y <- vector()
    for(k in 1:n){
      u <- runif(1,0,1)
      ifelse(u<1-p,y<-c(y,0),y<-c(y,1))
    }
    x <- c(x,sum(y))
  }
  x
}

set.seed(123)
x <- simbin(N=1000,n=10,p=0.3)

discrete.histogram(x)
lines(0:10,dbinom(0:10,10,0.3))
points(0:10,dbinom(0:10,10,0.3),pch=19)
box(lwd=2)
