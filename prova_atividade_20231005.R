# =-==========================================-=
# AVALIAÇÃO/ATIVIDADE MATD46 05/10/2023
# =-==========================================-=

set.seed(123)

# AVALIAÇÃO ----
## Questão 1 ----
### Item a ----
dAbs <- function(n){
  x = base::vector()
    u = runif(n,-3,3)
    x = ifelse(-2<=u && u<=2, abs(u/4), abs(u/4))
return(x)
}

amostra = dAbs(100)

x <- c(-12:12)
plot(abs(x/4),type = "l")
plot(amostra,type = "l")

## Item b ----

## Questão 2 ----

h <- function(x){
  num = 30*x^3*(1-x)^2
  den = 1 # uniforme (0,1)
  r = num/den
  return(r)
}


df <- function(n) {
  x <- numeric(n)
  i <- 1
  while (i <= n) {
    u <- runif(1)
    y <- runif(1)
    if (y <= h(u)) {
      x[i] <- u
      i <- i + 1
    }
  }
  return(sample)
}

amostra = df(1000)
hist(amostra)


# CORREÇÃO ----
## Q1 ----
sim1 <- function(n){
  x <- vector()
  for (i in 1:n) {
    u <- runif(1)
    # if(u>=0 & u<=0.5){
    if(u<0.5){
      aux1 = sqrt(-8*(u-0.5))
      x <- c(x,-aux1)
    }else{
      aux2 = sqrt(8*(u-0.5))
      x <- c(x,aux2)
    }
  }
  return(x)
}

amostra <- sim1(10000)
hist(amostra,freq = F)

# Aula 10/10/23 ----
## Ex1 ----
sim2 <- function(n,p){
  u <- runif(n)
  aux <- log(1-u)/log(1-p)
  x <- base::floor(aux)+1
  return(x)
}

amostra <- sim2(n=1000,p=0.3)
plot(prop.table(table(amostra)))

## Ex2 ----
h <- function(x){
  l <- 30*x^3*(1-x)^2
  return(l)
}

sim3 <- function(n){
  aux <- optimize(h,c(0,1), maximum = T)
  c <- aux$objective
  i <- 1
  x <- vector()
  while (i<=n) {
    x.c <- runif(1)
    alpha <- (1/c)*h(x.c)
    u <- runif(1)
    if(u<alpha){
      x <- c(x,x.c)
      i=i+1
    }
  }
  return(x)
}

sim3 <- function(n){
  aux <- optimize(h,c(0,1), maximum = T)
  c <- aux$objective
  i <- 1
  x <- vector()
  rej <- vector()
  while (i<=n) {
    x.c <- runif(1)
    alpha <- (1/c)*h(x.c)
    u <- runif(1)
    if(u<alpha){
      x <- c(x,x.c)
      i=i+1
    }
    rej <- c(rej,x.c)
  }
  result <- list()
  result$x <- x
  result$y <- rej
  
  return(result)
}

sim3 <- function(n){
  aux <- optimize(h,c(0,1), maximum = T)
  c <- aux$objective
  i <- 1
  x <- vector()
  rej <- vector()
  while (i<=n) {
    x.c <- runif(1)
    alpha <- (1/c)*h(x.c)
    u <- runif(1)
    if(u<alpha){
      x <- c(x,x.c)
      i=i+1
    }else{
      rej <- c(rej,x.c)
    }
  }
  result <- list()
  result$x <- x
  result$y <- rej
  
  return(result)
}

set.seed(123)
amostra <- sim3(10)
amostra
hist(amostra, freq = F)

x.seq <- seq(from=0, to=1, by=0.01)
h.x <- h(x.seq)

plot(x.seq,h.x,type = "l")

plot(amostra$x,amostra$y,type = "l")


# plot(amostra$x,type = "l")
plot(amostra$x,type = "p")
points(amostra$y, col="red", pch=16)

plot(amostra$x,type = "l")


amostra <- sim3(1000)
hist(amostra$x, freq = F)
lines(amostra$y)

# =================================-=
# A linha azul eu plotei assim:
  x = seq(0, 1, length = 1000)

t = function(x){
  f = 30*x^3*(1-x)^2
  return(f)
}

f = t(x)
lines(x, f, col = "blue", lwd = 2)
# =================================-=
