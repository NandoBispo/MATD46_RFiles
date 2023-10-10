# =-==========================================-=
# AVALIAÇÃO/ATIVIDADE MATD46 
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



