#_____________________________________
# MATD46 - Estatística Computacional A
# Prof. Jalmar Carrasco
#_____________________________________

# PACOTES ----
if (require(pacman)){
  library(pacman)
} else {
  install.packages("pacman")
}

pacman::p_load(janitor, kableExtra, tidyverse)
pacman::p_load(extraDistr, Rlab, arm, Broom)

# LAB 01 - 23/08/22 ----

# Ex1: Criar uma função que calcula o fatorial de um número real.

fat <- function(n){
  if(n == 0){
    print(1)
  }
  else{
    x <- 1
    for (i in 1:n-1) {
      
      x <- x*(n-i)
    }
    print(x)
  }
}

fat(6)

factorial(6)

# Ex2: Criar uma função que soma e subtrai 2 n° complexos.

(2+5i)+(3+5i)

complexo <- function(a,b,c,d){
  x <- (a+bi)
  y <- c + bi
  
  w <- x + y
  z <- x - y
  
  # return(c(x + y, x - y))
  return(c(w, z))
}

complex()
complexo(3,5,2,2)

# LAB 02 - 25/08/22 ----
# GNA - Gerador de Números Aleatórios UNIFORME

hist(runif(1000))

GNA.unif <- function(n, u0){
  i <- 1 # Apesar de informar no início que 1 = 0, no R deve-se iniciar com 1.
  tmp <- numeric() # Declara/define a variável como numerico
  if(u0 < 0 || u0 > 1){
    print("Informe um número de u0 entre 0 e 1")
    break
  }else{
    while(i<=n){
      tmp[i] <- 0.5+0.5*sin(2*pi*u0)
      u0 <- tmp[i]
      i = i + 1
    }
    return(tmp)
  }
}

r <- GNA.unif(n=1000,u0=.3)

hist(x)

xc <- cut(x, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
xc2 <- cut(r, breaks = seq(0, 1, 0.1), include.lowest = TRUE)

table(xc)

xc|>
  janitor::tabyl()|>
  janitor::adorn_pct_formatting(digits = 2)


janitor::chisq.test(x = table(xc))
  

ks.test(x, "punif")
ks.test(r, "punif")

# Para o teste em x: Não rejeita a hipótese nula de que a distribuição 
# acumulada empírica segue uma uniforme.


# Visualiza??o da distribui??o acumulada emp?rica da sequ?ncia gerada (linha preta) com uma distribui??o acumulada de uma uniforme gerada pela fun??o punif() (linha vermelha - usada como refer?ncia).


plot(ecdf(x))
plot(ecdf(punif(seq(0, 1, length.out = 1000))),
     add = TRUE, col = 2)

plot(ecdf(r))
plot(ecdf(punif(seq(0, 1, length.out = 1000))),
     add = TRUE, col = 2)

## Desafio Slide 2 ----

# https://stackoverflow.com/questions/54779922/how-to-generate-random-numbers-using-mid-square-method
# https://en.wikipedia.org/wiki/Middle-square_method

vonNeumann <- function(u0, n){
  vet <- numeric()
  i <- 0
  while(i<(n)){
    valor <- u0 * u0
    Y = as.numeric(unlist(strsplit(as.character(valor),split="")))
    Y = c(rep(0,8 - length(Y)), Y)
    P=Y[3:6]
    u0 = as.numeric(paste(P,collapse= ""))
    vet <- c(vet, u0)
    # u0 <- sample[i]
    i <- i+1
  }
  return(sprintf("%04d", vet))
}

vonNeumann(1234, 50)

strsplit(as.character(1234),split="")

Y = as.numeric(unlist(strsplit(as.character(1234),split="")))
X <- c(rep(0,8 - length(Y)), Y)
X[3:6]
as.numeric(paste(X[3:6],collapse= ""))
as.numeric(X[3:6])

# midSquareRand <- function(seed, len) {
#   randvector <- NULL
#   for(i in 1:len) {
#     value <- seed * seed   
#     Y=as.numeric(unlist(strsplit(as.character(value),split="")))
#     Y = c(rep(0,8 - length(Y)), Y)
#     P=Y[3:6]
#     seed=as.numeric(paste(P,collapse= ""))
#     randvector <- c(randvector,seed)
#   }
#   return(sprintf("%04d", randvector))
# }
# 
# 
# midSquareRand(1234, 50)

# LAB 03 - 01/09/22 ----
# GNA - Gerador de Números Aleatórios NÃO UNIFORME

# plot(ecdf(x)) - ecdf = empiric cumulative distribuition function

pacman::p_load(tidyverse)

# Função 
rGE <- function(n, alpha, lambda){
  u <- runif(n)
  fun <- (-1/lambda)*log(1-u^(1/alpha))
  return(fun)
} #rGE: Randon General Exponencial

hist(rGE(n = 1000, alpha = 0.5, lambda = 1.5))
round()

signif(k, digits = 4)

# Função de Densidade
dGE <- function(x, alpha, lambda){
  aux1 <- exp(-lambda*x)
  aux2 <- (1-aux1)^(alpha-1)
  fun <- lambda*alpha*aux1*aux2
  
  # fun <- (lambda*alpha*exp(-lambda*x))*(1-exp(-lambda*x)^(alpha-1))
  # fun <- lambda*alpha*aux1*(1-aux1)^(alpha-1)
  return(fun)
}

xx <- seq(0, 10, length = 100)

fx1 <- dGE(x = xx, alpha = 1, lambda = 0.5)
fx2 <- dGE(x = xx, alpha = 1, lambda = 1.0)
fx3 <- dGE(x = xx, alpha = 1, lambda = 1.5)

plot(xx, fx1, type = "l", col = 1)
points(xx, fx2, type = "l", col = 2)
points(xx, fx3, type = "l", col = 3)


fx1 <- dGE(x = xx, alpha = 1, lambda = 1)
fx2 <- dGE(x = xx, alpha = 2, lambda = 1)
fx3 <- dGE(x = xx, alpha = 3, lambda = 1)


plot(xx, fx1, type = "l", col = 1)
points(xx, fx2, type = "l", col = 2)
points(xx, fx3, type = "l", col = 4)

y <- rGE(n=1000, alpha=2,lambda=1)
y.ord <- sort(y)
fy <- dGE(x=y.ord, alpha = 2, lambda = 1)

hist(y.ord, prob=T, ylim = c(0, 0.6))
points(y.ord, fy, type="l")

sur <- function(x, alpha, lambda){
  aux <- exp(-lambda*x)
  fun <- 1-(1-aux^alpha)
  
  return(fun)
}

install.packages("sos")
install.packages("???")

# Px aula: Mostrar o artigo no R, criar o pacote com as funções do artigo,
# var randomicas, densidade, sobrevivencia, falha.

### Exercícios Slide 3 ----

n <- 1000
u <- runif(n)
x <- 1+sqrt(1-u)
hist(x, prob = TRUE)
# curve(3 * x^2, from = 0, to = 1, add = TRUE, col = 2)
plot(ecdf(x))
curve(1-(x-1)^2, from = 0, to = 1, add = TRUE, col = 2)
legend("left", legend = c("Empírica", "Teórica"),
       lty = 1, col = 1:2, bty = "n")


x <- u/exp(theta)

a=12.4556785

# display decimal places upto 3
(format(round(20.25, 4), nsmall = 4))



# LAB 0 - 29/09/22 ----

rExp <- function(n, theta){
  u <- runif(n) #r = randon
  x <- (-1/theta)*log(1-u)
  return(x)
}

dExp <- function(x, theta, log = F){
  f <- theta*exp(-theta*x) #d = densidade
  if(log == "F"){return(f)}else{return(log(f))}
}

lvero.Exp <- function(theta, x){
  f <- dExp(x=x, theta, log = T)
  return(-sum(f))
}

simula <- function(theta, R, n){
  r <- 1
  vTheta <- numeric()
  vBias <- numeric()
  vEQM <- numeric()
  while(r <= (R+1)){
    pAmostra <- rExp(n=n, theta=theta)
    op <- optim(par=theta, lvero.Exp, method = "BFGS", x = pAmostra)
# *par = é o chute inicial
    vTheta[r] <- op$par
    vBias[r] <- op$par-theta
    vEQM[r] <- (op$par-theta)^2
    r <- r+1
  }
  mTheta <- mean(vTheta) #m = media
  mBias <- mean(vBias)
  mEQM <- mean(vEQM)
  return(round(rbind(mTheta, mBias, mEQM), digits = 4))
  # return(mTheta)
  # return(vBias)
  # return(vEQM)
}

amostra <- rExp(n = 1000, theta = 0.3)
head(amostra)

optim(par = 0.2, lvero.Exp, method = "BFGS", x = amostra)
simula(theta = 0.3, R = 1000, n = 500)

# ATENÇÃO!!! Pesquisar função try

try

# Slide A4 ----
# Códigos do slide A4

simunif <- function(n, K){
  x <- vector()
  pcum <- c((1:K)/K)
  for(i in 1:n){
    u <- runif(1, 0, 1)
    for(j in 1:(length(pcum))){
      if(isTRUE((j-1)/K <= u && u < j/K)){t <- j}
    }
    x <- c(x, t)
  }
  x
}

simunif2 <- function(n, K){
  x <- vector()
  for(i in 1:n){
    u = runif(1, 0, 1)
    x <- c(x, 1 + floor(K*u))
  }
  x
}

set.seed(123)
simunif(10,9)

set.seed(123)
simunif2(10,9)

set.seed(123)
extraDistr::rdunif(10, 1, 9)

simber <- function(n, p){
  x <- vector()
  for(i in 1:n){
    u = runif(1, 0, 1)
    ifelse(u < (1-p), x <- c(x, 0), x <- c(x, 1))
  }
  x
}

set.seed(123)
simber(20, 0.3)

Rlab::rbern(20, 0.3)


simbin <- function(N, n, p){
  x <- vector()
  for(i in 1:N){
    y <- vector()
    for (k in 1:n) {
      u <- runif(1, 0, 1)
      ifelse(u < (1-p), y <- c(y, 0), y <- c(y, 1))
    }
    x <- c(x, sum(y))
  }
  x
}

set.seed(123)
simbin(20, 10, 0.3)


set.seed(123)
rbinom(20, 10, 0.3)


# MAR -  Slide A5 ----
## Contínuo ----
simbeta <- function(n){
  x <- vector()
  for(i in 1:n){
    u = 1
    alpha = 0
    while (u > alpha) {
      x_c <- runif(1, 0, 1)
      alpha <- (27/4)*(x_c^2)*(1-x_c)
      u <- runif(1, 0, 1)
      
    }
    x <- c(x, x_c)
  }
  x
}

set.seed(12345)

hist(
  simbeta(10000),
  freq = F,
  main = "Beta(3, 2)", # Título do gráfico
  ylim = c(0, 2), # Range do eixo y.
  xlab = "x", # Título do eixo x.
  col = "grey", # Cor do gráfico
  cex.main = 1.5, # Tamanho da fonte do títulos
  cex.lab = 1.15 # Tamanho da fonte dos títulos dos eixos
)

# Adiciona a curva da f.d.p. no topo do hist.
graphics::curve(dbeta(x, 3, 2), add = T, lwd = 3, lty = 1)

# Cria uma caixa ao redor do gráfico.
graphics::box(lwd = 2)

## Discreto ----

# função uniforme discreta

# p.m.f
set.seed(12345)

simunif(1, 10)

simsomepmf <- function(n){
  x <- vector()
  pmf <- c(0.11, 0.12, 0.09, 0.08, 0.12,
           0.1, 0.09, 0.09, 0.1, 0.1)
  for(i in 1:n){
    u = 1
    alpha = 0
    while (u > alpha) {
      x_c <- simunif(1, 10)
      alpha <- (10/12)*pmf[x_c]
      u <- runif(1, 0, 1)
    }
    x <- c(x, x_c)
  }
  x
}

arm::discrete.histogram(
  simsomepmf(10000),
  ylim = c(0, 0.15), # Range do eixo y.
  cex.main = 1.5, # Tamanho da fonte do títulos
  cex.axix = 1.15, #
  main = "X", # Título do gráfico
  xlim = c(0, 11)
)

# Adiciona a curva da f.m.p. no topo do hist.
pmf <- c(0.11, 0.12, 0.09, 0.08, 0.12, 0.1, 0.09, 0.09, 0.1, 0.1)
graphics::lines(1:10, pmf, lwd = 1, lty = 1) # lty = line type
graphics::points(1:10, pmf, lwd = 0.1, pch = 19)

# Cria uma caixa ao redor do gráfico.
graphics::box(lwd = 2) # lwd = line width

# LAB 0 - 20/10/2022 ----
# Metodos de Reamostragem (Bootstrap e Jacknnife)
## Amostra Original ----
set.seed(123)
n <- 100
beta0 <- 1.5
beta1 <- -0.3
x <- runif(n)
sigma2 <- 1
y <- rnorm(n, mean = beta0 + beta1*x, sd = sigma2)
dados <- data.frame(y, x)
head(dados)
mod <- lm(y~x, data = dados)
betas <- coefficients(mod)

## Bootstrap ----
B <- 10E3
mBetas <- matrix(NA, ncol = 2, nrow = B)
for (b in 1:B) {
  ind <- sample(1:n, n, replace = T)
  dados.boot <- dados[ind,]
  esti_b <- lm(y~x, data = dados.boot) # Estimador
  mBetas[b,] <- coefficients(esti)
}
(mean.betas <- apply(mBetas, 2, mean))

(ep.boot <- sqrt(apply(mBetas, 2, var)*(B/(B-1))))

## Jacknnife ----
mBetas2 <- matrix(NA, ncol = 2, nrow = n)
for(i in 1:n){
  dados.jack <- dados[-i,]
  esti_j <- lm(y~x, data = dados.jack)
  mBetas2[i,] <- coefficients(esti)
}

(mean.betas2 <- apply(mBetas2, 2, mean))

resultados <- rbind(c(beta0, beta1), betas, mean.betas, mean.betas2, ep.boot)
row.names(resultados) <- c("Real values", "Linear Model", "Bootstrap", 
                           "Jacknnife", "Ep.Boot")
round(resultados, digits = 4)

# Erro padrão
summary(mod)
summary(esti_b)

broom::tidy(mod)
broom::tidy(esti_b)
broom::tidy(esti_j)


# LAB 0 - 03/11/2022 ----

set.seed(20)

y <- rpois(20, lambda = 10) # Amostra de tamanho 20 e lambda 10.
y

## Função escore para Poisson. ----
am <- list(n = length(y), soma = sum(y))

Upois <- function(lambda, amostra){
  return(with(amostra, n - soma/lambda))
}

uniroot(Upois, interval = range(y), amostra = am)$root

mean(y)

# Objetivo: Encontrar um processo iterativo de Newton/Rapson para a distribuição de poison.

## Hessiana ----

HPois <- function(lambda, amostra){
  return(amostra$soma/lambda^2)
}

## Método de Newton-Raphson ----
maxit <- 100 # Máximo de iterações.
lambdaNR <- 5
iter <- 0
d <- 1
while(d > 1e-12 & iter <= maxit){
  lambdaNR.new <- lambdaNR -
    Upois(lambdaNR, am)/HPois(lambdaNR, am)
  d <- abs(lambdaNR - lambdaNR.new)
  lambdaNR <- lambdaNR.new
  iter <- iter + 1
}

c(lambdaNR, iter)

## Definindo a função de verossimilhança ----
veroPois <- function(par, amostra, tipo = "logL", maxlogL){
tipo = match.arg(tipo, choices = c("L", "LR", "logL", "dev"))
  ll <- with(amostra, -n*par + soma * log(par))
  return(switch(tipo, "L" = exp(ll),
                "LR" = exp(ll - maxlogL),
                "logL" = ll,
                "dev" = 2 * maxlogL - ll))
}

optimize(veroPois, interval = range(y),
         maximum = T, amostra = am)

veroPois <- function(par, dados, tipo, maxlogL){
  tipo = match.arg(tipo, choices = c("L", "LR", "logL", "dev"))
  ll <- sapply(par,
               function(p){sum(dpois(dados,
                                     lambda = p, log = T))})
  return(switch(tipo, "L" = exp(ll),
                "LR" = exp(ll - maxlogL),
                "logL" = ll,
                "dev" = 2 * (maxlogL - ll)))
}

mll <- sum(dpois(y, lambda = mean(y), log = T))

par(mfrow = c(1, 4)) # Possibilita incorporar 4 gráficos em 1 linha.
curve(veroPois(x, dados = y, tipo = "L", maxlogL = mll), 8, 11,
      ylab = expression(L(lambda)), xlab = expression(lambda))

curve(veroPois(x, dados = y, tipo = "LR", maxlogL = mll), 8, 11,
      ylab = expression(L(lambda)), xlab = expression(lambda))

curve(veroPois(x, dados = y, tipo = "logL", maxlogL = mll), 8, 11,
      ylab = expression(L(lambda)), xlab = expression(lambda))

curve(veroPois(x, dados = y, tipo = "dev", maxlogL = mll), 8, 11,
      ylab = expression(L(lambda)), xlab = expression(lambda))

# Px Aula ----






# FIM ----




