#______________________________________
# MATD46 - Estatística Computacional A
# Prof. Jalmar Carrasco
#______________________________________

# Distribuição Uniforme ----

x <- seq(0, 1, 0.01)
fx <- dunif(x)
par(mfrow = c(1, 2))
plot(x, fx, type = "l")
plot(ecdf(x))
par(mfrow = c(1, 1))

## Variáveis Contínuas ----
# f(x) = 3x², 0 < x < 1

# Gera 1000 valores da uniforme
n <- 1000
u <- runif(n)
# Calcula a inversa
x <- u^(1/3)
# Histograma dos valores gerados
hist(x, prob = TRUE)
# Modelo teórico
curve(3 * x^2, from = 0, to = 1, add = TRUE, col = 2)
## Acumulada empírica
plot(ecdf(x))
# Acumulada teC3rica
curve(x^3, from = 0, to = 1, add = TRUE, col = 2)
legend("left", legend = c("Empírica", "Teórica"),
       lty = 1, col = 1:2, bty = "n")

### Exponencial ----

randexp <- function(n, lambda) {
  stopifnot(lambda > 0)
  u <- runif(n)
  x <- -log(1 - u)/lambda
  return(x)
}

x <- randexp(n = 1000, lambda = 0.5)
hist(x, freq = FALSE)
curve(dexp(x, rate = 0.5), add = TRUE, col = 2, from = 0)

plot(ecdf(x))

Fx <- function(x, lambda) 1 - exp(-lambda * x)
curve(Fx(x, lambda = 0.5), add = TRUE, col = 2, from = 0)
plot(ecdf(rexp(x, rate = 0.5)), add = TRUE, col = 3)
legend("right", legend = c("Empírica", "Teórica", "rexp"),
       lty = 1, col = 1:3, bty = "n")

### Uniforme Contínua ----

randunif <- function(n, a, b) {
  u <- runif(n)
  x <- a + (b - 1) * u
  return(x)
}

x <- randunif(n = 1000, a = 1, b = 10)
hist(x, freq = FALSE)
curve(dunif(x, 1, 10), add = TRUE, col = 2, from = 1)

plot(ecdf(x))

Fx <- function(x, a, b) (x - a)/(b - a)
curve(Fx(x, a = 1, b = 10), add = TRUE, col = 2,
      from = 1, to = 10)
plot(ecdf(runif(x, 1, 10)), add = TRUE, col = 3)
legend("right", legend = c("Empírica", "Teórica", "runif"),
       lty = 1, col = 1:3, bty = "n")

## Variáveis discretas ----

### Bernoulli ----
p <- 0.4
q <- 1 - p
x <- rbinom(1000, size = 1, prob = p)
plot(ecdf(x), verticals = TRUE, col.vert = "red")

# Uma implementação elegante
n <- 1000
u <- runif(n)
x <- as.integer(u > 0.6)

# Uma forma mais longa (e menos eficiente)
x2 <- integer(n)
p <- 0.4
q <- 1 - p
for(i in 1:length(x2)) {
  if(u[i] < q) {
    x2[i] <- 0L
  } else {
    x2[i] <- 1L
  }
}
identical(x, x2)

# Usando ifelse vetorizado
x3 <- ifelse(u < q, 0L, 1L)
identical(x2, x3)

# Usando seleção condicional
x4 <- integer(n)
x4[u >= q] <- 1L
identical(x3, x4)

rbinom(n, size = 1, prob = p)
sample(c(0, 1), size = n, replace = TRUE,
       prob = c(0.6, 0.4))

# Marginais
addmargins(prop.table(table(x)))

plot(prop.table(table(x)), ylim = c(0, 0.7), type = "h")
lines(x = c(0, 1) + .01, y = c(.6, .4),
      col = "red", type = "h", lwd = 2)
legend("topright", legend = c("Empírica", "Teórica"),
       lty = 1, col = 1:2, bty = "n")

plot(ecdf(x))
curve(pbinom(x, size = 1, p = p), add = TRUE, type = "s", col = 2)
plot(ecdf(rbinom(x, size = 1, prob = p)), add = TRUE, col = 3)
legend("right", legend = c("Empírica", "Teórica", "rbinom"),
       lty = 1, col = 1:3, bty = "n")


### Uniforme ----

x <- sample(1:10, size = 1000, replace = TRUE)
plot(ecdf(x), verticals = TRUE, col.vert = "red")

### Algoritmo sequêncial ----
randd <- function(x, px) {
  u <- runif(1)
  x <- 1
  Fx <- px[x]
  while(u > Fx) {
    x <- x + 1
    Fx <- Fx + px[x]
  }
  return(x)
}

k <- 10
x <- 1:k
px <- rep(1/k, length(x))
randd(x = x, px = px)

#Para gerar mais valores podemos usar a função replicate()

replicate(n = 10, randd(x = x, px = px))

