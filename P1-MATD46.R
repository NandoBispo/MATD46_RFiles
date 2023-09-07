# *******************************
# P1 - MATD46
# Fernando Bispo
# *******************************

# Questão 1 ----

## 1a ----
simfunc <- function(n){
  x <- vector()
  for(i in 1:n){
    u = 1
    alpha = 0
    while (u > alpha) {
      x_c <- runif(1, 0, 1)
      alpha <- (64/135)*20*x_c*((1-x_c)^3)
      u <- runif(1, 0, 1)
    }
    x <- c(x, x_c)
  }
  return(x)
}

## 1b ----
{set.seed(2022)

hist(
  simfunc(1000),
  freq = F,
  main = "f.d.p.",
  ylim = c(0, 2.5),
  xlab = "x",
  col = "lightblue",
  cex.main = 1.5,
  cex.lab = 1.15
)

graphics::curve(20*x*(1-x)^3, add = T, lwd = 3, lty = 1)
graphics::box(lwd = 2)} # Histograma

## 1c ----



# Questão 2 ----
## 2a ----

{set.seed(2022)

m <- 1000
g <- function(x){exp(-x^2)}
x <-  rexp(m, 3)
I_hat <- mean(g(x))
V_I_hat <- var(g(x))/m
print(glue::glue("E[g(x)] = {round(I_hat, 4)}, Var[g(x)] = {format(round(V_I_hat, 7), scientific = F)}"))
}

# format(num, scientific = F)

## 2b ----

integrate(g, lower = 0, upper = Inf)


identical(I_hat, integrate(g, lower = 0, upper = Inf))








