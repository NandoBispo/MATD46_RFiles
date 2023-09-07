#--------------------------------------
# MATD46 - Estatística computacional A
# Prof. Jalmar Carrasco
#--------------------------------------

x <- c(30,37,36,43,42,43,43,46,41,42)
n <- length(x)

xbar <- mean(x)
xbar

var.mean <- var(x)/n
var.mean

se.mean <- sqrt(var.mean)
se.mean

B <- 1000 
n <- length(x)
M.npb <- numeric(B) 
sd.boot <- numeric(B)
set.seed(123)
for(i in 1:B){
  x.npb <- sample(x,size=n,replace=TRUE)
  M.npb[i] <- mean(x.npb)
  sd.boot[i]<- sd(x.npb)
}

# Computing the percentile bootstrap 95% CI
d <- quantile(M.npb, c(.025,.975))
d

# Computing the pivotal (or basic) 95% CI
deltastar <- M.npb - xbar
d <- quantile(deltastar, c(.025,.975))
ci <- xbar - c(d[2], d[1])
ci

# Computing the studentized bootstrap 95% CI
deltastar <- (M.npb - xbar)/sd.boot
d <- quantile(deltastar, c(.025,.975))
ci <- xbar - c(d[2], d[1])*sd(M.npb)
ci

# The estimated bootstrap bias and standard error, as well as 
# the bootstrap and bias corrected bootstrap estimates of theta 
# are given by
bias <- mean(M.npb)-xbar 
bias
se <- sd(M.npb)
se
M.boot <- mean(M.npb) 
M.boot
bias.corrected.estimate <- xbar-bias 
bias.corrected.estimate

# Teste de hipótese
# H0: mu=41 contra H1:mu!=41

t.obs <- (mean(x)-41)/(sd(x)/sqrt(n))
t.obs 
p.value <- 2*min(1-pt(t.obs,n-1),pt(t.obs,n-1))
p.value 

#non-parametric bootstrap
B <- 1000 
set.seed(123)
mu0 <- 41 
sd0 <- sd(x)
t.obs <- (mean(x)-mu0)/(sd0/sqrt(n))
t.npb <- numeric(B)
z <- x-mean(x)+mu0
for(i in 1:B){
  x.npb <- sample(z,n,replace=T)
  sd.npb <- sd(x.npb)
  t.npb[i] <- (mean(x.npb)-mu0)/(sd.npb/sqrt(n))
}
# decision on H0 based on the p.value
p.value <- 2*min(sum(t.npb>t.obs)/B,sum(t.npb<t.obs)/B)
p.value

# Utilizando a libraria "boot"

library(boot)
set.seed(666)

fun <- function(dados,i){return(mean(dados[i]))}
result <- boot(data=x,statistic=fun,R=1000)
result
plot(result)
IC <- boot.ci(result,conf = 0.95,type = "bca")
IC
