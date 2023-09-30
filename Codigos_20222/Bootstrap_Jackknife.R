set.seed(123)
n <- 100
beta0 <- 1.5
beta1 <- -0.3
x <- runif(n)
sigma2 <- 1
y <- rnorm(n, mean=beta0+beta1*x,sd=sigma2)
dados <- data.frame(y,x)
head(dados)
mod <- lm(y~x,data=dados)
betas <- coefficients(mod)
#---------------------
#bootstrap

B <- 10E3
mBetas <- matrix(NA,ncol=2,nrow=B)
for(b in 1:B){
  ind <- sample(1:n,n,replace=T)
  dados.boot <-dados[ind,]
  esti <- lm(y~x,data=dados.boot)
  mBetas[b,] <- coefficients(esti)
}
mean.betas <- apply(mBetas,2,mean)
mean.betas

ep.boot <- sqrt(apply(mBetas,2,var)*(B/(B-1)))
ep.boot
#------------------------
# Jacknnife

mBetas2 <- matrix(NA,ncol=2,nrow=n)
for(i in 1:n){
  dados.jack <- dados[-i,]
  esti <- lm(y~x,data=dados.jack)
  mBetas2[i,] <- coefficients(esti)
}
mean.betas2 <- apply(mBetas2,2,mean)

resultados <- rbind(c(beta0,beta1),betas, 
                    mean.betas,mean.betas2,ep.boot)
row.names(resultados) <- c("Real values","Linear model",
                           "Bootstrap","Jacknnife","ep.Boot")
round(resultados,digits = 4)

