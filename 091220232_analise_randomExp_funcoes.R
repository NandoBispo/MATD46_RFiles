base::source("091220232_randomExp_funcoes.R")

base:set.seed(123)

amostra=random.Exp(n=1000,lambda=0.1)

f=dens.Exp(amostra, lambda=0.1)

graphics::hist(amostra, freq = F)
graphics::points(amostra, f)
# graphics::points(amostra, f, type = "l")

# lines(exp(0.1))

amostra.EE=random.EE(n=100, theta = c(0.1,10))
graphics::hist(amostra.EE, freq = F)


f.EE = d.EE(amostra.EE,theta = c(0.1,10))
points(amostra.EE,f.EE)














