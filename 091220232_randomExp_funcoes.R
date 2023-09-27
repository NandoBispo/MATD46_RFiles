# Gerador de números aleatórios da dist. exponencial

random.Exp <- function(n, lambda){
  u = runif(n,0,1)
  x = -(1/lambda)*log(u)
  base::return(x)
}

# Função de densidade de uma dist. exp.
dens.Exp <- function(x, lambda){
  f=lambda*exp(-lambda*x)
  base::return(f)
}


# Gerador de números aleatórios para uma dist. Exp-Exp

random.EE <- function(n, theta){
  lambda=theta[1]
  alpha=theta[2]
  u=runif(n,0,1)
  aux=1-u^(1/alpha)
  x=(-1/lambda)*log(aux)
  return(x)
}




d.EE <- function(x, theta){
  lambda=theta[1]
  alpha=theta[2]
  aux=exp(-lambda*x)
  f=alpha*lambda*aux*(1-aux)^(alpha-1)
  return(f)
}

