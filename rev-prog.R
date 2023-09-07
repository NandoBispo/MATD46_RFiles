#--------------------------------------
# MATD46 - Estatística computacional A
# Prof. Jalmar Carrasco
#--------------------------------------

# Problema 1

x <- 7
if(x>3){
  print("3")
  if(x<5){
    print("5")
    if(x==7){
      print("7")
    }
  }
}

# Problema 2

i <- 3
while(i>=0){
  print(i)
  i <- i-1
}

# Problema 3

i <- 5
while(TRUE){
  print(i)
  i <- i-1
  if(i<=2){
    break
  }
}

# Problema 4

j <- 0
for(i in 1:10){
  if(j %% 3 == 0){
    j <- 1
  }
  j <- i
}

j

# Funções

baskara <- function(a,b,c){
  stopifnot(a != 0)
  delta <- b^2-4*a*c
  if(delta >=0){
    den <- 2*a
    sqrt.delta <- sqrt(delta)
    x1 <- (-b-sqrt.delta)/den
    x2 <- (-b+sqrt.delta)/den
    return(c(x1,x2))
  }else{
    return(NULL)
  }
  
}

baskara(-3,2,1)
baskara(0,2,1)
