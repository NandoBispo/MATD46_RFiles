

# Função que calcula o fatorial de um número

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


complexo <- function(a,b,c,d){
  # x <- a + bi
  # y <- c + bi
  
  # return(c(x + y, x - y))
  return(c(a+bi + c+di,a+bi - c+di))
}

complexo(3,5,2,2)

