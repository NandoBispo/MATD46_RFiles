#______________________________________
# MATD46 - Estatística Computacional A
# Prof. Jalmar Carrasco
#______________________________________

# 1. Pct com GNA ----
# Números aleatórios reais
# O pacote random do R faz uma conexão com o site 
# https://www.random.org/ para trazer números aleatórios "reais".
# install.packages("random")
library(random)
randomNumbers(n = 10, min = 1, max = 10, col = 10, base = 10) # com duplicação
randomSequence(min = 1, max = 10, col = 10) # sem duplicação
randomStrings(n = 4, len = 5, digits = TRUE, upperalpha = TRUE,
              loweralpha = TRUE, unique = TRUE)
# Na prática, para aplicações em estatística, os números aleatório reais 
# não tem muita utilidade, já que suas propriedades são desconhecidas.

# 2. GNA Uniforme ----
# O método gerador padrão no R é o Mersenne-Twister
# gera uma sequ?ncia de n?meros uniformes baseada nesse algoritmo.
runif(10)

# Através da função set.seed(.), podemos especificar um n?mero inteiro 
# como semente.
set.seed(1)
runif(10)
# Sempre que essa semente for usada, a mesma sequ?ncia ser? obtida.
set.seed(1)
runif(10)

#A função set.seed() também serve para especificar outro método gerador. 
# Por exemplo

set.seed(1, kind = "Knuth-TAOCP-2002")
runif(10)

# É diferente do padrão

set.seed(1, kind = "Mersenne-Twister")
runif(10)

# Atenção: Alterar o método através da função set.seed() fará com que o último 
# método seja utilizado durante toda a sessão.
# Para mudar o método gerador permanentemente em uma sessão, use a função 
# RNGkind() (mais seguro, veja os detalhes)
# É aconselhável manter o padrão (Mersenne-Twister) por ser o que possui as 
# melhores propriedades.

# 3. Função ----

random.J <- function(n,u0){
  sample <- numeric()
  i <- 1
  while(i<(n+1)){
    sample[i] <- 0.5+0.5*sin(2*pi*u0)
    u0 <- sample[i]
    i <- i+1
  }
  return(sample)
}

r <- random.J(n=1000,u0=0.3)
plot(r)
hist(r)
acf(r)

#4. Testes ----

x <- runif(1000)

## Testes visuais ----

plot(x)
hist(x)
acf(x)

## Testes de Aderência ----
### Teste de qui-quadrado ----

# Compara frequência observada por classes, com o que seria esperado
# Testa a hipótese nula de que as frequências observadas e esperadas são iguais.

# Divide os dados em 10 classes de igual tamanho
xc <- cut(x, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
# Com 1000 dados, deveriam haver 100 observa??es em cada classe.
# Estas s?o as frequ?ncias observadas
table(xc)

# A fun??o chisq.test() faz o teste usando esta tabela
chisq.test(x = table(xc))

# Não rejeita a hipótese nula de que as frequências observadas e esperadas 
# são iguais.

### Teste de Kolmogorov-Smirnov ----

  # Teste "de aderência" não paramétrico
  # Compara a distribuição acumulada empírica dos dados com a acumulada de 
  # alguma distribuição de referência.
  # Calcula a maior dist?ncia entre estas duas distribuições
  # Testa a hipótese nula de que a acumulada da distribuição empírica 
  # (dos dados) "adere" (é igual) à distribuição teórica.

ks.test(x, "punif")

# Não rejeita a hipótese nula de que a distribui??o acumulada empírica 
# segue uma uniforme
# Visualiza??o da distribui??o acumulada emp?rica da sequ?ncia gerada (linha preta) com uma distribui??o acumulada de uma uniforme gerada pela fun??o punif() (linha vermelha - usada como refer?ncia).

plot(ecdf(x))
plot(ecdf(punif(seq(0, 1, length.out = 1000))),
     add = TRUE, col = 2)
