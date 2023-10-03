
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Lista 1 - MATD46_2023.2
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if (!require(pacman)) install.packages("pacman")
pacman::p_load(readxl,  janitor, tidyverse)

# Questão 1 ----
## Item d. ----
prod=c(4.5, 5.9, 4.2, 5.2, 8.1, 9.7, 10.7, 11.9, 12, 12.3)
pot=c(0.9, 2.5, 1.3, 1.8, 3.1, 4.6, 6.1, 6, 5.9, 6.1)
area=c(7.1, 10.4, 7.2, 8.2, 8.5, 11.9, 12.1, 12.5, 12, 11.3)

dados=base::cbind(prod, pot, area)|>base::as.data.frame()

dados|>
  ggplot2::ggplot(aes(x = pot, y = prod))+
  ggplot2::geom_point()+
  ggplot2::labs(
    x = "Potência", y = "Produção"
  )+
  theme_bw()

dados|>
  ggplot2::ggplot(aes(x = area, y = prod))+
  ggplot2::geom_point()+
  ggplot2::labs(
    x = "Área", y = "Produção"
  )+
  theme_bw()

## Item f. ----
mFit = stats::lm(formula = prod~., data = dados)

anova(mFit)

broom::tidy(mFit)

# Questão 2 ----

pacman::p_load(MASS, kableExtra, xtable)

utils::data(UScrime)
utils::help(UScrime)

dplyr::glimpse(UScrime)

## Itens a e b ----

anyNA(UScrime)

UScrime|>
  dplyr::select(Po1, Po2)|>
  mean()

summarytools::st_options(lang = "pt")

UScrime|>
  dplyr::select(Po1, Po2)|>
  summarytools::descr(
    stats = c("min", "q1", "med", "mean","q3", "max",  "sd"),
    # round.digits = 3,
    justify = "c",
    style =  "simple", #"rmarkdown", #"grid", #"jira", #"simple",
    headings = F,
    # split.tables = 1, 
    rescale.weights = T,
    transpose = T
  )|> xtable::xtable()


## Item c ----
UScrime|>
  dplyr::select(U1)|>
  summarytools::descr(
    stats = c("min", "max"),
    justify = "c",
    style =  "simple",
    headings = F,
    rescale.weights = T,
    transpose = T
  )|> xtable::xtable()

# Questão 3 ----

funVec <- function(x){
  if(is.vector(x) == F) print("Não é vetor")
    
  media <- mean(x)
  variancia <- var(x)
  coef <- (sd(x)/media)*100
  
  m1 <- cbind(media, variancia, coef)
  
  return(round(m1,3))
}

data(mtcars)

funVec(mtcars$wt)

# Teste de verificação da função _______________________________
mtcars|>
  dplyr::select(wt)|>
  summarytools::descr(
    stats = c("mean", "sd", "cv"),
    justify = "c",
    style =  "simple",
    headings = F,
    rescale.weights = T,
    transpose = T
  )
# __________________________________________________________________

# Questão 4 ----
## Fç split() ----

# https://acervolima.com/divida-os-dados-em-grupos-na-programacao-r-funcao-split/

help(split)
# Divide um conjunto de dados com base em algum critério.
# Exemplo:
split(x=mtcars, f=mtcars$gear)

# Usando Tidyverse
mtcars|>
  dplyr::group_by(gear)

## Fç aggregate() ----

# https://acervolima.com/calcule-estatisticas-de-resumo-de-subconjuntos-na-programacao-r-funcao-aggregate/

help(aggregate)

# Divide um conjunto de dados com base em algum critério e aplica alguma função estatística nele.

# Exemplo:

stats::aggregate(x=mtcars, by=list(mtcars$gear), FUN=mean)

# Usando Tidyverse
mtcars|>
  dplyr::group_by(gear)|>
  dplyr::summarise(
    mpg = mean(mpg),
    cyl = mean(cyl),
    disp = mean(disp),
    qtd = n()
  )


# Questão 5 ----
pacman::p_load(microbenchmark)

microbenchmark::microbenchmark()


# Questão 6 ----
x <- runif(3000, min = 0, max = 1)
y <- runif(3000, min = 0, max = 1)

z=x+y

hist(z)
hist(x)
hist(y)

# Questão 7 ----



























