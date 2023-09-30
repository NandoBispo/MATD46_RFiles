# Lista 1 - MATD46_2023.2


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












