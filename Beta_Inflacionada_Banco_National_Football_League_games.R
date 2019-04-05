

rm(list=ls())
#REGRESS?O BETA INFLACIONADA
install.packages("gamlss")
install.packages("gamlss.dist")
library(gamlss)
library(gamlss.dist)

# Usando o banco National_Football_League_games

# Porem como tem variáveis faltando, que foram preenchidas com NA
# vamos fazer imputação de dados utilizando as médias móveis
# sendo preenchidos pela média dos dois 
