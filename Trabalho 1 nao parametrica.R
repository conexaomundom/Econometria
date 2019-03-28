

install.packages("MVA")
# install.packages("MVN")
install.packages("moments")
install.packages("robustbase")
install.packages("mvoutlier")
install.packages("BSDA")

library(BSDA)
library(moments)
library(robustbase)
library(mvoutlier)

####### Banco de dados Casas
banco
attach(banco)
names(banco)


########################################################################
# A distribuição do preço de venda é a mesma do preço anunciado.
# essas duas veriaveis estão em milhares de dolares
Preco_de_Venda
hist(Preco_de_Venda)
summary(Preco_de_Venda)
# A media nao esta muito longe da mediana
var(Preco_de_Venda)
# variancia alta  5296.443 ne

library(nortest)
shapiro.test(Preco_de_Venda)
lillie.test(Preco_de_Venda)

library(MVA)

qqnorm(Preco_de_Venda); qqline(Preco_de_Venda, col = 2)


# Preco anunciado 
Preco_Anunciado
hist(Preco_Anunciado)
summary(Preco_Anunciado)
# A media nao esta muito longe da mediana
var(Preco_Anunciado)
# variancia alta  6301.617 ne
length(Preco_Anunciado)

detach(banco)
library(psych)
pairs(banco[, 1:2])
pairs.panels(banco[ , 1:2])
# Para testar se Preco_de_Venda e Preco_Anunciado têm a mesma distribuição faz-se o 
# teste de Smirnov para duas amostras, o teste é feito para captar diferenças 
# nos parâmetros de locacao e no formato da distribuição. Como o tamanho da amostra
# é 50 nem é grande nem pequeno, então será feito dois testes esse de Smirnov
# e o teste Qui-quadrado que também é usado para avaliar se as amostras foram 
# sao de populações coma mesma distribuição. ###
## analisar a questão das frequencias esperadas para o uso do teste qui-quadrado.

chisq.test(Preco_de_Venda, Preco_Anunciado)
# no teste qui-quadrado obtivemos o p-valor de p-value = 0.2401, não rejeitando a 
# hipotese nula, ou seja, nao rejeitando a hipotese que as populações têm a mesma 
# distribuição
ks.test(Preco_de_Venda, Preco_Anunciado)
# o p-valor desse teste foi 0.7112, ao nivel de significancia de 5% nao rejeitamos a 
#hipotese nula, ou seja, nao rejeitamos a hipotese que os dados seguem a mesma 
# distribuição.

library(nortest)
shapiro.test(Preco_Anunciado)
lillie.test(Preco_Anunciado)


# nenhum dos dois bancos seguem normalidade 

# O objetivo deste teste ´e comprovar se duas amostras foram extra´ıdas
# de uma mesma popula¸c˜ao (ou de popula¸c˜oes com a mesma distribui¸c˜ao). 
# Trata-se da vers˜ao para duas amostras do teste de Kolmogorov para bondade do ajuste.


####################################################################
# O que você pode dizer acerca da distribuição dos dados? E o que isso implica?
detach(banco)
library(psych)
pairs(banco[, 1:4])
pairs.panels(banco[ , 1:4])
pairs.panels(banco[ , 5:9])

####

attach(banco)
names(banco)

# "Preco_de_Venda"  "Preco_Anunciado" 
chisq.test(Preco_de_Venda, Preco_Anunciado)
ks.test(Preco_de_Venda, Preco_Anunciado)

# "area_util", "Coodos"
chisq.test(area_util,Coodos)
ks.test(area_util, Coodos)

# "Quartos"         "Banheiros"
chisq.test(Quartos, Banheiros)
ks.test(Quartos, Banheiros)

# Idade"           "Terreno
chisq.test(Idade, Terreno)
ks.test(Idade, Terreno)

#### Testes muito inuteis nessa situação


###
medias <- apply(banco, 2, "mean")
medias
banco1 <- as.matrix(banco)
modelo_anova <-  anova(banco1)

normalidade_shapiro <- apply(banco, 2, shapiro.test)
library(nortest)
normalidade_lillie <- apply(banco, 2, lillie.test)
# os grficos vao ser aqueles mesmo


# Existe diferenca entre o preco de venda e o anunciado, segundo o parâmetro de locacaoo?
# O parametro de locação a ser usado é a mediana, elas sao diferentes ou iguais


# H0: NÃO existe diferenca entre o preco de venda e o anunciado, segundo o parâmetro de locacao
# H1: Existe diferenca entre o preco de venda e o anunciado, segundo o parâmetro de locacao

SIGN.test(Preco_Anunciado,Preco_de_Venda,alternative = "two.sided")
wilcox.test(Preco_Anunciado,Preco_de_Venda, paired = TRUE)

# Rejeita H0, ou seja, rejeita-se a hipótese que que não há diferença entre o preçode venda e o preço
# anunciado, ou seja podemos afirmar que há diferença entre eles. para ambos os testes com p-valores
# 3.331e-15 e 1.082e-09 para os teste de do sinal e o teste de Wilcoxon respectivamente.

####################################################################################
#Existe associaçaoo entre o número de quartos e banheiros?

# teste de indenpendencia ou homogeneidade

# H0: Nao Existe associaçaoo entre o número de quartos e banheiros
# H1: Existe associaçaoo entre o número de quartos e banheiros

ma <- matrix(c(Quartos,Banheiros), 50, 2, byrow = TRUE)
chisq.test(ma)
# Ao realizar o teste qui- quadrado de independencia, associação, o p-valor resultou em ao muuuuuuito proximo de
# 1 ou seja, rejeitamos a hipotese nula com louvor sinalizando que rejeitamos a hipotese que não há associação
# entre o numero de quartos e o numero de banheiros afirmando que há associação sim entre essas duas variáveis.


