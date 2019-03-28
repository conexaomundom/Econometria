
install.packages("MVA")
# install.packages("MVN")
install.packages("moments")
install.packages("robustbase")
install.packages("mvoutlier")
install.packages("BSDA")
install.packages("xtable")
library(BSDA)


library(moments)
library(robustbase)
library(mvoutlier)

####### Banco de dados Casas
# ficheiro = file.choose()
# banco = read.table(file = ficheiro, header=TRUE, sep = ";", dec = ".")
SISTOLICA

attach(SISTOLICA)
names(SISTOLICA)
####################################################################
# O que você pode dizer acerca da distribuição dos dados? E o que isso implica?
detach(SISTOLICA)
library(psych)
pairs(SISTOLICA[, 1:4])
pairs.panels(SISTOLICA[ , 1:4])
pairs.panels(SISTOLICA[ , 5:8])

###
medias_SISTOLICA <- apply(SISTOLICA, 2, "mean")
medias_SISTOLICA

# normalidade_shapiro_ <- apply(SISTOLICA, 2, shapiro.test)
# library(nortest)
# normalidade_lillie_ <- apply(SISTOLICA, 2, lillie.test)
# os grficos vao ser aqueles mesmo

# Nada posso afirmar sobre, até pq nenhum teste pode ser aplicado.

#####################################################################################
## Realizando testes nao parametricos
#####################################################################################
# Existe associaçaõ entre genero e raça?

# H0: Nao xiste associaçaõ entre genero e raça
# H1: Existe associaçaõ entre genero e raça
attach(SISTOLICA)

sexo = ifelse(SISTOLICA$Sexo == "Masculino", 1, 0)
raca = ifelse(SISTOLICA$Raça == "Branca", 1, 0)
matriz <- matrix(c(sexo,raca), 24, 2, byrow = FALSE)
# NA REALIDADDE CREIO EU QUE AQUI SE APLIQUE O TESTE EXATO DE FISHER
table(sexo,raca)
fisher.test(matriz)
# Sendo uma amostra pequena com apenas 12 observações faz-se o teste exato de Fisher 
# baseando-se nas frequências observadas em tabelas de contingência 2X2 para analisar 
# a independência e homogeneidade de duas populações. Sendo variaveis aleatórias 
# independentes dicotomicas com categorias mutualmente excludentes. Ao realizar o teste
# exato de Fisher o ao nível de significancia de 5% não rejeita-se a hipótese nula,
# Não rejeitando a hipótese que não haja associação entre Sexo e Raça, com p-valor de
# aproximadamente 1, ou sjea, existe associação entre genero e raça.

##########################
# Para testar se existe diferença no parÂmetro de locação da pressão sanguínea entre os generos?
# Faremos o teste do sinal e o teste de Wilcoxon(que é um teste mais poderoso por não considerar
# apenas o sinal das diferenças como o Teste do Sinal faz, sendo também considerada a magnitude
# das diferenças, além disso o teste de Wilcoxon atribui maior peso aos maiores D_{i}'so que aos
# menores.)

## H0: NÃO Existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea entre os gˆeneros?
## H1: Existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea entre os gˆeneros
names(SISTOLICA)
SIGN.test(sexo, Pre_Exerc_sem_Estresse,alternative = "two.sided")
wilcox.test(sexo, Pre_Exerc_sem_Estresse, paired = TRUE)

# Em ambos os testes Rejeitamos H0 ao nível de significancia de 5% com p-valores de 1.192e-07 e 
# 1.938e-05 para o teste do sinal e teste de wilcoxon respectivamente. Rejeitando H0, rejeitamos 
# a hipótese que não existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea entre os 
# gˆeneros. Desta forma podendo afirmar que Existe diferença no parÂmetro de loca¸c˜ao da press˜ao 
# sangu´ınea entre os gˆeneros.


# Entre quais tipo de press˜ao sangu´ınea existe diferen¸ca em relaçao ao parˆametro de locaçaõ?
names(SISTOLICA)
# Ao ohar do pesquisador que está realizando os testes não sentido comparar pre com pos e nao
# pre com pre, e pos com pos, inclusive nao cruzando os motivos do estresse até porque a pessoa que vai
# realizar eo exercício já passou pelo estresse e nao duramte os exercícios nao tera 
# uma interpretacao plausível para tal relação.

# pre e pos de exercicios sem estresse
SIGN.test(Pre_Exerc_sem_Estresse,Pos_Exerc_sem_Estresse, alternative = "two.sided")
wilcox.test(Pre_Exerc_sem_Estresse,Pos_Exerc_sem_Estresse, paired = TRUE)

# Em ambos os testes não Rejeitamos H0 com p-valores de 0.6776 e 0.9031 para o teste do sinal e 
# teste de wilcoxon respectivamente.  Não Rejeitando H0,  não rejeitamos a hipótese que não existe diferença 
# no parÂmetro de  presso sangu´ínea pre e pos sem estresse. Desta forma podendo afirmar que 
# não xiste diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea sem sem estresse.


# pre e pos de exercicios com estresse Matematica
SIGN.test(Pre_Exerc_com_Estresse_Matematica,Pos_Exerc_com_Estresse_Matematica, alternative = "two.sided")
wilcox.test(Pre_Exerc_com_Estresse_Matematica,Pos_Exerc_com_Estresse_Matematica, paired = TRUE)
# Ao nível de significancia de 5% em ambos os testes Rejeitamos H0 com p-valores de 0.0002772 e 0.0001815
# para o teste do sinal e teste de wilcoxon respectivamente. Rejeitando H0, rejeitamos a hipótese que não 
# existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea pre e pos de exercicios com estresse 
# Matematica. Desta forma podendo afirmar que Existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea 
# pré e pés de exercicios sem estresse Matematica, ou seja a pressão sanguinea se altera antes e depois do exercicio
# se o praticante passou por um estresse verbal antes de ir praticar os exercicios.

# pre e pos de exercicios com estresse Verbal
SIGN.test(Pre_Exerc_com_Estresse_Verbal,Pos_Exerc_com_Estresse_Verbal, alternative = "two.sided")
wilcox.test(Pre_Exerc_com_Estresse_Verbal,Pos_Exerc_com_Estresse_Verbal, paired = TRUE)
# Ao nível de significancia de 5% em ambos os testes Rejeitamos H0 com p-valores de 1.192e-07 e 1.933e-05
# para o teste do sinal e teste de wilcoxon respectivamente. Rejeitando H0, rejeitamos a hipótese que não 
# existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea pre e pos de exercicios com estresse 
# Verbal. Desta forma podendo afirmar que Existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea 
# pre e pos de exercicios sem estresse Verbal, ou seja a pressão sanguinea se altera antes e depois do exercicio
# se o praticante passou por um estresse verbal antes de ir praticar os exercicios.

  
##### Podemos afirmar que a press˜ao sangu´ınea aumenta ap´os o exerc´ıcio?

# Apos realizar os testes para verificar se ha diferenças vamos calcular se aumenta apos o exercicio
# vamos calcular se pos exercicio com estresse matemarica e por exercicio com estresse verbal é maior que 
# pre exercicios com esses tipor de estresse. Não vamos fazer com o pre e pós exercicio sem estresse
# pois o teste anterior apontou que nao há diferenca nessas variaveis.

# H0: A pressao sanguinea é maior pre exercicio com estresse matematica >=0
# H!: A pressao sanguinea é maior pos exercicio com estresse matematica <0

wilcox.test(Pre_Exerc_com_Estresse_Matematica,Pos_Exerc_com_Estresse_Matematica,
            paired = TRUE, alternative = "less")
# Ao realizar o teste de wilcoxon para saber se apressao sanguinea é maior pos exercicio
# com estresse matematica o p-valor resultou em 0.9999 rejeitando a hipotese nula, ou seja,
# podemos afirmar com base na amostra que a pressão sanguinea aumenta após o exercicio com
# estresse Matematica.

wilcox.test(Pre_Exerc_com_Estresse_Verbal,Pos_Exerc_com_Estresse_Verbal,
            paired = TRUE, alternative = "less")
# Ao realizar o teste de wilcoxon para saber se apressao sanguinea é maior pos exercicio
# com estresse matematica o p-valor resultou em um valor extremamente proximo de 1 rejeitando 
# a hipotese nula, ou seja, podemos afirmar com base na amostra que a pressão sanguinea aumenta 
# após o exercicio com estresse Verbal.

  
# Quais diferentes press˜oes sangu´ıneas podemos dizer que tˆem a mesma distribui¸c˜ao?
# "Pre_Exerc_sem_Estresse"           
# "Pre_Exerc_com_Estresse_Matematica"
# "Pre_Exerc_com_Estresse_Verbal"    
# "Pos_Exerc_sem_Estresse"           
# "Pos_Exerc_com_Estresse_Matematica"
# "Pos_Exerc_com_Estresse_Verbal"   


a <- ks.test(Pre_Exerc_sem_Estresse, Pre_Exerc_com_Estresse_Matematica)$p.value
b <- ks.test(Pre_Exerc_sem_Estresse, Pre_Exerc_com_Estresse_Verbal)$p.value
c <- ks.test(Pre_Exerc_com_Estresse_Matematica, Pre_Exerc_com_Estresse_Verbal)$p.value



d <- ks.test(Pre_Exerc_sem_Estresse, Pos_Exerc_sem_Estresse)$p.value
e <- ks.test(Pre_Exerc_com_Estresse_Matematica, Pos_Exerc_com_Estresse_Matematica)$p.value
f <- ks.test(Pre_Exerc_com_Estresse_Verbal, Pos_Exerc_com_Estresse_Verbal)$p.value



g <- ks.test(Pos_Exerc_sem_Estresse, Pos_Exerc_com_Estresse_Matematica)$p.value
h <- ks.test(Pos_Exerc_sem_Estresse, Pos_Exerc_com_Estresse_Verbal)$p.value
i <- ks.test(Pos_Exerc_com_Estresse_Matematica, Pos_Exerc_com_Estresse_Verbal)$p.value

library(xtable)
tabela <- rbind(a,b,c,d,e,f,g,h,i)
tabs1 <- data.frame(tabela, row.names = c("Pre_Exerc_sem_Estresse, Pre_Exerc_com_Estresse_Matematica","Pre_Exerc_sem_Estresse, Pre_Exerc_com_Estresse_Verbal",
                                                     "Pre_Exerc_com_Estresse_Matematica, Pre_Exerc_com_Estresse_Verbal","Pre_Exerc_sem_Estresse, Pos_Exerc_sem_Estresse",
                                                     "Pre_Exerc_com_Estresse_Matematica, Pos_Exerc_com_Estresse_Matematica","Pre_Exerc_com_Estresse_Verbal, Pos_Exerc_com_Estresse_Verbal",
                                                     "Pos_Exerc_sem_Estresse, Pos_Exerc_com_Estresse_Matematica", "Pos_Exerc_sem_Estresse, Pos_Exerc_com_Estresse_Verbal", 
                                                     "Pos_Exerc_com_Estresse_Matematica, Pos_Exerc_com_Estresse_Verbal"))
names(tabs1) <- c("Teste de Smirnov")
library(xtable)

xtable(tabs1)


### Nem todas as variáveis têm a mesma distribuição.

# Pre_Exerc_sem_Estresse, Pos_Exerc_sem_Estresse 0.4413066
# Pre_Exerc_com_Estresse_Matematica, Pos_Exerc_com_Estresse_Matematica 0.2590564
# Pre_Exerc_com_Estresse_Verbal, Pos_Exerc_com_Estresse_Verbal 0.03100759

"Pre_Exerc_sem_Estresse"
# "Pre_Exerc_sem_Estresse"
# "Pre_Exerc_com_Estresse_Matematica"
# "Pre_Exerc_com_Estresse_Verbal"    
# "Pos_Exerc_sem_Estresse"           
# "Pos_Exerc_com_Estresse_Matematica"
# "Pos_Exerc_com_Estresse_Verbal" 

library(nortest)
lillie_q1 <- apply(SISTOLICA[, -c(1:2)],2,lillie.test)
shapiro_q1 <- apply(SISTOLICA[, -c(1:2)],2,shapiro.test)
tab_lilli <- c(lillie_q1$Pre_Exerc_sem_Estresse$p.value, lillie_q1$Pre_Exerc_com_Estresse_Matematica$p.value,
               lillie_q1$Pre_Exerc_com_Estresse_Verbal$p.value, lillie_q1$Pos_Exerc_sem_Estresse$p.value,
               lillie_q1$Pos_Exerc_com_Estresse_Matematica$p.value, lillie_q1$Pos_Exerc_com_Estresse_Verbal$p.value)
tab_shap <- c(shapiro_q1$Pre_Exerc_sem_Estresse$p.value, shapiro_q1$Pre_Exerc_com_Estresse_Matematica$p.value,
              shapiro_q1$Pre_Exerc_com_Estresse_Verbal$p.value, shapiro_q1$Pos_Exerc_sem_Estresse$p.value,
              shapiro_q1$Pos_Exerc_com_Estresse_Matematica$p.value, shapiro_q1$Pos_Exerc_com_Estresse_Verbal$p.value)
tab1 <- data.frame(tab_lilli,tab_shap, row.names = c("Pre Exerc sem Estresse","Pre Exerc com Estresse Matematica",
                                                     "Pre Exerc com Estresse Verbal","Pos Exerc sem Estresse",
                                                     "Pos Exerc com Estresse Matematica","Pos Exerc com Estresse Verbal"))
names(tab1) <- c("Teste Lilliefors","Teste Shapiro-Wilk")
library(xtable)
xtable(tab1)
