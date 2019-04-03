# 
#

# # banco é a tabela da 12.7 do livro Gujarati e Porter.
banco 
library(xtable)
head_tabela <- xtable(head(banco))

# a) rodando modelo log de todas as variáveis

attach(banco)

modelo_log <- lm(log(C) ~ log(I) + log(L) + log(PNB) + log(A))
summary(modelo_log)

# Fazendo a seleção de variáveis que a variável com maior p-valor no
# teste t foi log(I) com p-valor de .649559  demonstrando não ser 
# significativa para o modelo, ao nível de significancia de 5\%.

modelo_log1 <- lm(log(C) ~ log(L) + log(PNB) + log(A))
summary(modelo_log1)

#  Ainda realizando a seleção de variáveis que a variável com maior p-valor no
# teste t foi log(A) com p-valor de 0.116398  demonstrando não ser 
# significativa para o modelo, ao nível de significancia de 5\%.

modelo_log2 <- lm(log(C) ~ log(L) + log(PNB))
summary(modelo_log2)

# Sendo esse o modelo final sendo significante para o modelo o intercepto,
# log(L) () e log(PNB) (), Com um R2 ajutado de 0.9241 tendo um poder explicativo bom,
# 92.41\% de log(C) ()  é explicado por log de e log de....

# ?? Faço uma analise de RESIDUOS?? CREIO QUE SIM. 
