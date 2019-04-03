# 
#
install.packages("car")
# # banco é a tabela da 12.7 do livro Gujarati e Porter.
banco 
library(xtable)
head_tabela <- xtable(head(banco))

# a) rodando modelo log de todas as variáveis

attach(banco)

modelo_log <- lm(log(C) ~ log(I) + log(L) + log(PNB) + log(A))
xtable(summary(modelo_log))

# Fazendo a seleção de variáveis que a variável com maior p-valor no
# teste t foi log(I) com p-valor de .649559  demonstrando não ser 
# significativa para o modelo, ao nível de significancia de 5\%.

modelo_log1 <- lm(log(C) ~ log(L) + log(PNB) + log(A))
xtable(summary(modelo_log1))

#  Ainda realizando a seleção de variáveis que a variável com maior p-valor no
# teste t foi log(A) com p-valor de 0.116398  demonstrando não ser 
# significativa para o modelo, ao nível de significancia de 5\%.

modelo_log2 <- lm(log(C) ~ log(L) + log(PNB))
xtable(summary(modelo_log2))

# Sendo esse o modelo final sendo significante para o modelo o intercepto,
# log(L) () e log(PNB) (), Com um R2 ajutado de 0.9241 tendo um poder explicativo bom,
# 92.41\% de log(C) ()  é explicado por log de e log de....



#########################################################################################

# b) Obtenha os resíduos e os resíduos padronizados da regressão e faça um 
# gráfico. O que poderíamos dizer sobre a presença de autocorrelação nesses resíduos?

residuo <- residuals(modelo_log2)
fit <- fitted.values(modelo_log2)
ard <- ls.diag(modelo_log2)

residuo_padronizado <- ard$std.res

acf(residuo_padronizado)

# O segundo lag deu alto, estando até fora do intervalo de confiança
# feito para a autocorrelação, chegando proximo de 0.5, já os demais 
# não houveram problemas.


# da autocorrelação presente nos dados.
#Autocorrelação
library(car)
durbinWatsonTest(modelo_log2,max.lag=1)

#> durbinWatsonTest(modelo_log2,max.lag=1)
#lag Autocorrelation D-W Statistic p-value
#1        0.513879     0.9462146   0.002
#Alternative hypothesis: rho != 0

# Ao realizar o teste de Durbin Watson obtemos um p-valor de 0.02
# utilizando o nível de significância de 5% rejeitamos a hipótese nula 
# rejeitamos a hipótese que não há correlação entre as variáveis, isto é
# a correlação está presente nos dados de acordo com o teste Durbin Watson.

# Faça o teste das carreiras e verifique se sua resposta
# difere daquela dada em c.

# Realizando o teste não parametrico

res <- ifelse(residuo_padronizado < 0, res <- 0, res <- 1)
# Numero de sinais positivos
N1 <- sum(res)
# Numero de sinais negativos
N2 <- length(res) - N1

# > res
# [1] 0 0 0 0 1 1 1 0 1 1 1 1 0 0 0 0 0 0 0 1 1 1 0 1 1 0 0 0 1 1
# Observando res tem 10 carreiras
N <- length(res)

# Como sob H0 de que os resultados sucessivos são independentes e supondo
# que N1 > 10 e N2> 10, que no nosso caso N1 = 24 e N2 = 16, o número de
# carreiras apresentadas é assintoticamente normalmente distribuiída
# cuja média e variância são dada por: 


# Calculando a esperança de 
media <- ((2 * N1 * N2)/N ) + 1

# Calculando a variancia
variancia <- ((2 * N1 * N2) * ((2 * N1 * N2) - N))/((N^2) * (N - 1))
