

install.packages("ggplot2")
library(ggplot2)
# banco é a tabela da 10.17 do livro Gujarati e Porter.
attach(banco)

# a) Diagramas de dispersao

graf_disp <- ggplot(banco, aes(x=Paladar, y=Acido_Acetico)) + geom_point(aes(y = Acido_Acetico), col =2) +
  geom_point(aes(y=Acido_Latico, col =3))+geom_point(aes(y=H2_S, col = 4)) + # geom_smooth(method="loess", se = F) +
   labs(subtitle = "Diagrama de dispersão do paladar com relação ao Ácido Acético, ao H2S e ao Ácido Latico.", 
                            y = "Acido_Acetico, H2_S, Acido_Latico", x = "Paladar", caption = "Fonte: Dados retirado do livro Gujarati e Porter")
legend(5.5,26, c('Acido_Acetico", "H2_S","Acido_Latico'), col=c(2,3,4))
plot(graf_disp)

plot(banco[,-1], main = "Diagrama de dispersão de todas as variéveisdo banco: Ácido Acético, H2S e Ácido Latico.")

# b) Regressão bivariada do paladar contra o ácido acético e H2S e interprete os resultados obtidos.

plot(Paladar ~ H2_S, xlab = "Paladar", ylab ="H2S", main = "Diagrama de dispersão entre Paladar e H2S") 
plot(Paladar ~ Acido_Acetico,xlab = "Paladar", ylab ="Ácido Acético", main = "Diagrama de dispersão entre Paladar e Ácido Acético")
plot(H2_S ~ Acido_Acetico, xlab ="H2S", ylab = "Ácido Acético", main = "Diagrama de dispersão entre H2S e Ácido Acético")



# Bem observado o gráfico de dispersão as variáveis Paladar e H2S tem uma relação
# muito proxima a uma relaao linear positiva mesmo só com30 observações já é
# possível notar que quando o paladar aumenta gradamente é possível ver que H2S
# também aumenta, uma regressão linear seria um bom ajuste creio eu neste caso.
# Já observando o gráfico de dispersão do Paladar com Ácido acético não se consegue 
# ver claramente uma relação linear entre as variáveis, a medida que o Paladar aumenta 
# há níveis de Ácido Acético que também aumentam, porém há níveis que continuam baixos
# A interpretação do gráfico de dispersão de H2S e Ácido Acético é muito parecido com o 
# gráfico de dispersão anterior referente às variáveis Paladar com Ácido acético, porém como
# se trata de de duas variáveis explicativas agora esse resultado é interessante para nós
# porque como não tem uma relação linear não tão clara, pode ser um indício que pode não haver
# colinearidade entre as variáveis.
xtable(cor(banco[,c(2,3,4)]))
## Além de todas essas visualizações da relação das variávies ao calcular a matriz de correlação
## a correlação entre a variável dependente Paladar e a variável explicativa H2S é de 0.75575227,
## uma correlação boa o que dá indicios de um possível modelo de regressão linear bom, já a 
## Já a correlação entre 0.2953833, uma correlação baixa, confirmando o que foi visto no gráfico,
## será que essa variável será explicativa para o modelo de regressão linear, bem depende, 
## por isso vamos rodar o modelo e ver se será siginificativa ou não para o modelo. E a correlação
## entre as duas variáveis explicativas H2S e Ácido Acético foi pequena de 0.1843119 e é o que desejamos
## porém a relação delas pode não ser linear.

modelo_linear <- lm(Paladar ~ Acido_Acetico + H2_S)
summary(modelo_linear)
# Como o esperado ao pegar o summary do modelo linear de regressão com duas variáveis explicativas sendo
# elas Acido_Acetico e H2_S para explicar a variável Paladar, a variável Ácido Acético será retirada
# do modelo pois não passou no teste t não rejeitando a hipotese de nao ser significativo para o modelo
# com p-valor de 0.205, enquanto o a outra variável explicativa passou pelo teste t demonstrando ser significativ
# para o modelo, bem após retirar essa variável vamos olhar o resultado do modelo.

modelo_linear1 <- lm(Paladar ~  H2_S)
summary(modelo_linear1)

# Rodando o modelo apenas com uma variável explicativa, sendo ela H2S o intercepto não passa pelo teste t
# com p-valor de 0.112. Vamos rodar novamente o modelo e verficar o resultado.

modelo_linear3 <- lm(Paladar ~ 0 + H2_S)
summary(modelo_linear3)
# Então o modelo final ficou apenas H2S explicando o Paladar, com um poder explicativo de 85.99% da
# variabilidade do Paladar é explicado por H2S, sendo um bom poder explicativo.

# c) ###########################################
# regressão bivariada do paladar contra o ácido lático e H2S e interprete os resultados obtidos. 
plot(Paladar ~ H2_S, xlab = "Paladar", ylab ="H2S", main = "Diagrama de dispersão entre Paladar e H2S") 
plot(Paladar ~ Acido_Latico,xlab = "Paladar", ylab ="Ácido Lático", main = "Diagrama de dispersão entre Paladar e Ácido Lático")
plot(H2_S ~ Acido_Latico, xlab ="H2S", ylab = "Ácido Lático", main = "Diagrama de dispersão entre H2S e Ácido Lático")
xtable(cor(banco[,c(2,4,5)]))

# O gráfico de dispersão entre Paladar e H2S ja foi analisado anteriormente.
# O gráfico de dispersão entre Paladar e Ácido Lático demonstra uma certa relação
# linear sim e com uma correlação de 0.7042362, sendo boa para a realização do
# modelo de regressão linear. Já o gráfico de dispersão entre as duas variáveis
# explicativas pode estar demonstrando certa relação, o que não é muito bom, além de 
# ter uma correlação de 0.6448123 que poe ser indicio de colinearidade.

modelo_linear0.1 <- lm(Paladar ~ H2_S + Acido_Latico)
summary(modelo_linear0.1)
# O modelo de regressão linear final ficou com as duas variáveis explicativas
# mais o intercep, todos demonstrando ser significantes para o modelo porém
# o R2 não foi satisfatorio demonstrando que cerca de 62.59% da variabilidade
# do Paladar é explicado por H2S e Ácido Lático.


# d) #########################################################################
# egressão múltipla do paladar contra o ácido acético, H2S e ácido lático.

modelo_full <- lm(Paladar ~ H2_S + Acido_Latico + Acido_Acetico)
summary(modelo_full)
modelo_full1 <- lm(Paladar ~ H2_S + Acido_Latico)
summary(modelo_full1)
# Realizando o modelo com todas as variáveis a variável Ácido Acetico demonstra
# não ser significativa para o modelo voltando ser o modelo_linear0.1. com o R2 
# não foi satisfatorio demonstrando que cerca de 62.59% da variabilidade do 
# Paladar é explicado por H2S e Ácido Lático.

# e) ########################################################################

# Dados os seus conhecimentos sobre multicolinearidade, como decidiria 
# entre essas regressões?

# Com meus conhecimentos de multicolinaeridade chama minha atenção (Após feita a seleção
# de variáveis) o modelo_linear01, porque as variáveis explicativas tem uma correlação
# considerável de 0.6448123 e no gráfico de dispersão também foi perceptível uma relação
# entre H2S e Ácido Lático e esse modelo teve um poder explicativo de 62.59% não muito
# satisfatório, mas o primeiro modelo rodado modelo_linear3 que consiste em Paladar sendo
# explicado por H2S e sem intercepto que teve a mior correlação entre as variáveis explicativas
# foi H2S e a variável dependente de interesse Paladar de 0.7557523 sendo o modelo que eu escolheria
# por ter o maior poder explicativo de 85.99%.

# f) ########################################################################
# Que conclusões gerais você pode tirar de sua análise?
# À medida que o queijo envelhece, vários processos químicos ocorrem, determinando o sabor
# do produto final. FAZER OUTRAS REGRESSÕES. 