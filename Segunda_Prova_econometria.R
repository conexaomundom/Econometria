######33
#######
#########

install.packages("ggplot2")
library(ggplot2)
banco
attach(banco)

#a) Diagramas de dispersao

graf_disp <- ggplot(banco, aes(x=Paladar, y=Acido_Acetico)) + geom_point(aes(y = Acido_Acetico), col =2) +
  geom_point(aes(y=Acido_Latico, col =3))+geom_point(aes(y=H2_S, col = 4)) + # geom_smooth(method="loess", se = F) +
   labs(subtitle = "Diagrama de dispersão do paladar com relação ao Ácido Acético, ao H2S e ao Ácido Latico.", 
                            y = "Acido_Acetico, H2_S, Acido_Latico", x = "Paladar", caption = "Fonte: Dados retirado do livro Gujarati e Porter")
legend(5.5,26, c('Acido_Acetico", "H2_S","Acido_Latico'), col=c(2,3,4))
plot(graf_disp)

plot(banco[,-1], main = "Diagrama de dispersão de todas as variéveisdo banco: Ácido Acético, H2S e Ácido Latico.")

#b) Regressão bivariada do paladar contra o ácido acético e H2S e interprete os resultados obtidos.

plot(Paladar ~ H2_S, xlab = "Paladar", ylab ="H2S", main = "Diagrama de dispersão entre Paladar e H2S") 
plot(Paladar ~ Acido_Acetico,xlab = "Paladar", ylab ="Ácido Acético", main = "Diagrama de dispersão entre Paladar e H2S")
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
cor(banco[,-1])
## Além de todas essas visualizações da relação das variávies ao calcular a matriz de correlação
## a correlação entre a variável dependente Paladar e a variável explicativa H2S é de 0.75575227,
## uma correlação boa o que dá indicios de um possível modelo de regressão linear bom, já a 
## 