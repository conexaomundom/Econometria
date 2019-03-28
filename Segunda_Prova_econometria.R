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

