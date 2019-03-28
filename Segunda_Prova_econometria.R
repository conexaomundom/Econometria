######33
#######
#########

install.packages("ggplot2")
library(ggplot2)
banco
attach(banco)

# Diagramas de dispersao

graf_disp <- ggplot(banco, aes(x=Paladar, y=Acido_Acetico)) + geom_point(aes(col=H2_S)) +
  geom_point(aes(col=Acido_Latico))+
  geom_smooth(method="loess", se = F) + labs(subtitle = "Diagrama de dispersão do paladar com relação ao Ácido Acético, ao H2S e ao Ácido Latico.", 
                            y = "Ácido Acético", x = "Paladar", caption = "Fonte: Dados retirado do livro Gujarati e Porter")

plot(graf_disp)

plot(banco[,-1])

# Gráfico 1
plot(x,y, xlim=c(0,8), ylim=c(0,27), col='red', xlab='Tempo', ylab='Peso')
points(x,z, pch=2, col='dark blue')
points(x,h, pch=3, col= 'dark green')
legend(5.5,26, c('Tratamento A ', 'Tratamento B ', 'Tratamento C '), col=c('red', 'dark blue', 'dark green'), pch=c(1,2,3))


# Gráfico 2
plot(x,y, xlim=c(0,8), ylim=c(0,27), xlab='Tempo', ylab='Peso', type='o')
points(x,z, pch=2, type='o')
points(x,h, pch=3, type='o')
legend(5.5,26, c('Tratamento A ', 'Tratamento B ', 'Tratamento C '), pch=c(1,2,3))





# Gráfico 3
plot(x,y, xlim=c(0,8), ylim=c(0,27), col='red', xlab='Tempo', ylab='Peso', type='o',  col.lab=2, col.axis='light green', font=2, lwd=4, main='Gráfico 3', col.main=5, sub='Subtítulo', col.sub='grey')
points(x,z, pch=2, col='dark blue', type='o', lwd=4)
points(x,h, pch=3, col= 'dark green', type='o', lwd=8)
legend(5.5,26, c('Tratamento A ', 'Tratamento B ', 'Tratamento C '), col=c('red', 'dark blue', 'dark green'), pch=c(1,2,3))



# Gráfico 4
plot(x,y, xlim=c(0,8), ylim=c(0,27), col='red', xlab='Tempo', ylab='Peso', type='o', col.lab=2, col.axis='light green', font=2, lwd=4, main='Gráfico 3', col.main=5, sub='Subtítulo', col.sub='grey', lty=1)
points(x,z, pch=2, col='dark blue', type='o', lwd=4, lty=10)
points(x,h, pch=3, col= 'dark green', type='o', lwd=8, lty=3)
legend(5.5,26, c('Tratamento A ', 'Tratamento B ', 'Tratamento C '), col=c('red', 'dark blue', 'dark green'), pch=c(1,2,3))


r = function(x) {1.9+1.7*x} 
r1= function(x) { 0.4+4.2*x} 
r2= function(x) { 3.9+1.5*x} 





# Gráfico 5 (plot dos pontos e das equações, com duas legendas)
plot(x,y, xlim=c(0,8), ylim=c(0,27), xlab='Tempo', ylab='Peso', bty="l")
plot(r, 0.5,5, lty=4, add=T)
plot(r1, 0.5, 5, add=T, lty=1)
plot(r2, 0.5, 5, add=T, lty=2)
points(x,z, pch=2)
points(x,h, pch=3)
legend(5.5,18, c('Tratamento A ', 'Tratamento B ', 'Tratamento C '), pch=c(1,2,3))
legend("topright", c('Tratamento A ', 'Tratamento B ', 'Tratamento C '), lty = c(4,1,2))
