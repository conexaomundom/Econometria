

rm(list=ls())
#REGRESS?O BETA INFLACIONADA
install.packages("gamlss")
install.packages("gamlss.dist")
install.packages("xtable")
library(gamlss)
library(gamlss.dist)

# Usando o banco National_Football_League_games

# Porem como tem vari√°veis faltando, que foram preenchidas com NA
# vamos fazer imputa√ß√£o de dados utilizando as m√©dias m√≥veis
# sendo preenchidos pela m√©dia dos dois valores anteriores e seguintes
# j√° que as vari√°veis s√£o "weather_temperature", "weather_wind_mph" e 
# "weather_humidity" que s√£o a temperatua, o vento em mph  e a umidade.

attach(National_Football_League_games)
names(National_Football_League_games)

# Arredondando porque ao colocar a m√©dia ficaram n√∫mero quebrados.
weather_temperature <- round(weather_temperature)
weather_wind_mph <- round(weather_wind_mph)
weather_humidity <- round(weather_humidity)

# Modificando os escores para ficar entre 0 e 1
score_home <- score_home/100
score_away <- score_away/100



# Vamos rodar um modelo de regress√£o da beta inflacionada explicando o score home:

ajusterbezi1<-gamlss(score_home ~  (as.factor(stadium_neutral) + weather_temperature + weather_wind_mph),
                     nu.formula=~( 0 + weather_temperature),
                     sigma.formula=~(as.factor(stadium_neutral) + weather_wind_mph),family=BEZI)

xtable(summary(ajusterbezi1))


ajusterbeoi2<-stepGAIC(ajusterbezi1,what="mu")

ajusterbeoi3<-stepGAIC(ajusterbeoi2,what="nu")

ajusterbeoi4<-stepGAIC(ajusterbeoi3,what="sigma")

# vamos rodar um modelo de regress√£o da beta inflacionada explicando o score away:

#Medidas Descritivas

library(xtable)
xtable(summary(score_home))

yhatbeoi<-meanBEZI(ajusterbezi1)#diferente da regress„o beta cujo valores s„o obtidos usando "predict"
summary(yhatbeoi)

p1 = ifelse(score_home>0.5, 1, 0)
p2 = ifelse(yhatbeoi>0.5, 1, 0)

sum(p1)
sum(p2)

#ProporÁ„o de municÌpios plenamente eficientes
i<-ifelse(score_home==0,1,0)
pEFIC<-(sum(i))/length(score_home)
pEFIC

#####################Gr·ficos################################

#Densidades 

dyhatbeoi<-density(yhatbeoi)

postscript(file="Densidade.ps",horizontal=TRUE,paper="letter")
plot(dyhatbeoi,type="l",lwd=1.3,lty = 1,main="",xlab="",ylab="",ylim=c(0,12),xlim=c(0.3,1))
dev.off()

dEFIC<-density(score_home)

postscript(file="Densidades_yhat_y.ps",horizontal=TRUE,paper="letter")
plot(dEFIC,type="l",lwd=1.3,lty = 1,main="",xlab="",ylab="",ylim=c(0,12),xlim=c(0.3,1))
lines(dyhatbeoi, lty = 2)
legend(0.75, 12, c("score_home", "Inflated Beta"),lwd=1.3,lty = c(1, 2))
dev.off()


#BOXPLOT E HISTOGRAMA

histBEZI <- function(data, density = FALSE, nbreaks=8, main = paste("Histogram of", xname),
                     xlab = xname, ylab="", ...)
{
  if (!is.numeric(data)) 
    stop("'data' must be numeric")
  xname <- paste(deparse(substitute(data), 500), collapse = "\n")
  #		if(!density)
  #		{
  #		yname<- paste(deparse(substitute(Frequency), 500), collapse = "\n")
  #		}
  #		else
  #		{
  #		yname<- paste(deparse(substitute(Density), 500), collapse = "\n")
  #		}
  data<-data[!is.na(data)]
  if (any(data <= 0)|any(data > 1) ) 
    stop(paste("data must be beetwen (0, 1]", "\n", ""))
  n.data = length(data)
  #  freq.zero=length(data[data==0])	 #count of zeros
  freq.one=length(data[data==1])	 #count of ones
  data.aux=data[(data > 0 & data < 1)] #data in (0,1)
  p.hist=hist.default(data.aux,plot=FALSE,nclass=nbreaks)
  bins=p.hist$breaks
  ran.bins=bins[2]-bins[1]	
  counts<-p.hist$counts
  n<-length(counts)
  if (!density)
  {
    maxheight<-max(counts,freq.one)
    plot(c(1+(5/n.data), -(5/n.data)), c(maxheight, 0), type= "n",xlab=xlab, main=main,ylab=ylab,...)
    box()
    pr <- freq.one
    points(1, pr, type = "h", lwd=3)
    points(1, pr, type = "p", col = "black", cex=1.5)
    
    for(i in 1:n)
    {
      #frequency distribution in (0,1)		
      rect(bins[i],0,bins[i+1],counts[i],...)
      abline(h=0)
    }
  }
  else
  {
    maxheight<-max(c(counts/(ran.bins*n.data),freq.one/n.data))
    plot(c(1+(1/n.data), -(1/n.data)), c(maxheight, 0), type= "n", xlab=xlab, main=main,ylab=ylab,...)
    box() 
    pr <- freq.one/n.data
    points(1, pr, type = "h",lwd=3)
    points(1, pr, type = "p", col = "black", cex=1.5)
    #   marca = ran.bins/3
    #    A <- signif(pr*100, digits = 2)
    #    text(1-marca,pr+marca, c(A,"        %"), cex=0.7)
    for(i in 1:n)
    {
      
      #density of data in (0,1)
      rect(bins[i],0,bins[i+1],(counts[i]/(ran.bins*n.data)),...)
      abline(h=0)		
    }		
  }
}

postscript(file="HistBoxplotdeaccrI1SP.ps",horizontal=TRUE,paper="letter")
par(mfrow=c(1,2))
histBEZI(score_home, xlab="PontuaÁ„o em casa", main="", ylab = "FrequÍncia")
boxplot(score_home,horizontal=TRUE,xlab="EficiÍncia", main="")
dev.off()

#GR¡FICO RESÕDUOS 

#ConstruÌndo o Gr·fico de Envelope Beta

# ResÌduo Quantil Aleatorizado

rqbeoi<-residuals(ajusterbezi1, what = "z-scores")
summary(rqbeoi)



#ConstruÌndo o Gr·fico de Envelope BEOI

ybeoi=ajusterbeoif$y
muhatbeoi=ajusterbeoif$mu.fv
nuhatbeoi=(ajusterbeoif$nu.fv)
phihatbeoi=(ajusterbeoif$sigma.fv)

Xbeoi=ajusterbeoif$mu.x
Xbeoi<-as.matrix(Xbeoi)
Xbeoin=ajusterbeoif$nu.x
Xbeoin<-as.matrix(Xbeoin)
Xbeois=ajusterbeoif$sigma.x
Xbeois<-as.matrix(Xbeois)

n <- nrow(Xbeoi)
j<-seq(n)

yhatbeoi<-meanBEOI(ajusterbeoif)
rqbeoi<-residuals(ajusterbeoif, what = "z-scores")

sim = 19

f<- matrix(0,n,sim)

f1<- numeric(n)
f2<- numeric(n)
f3<- numeric(n)

for(i in 1:sim) {
  respbeoi<- rBEOI(n, muhatbeoi, phihatbeoi, nuhatbeoi)
  fitbeoi <- gamlss(respbeoi~Xbeoi-1,nu.formula=~Xbeoin-1,sigma.formula=~Xbeois-1,family=BEOI)
  
  
  
  
  rqbeoibeta<-residuals(fitbeoi, what = "z-scores")
  rqbeoibeta<-sort(abs(rqbeoibeta))
  
  f[,i]<-rqbeoibeta
}

for(i in 1:n) {
  
  f1[i] <- min(f[i,])
  f2[i] <- mean(f[i,])
  f3[i] <- max(f[i,])
  
}

t<-1:n
qq <- qnorm((t+n-1/8)/(2*n+0.5))


postscript(file="GraficoResiduos.ps",horizontal=TRUE,paper="letter")
par(mfrow=c(1,2))
plot(j, rqbeoi,main = "Residuals v. Ind. obs.-Beta Infl.",xlab="",ylab="", pch = "+",ylim=c(-3,4))
plot(qq, sort(abs(rqbeoi)), pch = "+", ylim=range(abs(rqbeoi), f1, f3),
     main="Half-normal plot res. - Beta Infl.",xlab="",ylab="")
lines(qq,f1,lty=1)
lines(qq,f3,lty=1)
lines(qq,f2,lty=2)
dev.off()


pdf(file="GraficoResiduos.pdf")
par(mfrow=c(1,2))
plot(j, rqbeoi,main = "Residuals v. Ind. obs.-Beta Infl.",xlab="",ylab="", pch = "+",ylim=c(-3,4))
plot(qq, sort(abs(rqbeoi)), pch = "+", ylim=range(abs(rqbeoi), f1, f3),
     main="Half-normal plot res. - Beta Infl.",xlab="",ylab="")
lines(qq,f1,lty=1)
lines(qq,f3,lty=1)
lines(qq,f2,lty=2)
dev.off()

#Pseudo R2

summary(ajusterbeoih0<-gamlss(EFIC~1,nu.formula=~1,sigma.formula=~1,family=BEOI))

ltheta<-logLik(ajusterbeoif) #logverossimilhanÁa do modelo selecionado
lthetah0<-logLik(ajusterbeoih0) #logverossimilhanÁa do modelo nulo

pR2<-1-(lthetah0/ltheta)
pR2

pR3<-1-exp((-2/n)*(ltheta-lthetah0))
pR3


##################TESTE RESET#############################

library(splines)
library(gamlss.data)

n=length(EFIC)
pw=2


y=EFIC
ystar=ifelse(y==1,0,log(y/(1-y)))

#Modelo que ser· testado
fit <- gamlss(EFIC~(E1+E4),sigma.formula=~(E4),nu.formula=~(E4),family=BEOI, trace=F)

# ADI«√O DA NOVA VARI¡VEl DE TESTE

etatest=fit$mu.lp
x1=etatest^2
x1=as.vector(x1)


fitnew <- gamlss(EFIC~(E1+E4+x1),sigma.formula=~(E4),nu.formula=~(E4),family=BEOI, trace=F)


# ESTATÕSTICA DOS TESTE

# RAZ√O DE VEROSSIMILHAN«A

lhat<-logLik(fitnew)
ltil<-logLik(fit)

Etest1=2*(lhat[1]-ltil[1])

qchisq90=qchisq(0.90,1)
qchisq95=qchisq(0.95,1)
qchisq99=qchisq(0.99,1)

print(Etest1)

print(qchisq90)
print(qchisq95)
print(qchisq99)






