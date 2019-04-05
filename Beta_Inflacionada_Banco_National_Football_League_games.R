

rm(list=ls())
#REGRESS?O BETA INFLACIONADA
install.packages("gamlss")
install.packages("gamlss.dist")
library(gamlss)
library(gamlss.dist)

# Usando o banco National_Football_League_games

# Porem como tem variáveis faltando, que foram preenchidas com NA
# vamos fazer imputação de dados utilizando as médias móveis
# sendo preenchidos pela média dos dois valores anteriores e seguintes
# já que as variáveis são "weather_temperature", "weather_wind_mph" e 
# "weather_humidity" que são a temperatua, o vento em mph  e a umidade.

attach(National_Football_League_games)
names(National_Football_League_games)

# Arredondando porque ao colocar a média ficaram número quebrados.
weather_temperature <- round(weather_temperature)
weather_wind_mph <- round(weather_wind_mph)
weather_humidity <- round(weather_humidity)

# Modificando os escores para ficar entre 0 e 1
score_home <- score_home/100
score_away <- score_away/100



# Vamos rodar um modelo de regressão da beta inflacionada explicando o score home:

summary(ajusterbezi1<-gamlss(score_home ~  (team_home + score_away + team_away + stadium + stadium_neutral + weather_temperature + weather_wind_mph + weather_humidity),
                             nu.formula=~(team_home + score_away + team_away + stadium + stadium_neutral + weather_temperature + weather_wind_mph + weather_humidity),
                             sigma.formula=~(team_home + score_away + team_away + stadium + stadium_neutral + weather_temperature + weather_wind_mph + weather_humidity),family=BEZI))


ajusterbeoi2<-stepGAIC(ajusterbezi1,what="mu")

ajusterbeoi3<-stepGAIC(ajusterbeoi2,what="nu")

ajusterbeoi4<-stepGAIC(ajusterbeoi3,what="sigma")

# vamos rodar um modelo de regressão da beta inflacionada explicando o score away:


