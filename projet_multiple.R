#install.packages("car")
library(carData) 
library(car) # vérifier la multicollinéarité
library(ggplot2) # nuage de points
#library(QuantPsyc) #lm.beta

Data<-read.csv(file='song_data.csv',sep=",",header=TRUE)
summary(Data)

#model
reg_multi <- lm(song_popularity ~.-song_name, data = Data)
summary(reg_multi)
#plot(reg_multi)

#model 1 on retire audio_mode du model
reg_multi1<-lm(song_popularity ~.-song_name-audio_mode, data = Data)
summary(reg_multi1)
#plot(reg_multi1)

#model 2 on retire speechiness du model
reg_multi2<-lm(song_popularity ~.-song_name-audio_mode-speechiness, data = Data)
summary(reg_multi2)
#plot(reg_multi2)

#model 3 on retire key du model
reg_multi3<-lm(song_popularity ~.-song_name-audio_mode-speechiness-key, data = Data)
summary(reg_multi3)
#plot(reg_multi3)

#model 4 #on retire song_duration_ms du model
reg_multi4<-lm(song_popularity ~.-song_name-audio_mode-speechiness-key-song_duration_ms, data = Data)
summary(reg_multi4)
plot(reg_multi4)

#les coefficients
coef(reg_multi4)

confint(reg_multi4) #intervalle de confiance

fitted (reg_multi4) #les valeurs prédictes

resid(reg_multi4) #permet d'extraire les résidus (Valeur prédite - Valeur réelle).

#résidus
res<-resid(reg_multi4)
plot(res,main="Résidus")
abline(h=0,col="red")

### résidus vs. energy
plot(Data$energy,res,main="Résidus")
abline(h=0,col="red")

#test de multicollinéarité
vif(reg_multi4)
1/vif(reg_multi4)

#test de l'homoscédasticité
#install.packages("zoo")
#install.packages("lmtest")
library(zoo)
library(lmtest)
bptest(reg_multi4)

#test de la normalité des résidus
shapiro.test(reg_multi4$residuals)

library(MASS)
step<-stepAIC(reg_multi4,direction="both",trace=FALSE)
step
step1<-stepAIC(reg_multi4,direction="forward",trace=FALSE)
step1
step2<-stepAIC(reg_multi4,direction="backward",trace=FALSE)
step2

#Anderson-Darling normality test
#install.packages("nortest")
library(nortest)
ad.test(reg_multi4$residuals)

# test de l'auto-corrélation
durbinWatsonTest(reg_multi4)

# analyse des résidus
Data$résidus = rstandard(reg_multi4)
hist(Data$résidus)
Data$prévisions = fitted(reg_multi4)
nuage = ggplot(Data, aes(prévisions, résidus))
nuage + geom_point() + geom_smooth(method = "lm" ,  colour= "red") +
          labs( x= "Valeurs prédites" ,  y = "Résidus normalisés")
sigma(reg_multi4)/mean(Data$song_popularity)


#Prédiction de la popularité du song
predict(reg_multi4, newdata=data.frame(song_name="Can't Stop",song_duration_ms=2633,acousticness=0.05,danceability=0.2,energy=0.7,
                                       instrumentalness=0.1,key=0.1,liveness=2,loudness =0.2,audio_mode=0.12301,speechiness=-5.9,
                                       tempo=105.2,time_signature=3,audio_valence= 0.27),se.fit=TRUE, interval = "prediction",
                                       level = 0.99)



