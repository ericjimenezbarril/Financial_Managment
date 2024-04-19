##################
#Carreguem les llibreries
library(zoo)
library(forecast)
library(stats)
library(rugarch)
library(tseries)
library(lmtest)
library(fGarch)
library(car) #qqPlot
library(xtable)
library(goft)
library(Ecfun)
library (Ecdat)


### EXERCICI 1 ###
##################

#Carreguem les dades
data(Tbrate ,package="Ecdat") 
Tbill=Tbrate[,1]
Del.Tbill=diff(Tbill)


#a - Estudiem l'estacionarietat de la sèrie
plot(Tbill)
plot(Del.Tbill)
acf(Del.Tbill)
pacf(Del.Tbill)

#b - Ajustem les dades a un model ARMA(1,0)/GARCH(1,0)
garch.model=garchFit(formula=~arma(1,0)+garch(1,0),Del.Tbill) 
summary(garch.model)
garch.model@fit$matcoef

#c - Estudiem els residus ordinaris
res=residuals(garch.model) 
res_std=res/garch.model@sigma.t 
plot(res)
acf(res) 
Box.test(res, lag = 9, type ="Ljung")

#d - Estudiem els residus ordinaris al quadrat
acf(res^2) 
Box.test(res^2, lag =9, type ="Ljung")

#e - Estudiem els residus estandarditzats
plot(res_std) 
acf(res_std^2)
Box.test(res_std^2, lag =9, type ="Ljung")

#f - Estudiem els residus estandarditzats al quadrat
acf(res_std) 
Box.test(res_std, lag =9, type ="Ljung")

#g - Repetim el procediment per la sèrie diff(log(Tbill))

log.Del.Tbill <- diff(log(Tbill))
plot(log.Del.Tbill)

#Ajustem les dades a un model ARMA(1,0)/GARCH(1,0)
log.garch.model=garchFit(formula=~arma(1,0)+garch(1,0),log.Del.Tbill) 

log.res = residuals(log.garch.model)
log.res_std=log.res/log.garch.model@sigma.t 

par(mfrow=c(2,2))

#Estudiem els residus ordinaris
acf(log.res)
Box.test(log.res, lag =9, type ="Ljung")

#Estudiem els residus ordinaris al quadrat
acf(log.res^2)
Box.test(log.res^2, lag =9, type ="Ljung")

#Estudiem els residus estandarditzats
acf(log.res_std)
Box.test(log.res_std, lag =9, type ="Ljung")

#Estudiem els residus estandarditzats al quadrat
acf(log.res_std^2)
Box.test(log.res_std^2, lag =9, type ="Ljung")


##################
### EXERCICI 2 ###
##################


data("SP500", package = "Ecdat")
SP500
returnBM = SP500$r500[1805]
x= SP500$r500[(1804-2*253+1):1804]
plot(c(x, returnBM), tipus='l')
results = garchFit(~ arma(1,0) + garch(1,1), data=x, cond.dist="std")
dfhat =as.numeric(results@fit$par[6])
forecast = predict(results, n.ahead=1)

##################

# APARTAT a)
# Utilitza la informació anterior per calcular la probabilitat condicional 
# d’una rendibilitat menor o igual a -0,228 el Dilluns Negre.

mu = forecast$meanForecast
sigma = forecast$standardDeviation
z = returnBM/sigma
probabilitat = pt(z, df = dfhat)
probabilitat

# APARTAT b)
# Calcula i representa els residus estandarditzats. També representa l’ACF 
# dels residus i els seus quadrats. Indiquen un ajust del model adequat?

residus = residuals(results)
residus_estandaritzats = residuals(results, standardize = TRUE)

ts.plot(residus)
ts.plot(residus_estandaritzats, main = "Residus Estanderitzats", ylab = "Residus Estanderitzats")

acf(residus_estandaritzats, main = "ACF Residus Estanderitzats")
acf(residus_estandaritzats^2, main = "ACF Quadrat Residus Estanderitzats")

acf(residus, main = "ACF Residus")              # També incloem els ACF plots  
acf(residus^2, main = "ACF Quadrat Residus")    # dels residus i els quadrats

# APARTAT c)
# Proporcionaria un model AR(1)/ARCH(1) un ajust adequat?

results2 = garchFit(~ arma(1,0) + garch(1,0),   
                    data=x, cond.dist="std")    # Canviem de model

residus2 = residuals(results2) 
residus_estandaritzats2 = residuals(results2, standardize = TRUE) 

acf(residus_estandaritzats2, main = "ACF Residus Estanderitzats")
acf(residus_estandaritzats2^2, main = "ACF Quadrat Residus Estanderitzats")

acf(residus2, main = "ACF Residus")             # També incloem els ACF plots
acf(residus2^2, main = "ACF Quadrat Residus")   # dels residus i els quadrats
# i els Ljung-Box Test

Box.test(residus_estandaritzats2, lag =10, type ="Ljung")
Box.test(residus_estandaritzats2^2, lag =10, type ="Ljung")


# APARTAT d)
# Proporciona un model AR(1) un ajust adequat?

results3 = arima(x, c(1,0,0))                   # Canviem de model

residus3 = residuals(results3) 

acf(residus3, main = "ACF Residus")
acf(residus3^2, main = "ACF Quadrat Residus")

Box.test(residus3, lag = 10, type = "Ljung")    # També incloem els
Box.test(residus3^2, lag = 3, type = "Ljung")   # Ljung-Box Test


##################
### EXERCICI 3 ###
##################
rm(list=ls())
cat("\f")

### Segueix el model de selecció pas a pas per estimar
# un model ARMA(p,q)/GARCH(P,Q) per les dades. (Hint: q=0 i les altres són menor o igua lque 2)

Data<-load("C:/Users/ERIC/Desktop/3. GESTIÓ/PRÁCTICA 2/data_HW_3.Rdata")
data<-ts(x); data
par(mfrow=c(1,1))
plot(data)
abline(h=mean(data),lty=2 ,col="red")

#Tracen l'ACF i PACF de la sèrie original
par(mfrow=c(1,2))
Acf(data)       
Pacf(data)        
#En primer lloc, predim el model ARMA 
(fit1<-auto.arima(data))
coeftest(fit1)   
#AIC=1880.56 i BIC=1901.51

#Estudiem ara un segon model
fit2<-arima(data, c(2,0,0)); fit2   # AIC=1880.08
BIC(fit2)                           # BIC=1896.941

coeftest(fit2) #Notem que els 2 coeficients AR són significatius.


#Podem comprovar si el model ARIMA(1,0) modela bé les nostres dades
fit3<-arima(data, c(1,0,0));fit3
BIC(fit3)         ## NOtem que tant l'AIC com el BIC són pitjors,

#Ens quedem doncs amb un ARIMA(2,0)

#Mirem la significació dels coeficients
coeftest(fit2)

#Estudiem els residus
checkresiduals(fit2)
par(mfrow=c(1,1))
plot(fit2$residuals)
abline(h=mean(fit2$residuals), col="red", lwd=1.5)
par(mfrow=c(1,2))
Acf(fit2$residuals)
Pacf(fit2$residuals)

#Pasem a estudiar el model ARMA(2,0)/GARCH(P,Q), trobem P i Q.
plot(data^2)
par(mfrow=c(1,2))
Acf(data^2)         
Pacf(data^2)        


#Després de les estimacions qualitatives, podem pensar
# p=2, d=0, q=0, P=1 i Q=2.

q<-c(0,0,1,1,2,2)
p<-c(1,2,1,2,1,2)
coefs<-q+p+1


#Veiem el que ens recomana la funció garchFit
gfit1 <- garchFit(~arma(2,0)+garch(1,0), data=data, cond.dist="std") #ARMA(2,0)/GARCH(1,0)
gfit2 <- garchFit(~arma(2,0)+garch(2,0), data=data, cond.dist="std") #ARMA(2,0)/GARCH(2,0)
gfit3 <- garchFit(~arma(2,0)+garch(1,1), data=data, cond.dist="std") #ARMA(2,0)/GARCH(1,1)
gfit4 <- garchFit(~arma(2,0)+garch(2,1), data=data, cond.dist="std") #ARMA(2,0)/GARCH(2,1)
gfit5 <- garchFit(~arma(2,0)+garch(1,2), data=data, cond.dist="std") #ARMA(2,0)/GARCH(1,2)
gfit6 <- garchFit(~arma(2,0)+garch(2,2), data=data, cond.dist="std") #ARMA(2,0)/GARCH(2,2)
summary(gfit1) #AIC = 3.185562 , BIC=3.236138 
summary(gfit2) #AIC = 3.193095 , BIC=3.252099
summary(gfit3) #AIC = 3.189535 , BIC=3.248539 
summary(gfit4) #AIC = 3.197095 , BIC=3.264529  
summary(gfit5) #AIC = 3.197106 , BIC=3.264540 
summary(gfit3) #AIC = 3.189535 , BIC=3.248539 
AICf=c(3.185562, 3.193095,3.189535, 3.197095,3.197106,3.189535)
BICf=c(3.236138,3.252099,3.248539,3.264529,3.264540,3.248539)
print(data.frame(p,q, AICf, BICf))


#Ara podem fer amb el package rugarch 
umod1<- ugarchspec(variance.model = list(garchOrder=c(1,0)), mean.model=list(armaOrder=c(2,0)))  #ARMA(2,0)/GARCH(1,0)
ufit1<-ugarchfit(umod1,data)
umod2<- ugarchspec(variance.model = list(garchOrder=c(2,0)), mean.model=list(armaOrder=c(2,0)))  #ARMA(2,0)/GARCH(1,0)
ufit2<-ugarchfit(umod2,data)
umod3<- ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(2,0)))  #ARMA(2,0)/GARCH(1,0)
ufit3<-ugarchfit(umod3,data)
umod4<- ugarchspec(variance.model = list(garchOrder=c(2,1)), mean.model=list(armaOrder=c(2,0)))  #ARMA(2,0)/GARCH(1,0)
ufit4<-ugarchfit(umod4,data)
umod5<- ugarchspec(variance.model = list(garchOrder=c(1,2)), mean.model=list(armaOrder=c(2,0)))  #ARMA(2,0)/GARCH(1,0)
ufit5<-ugarchfit(umod5,data)
umod6<- ugarchspec(variance.model = list(garchOrder=c(2,2)), mean.model=list(armaOrder=c(2,0)))  #ARMA(2,0)/GARCH(1,0)
ufit6<-ugarchfit(umod6,data)

infocriteria(ufit1) # AIC = 3.171588, BIC = 3.213734
infocriteria(ufit2) # AIC = 3.175435, BIC = 3.226011
infocriteria(ufit3) # AIC = 3.175441, BIC = 3.226017
infocriteria(ufit4) # AIC = 3.179435, BIC = 3.238440
infocriteria(ufit5) # AIC = 3.179441, BIC = 3.238446
infocriteria(ufit6) # AIC = 3.183435, BIC = 3.250869



# Mirem ara que tots els paràmetres són significatius

#Ho fem per exemple amb la última aproximació (totes hauriem de ser similars)


#Mirem que tots els paràmetres siguin significatius
ufit1       
#Obtenim que tots 4 paràmetres són significatius. (La mu es la mitjana que no es un paràmetre a estudiar)
#Per tant tots els paràmetres són estadísticament significatius

#Comprovem per últim els residus.
#Els tracem graficament
res_ufit1 = ts(residuals(ufit1, standardize=TRUE))
par(mfrow=c(1,1))
plot(res_ufit1, col="red")
#Grafiquem les funcions ACF i PACF
par(mfrow=c(1,2))
Acf(res_ufit1)
Pacf(res_ufit1)      #Clarament és suroll blanc doncs són trivials ambdues dos gràfics

#Els residus per tant són clarament estacionaris, incorrelacionats
#Fem un Augmented Dickey-Fuller and a Ljung-Box test per confirmar-lo
adf.test(res_ufit1) #Són estacionaris
Box.test(res_ufit1,lag=1, type="Ljung-Box")
Box.test(res_ufit1,lag=12, type="Ljung-Box")
Box.test(res_ufit1,lag=24, type="Ljung-Box")
Box.test(res_ufit1,lag=36, type="Ljung-Box")

#Per últim podriem comprovar si els residus segueixen una distribució en concret.
#En particular podem veure si segueixen una distribució normal.

#Comprovació gràfica
par(mfrow=c(1,1))
qqPlot(res_ufit1, 
       distribution = "norm",  # Reference distribution
       xlab = "Theoretical quantiles", 
       ylab = "Sample quantiles", col.lines = "steelblue"
) #Graficamente parece que los residuos siguen una distribución normal.
hist(res_ufit1, breaks=100, probability = TRUE)
lines(density(res_ufit1), col = "red", lwd = 2) 


#Fem un shapiro-test ja que  no queda gaire clar
shapiro.test(res_ufit1)
#pvalor = 0.3599 por tanto no podemos rechazar la hipótesi nula d que los residuos estandarizados siguen una distribución normal.
###########################################################################################################################################

