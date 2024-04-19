####  HW_1 ####
library(tseries)
library(forecast)
library(lmtest)
library(evir)

### Els exercicis 2 i 3 no han siguts realitzats en R (no calia programar).

#EXERCICI 1

data ( Tbrate , package ="Ecdat") 
library ("tseries") 
# r = the 91 - day treasury bill rate 
# y = the log of real GDP 
# pi = the inflation rate

# Questions a ) and b ) 
head(Tbrate)
plot (Tbrate, main = "")
title(main = "Sèries temporals")
acf(Tbrate)

acf(Tbrate[ ,1], main = "")
title(main = "Autocorrelació - r")

acf(Tbrate[ ,2], main = "")
title(main = "Autocorrelació - y")

acf(Tbrate[ ,3], main = "")
title(main = "Autocorrelació - pi")
# Consider only the diagonal plots which correspond to the ACF plots of r, y and pi
adf.test ( Tbrate [ ,1]) 
adf.test ( Tbrate [ ,2]) 
adf.test ( Tbrate [ ,3])


# Questions c ) and d )
diff_rate = diff (Tbrate)

plot(diff_rate, main = "")
title(main = "Sèries temporals diferenciades")
acf(diff_rate)

adf.test(diff_rate [ ,1])
adf.test(diff_rate [ ,2])
adf.test(diff_rate [ ,3])


#Questions e), f), g) and h)
library (forecast)
auto.arima ( Tbrate [ ,1] , max.P=0 , max.Q=0 , ic="aic")
auto.arima ( Tbrate [ ,1] , max.P=0 , max.Q=0 , ic="bic")

#Questions i )
fit1 = arima ( Tbrate [ ,1] , order =c(0 ,1 ,1) )
acf (residuals ( fit1 ))
checkresiduals(fit1 ,lag =10)

### EXERCICI 4
# Utilitzeu les sèries temporals i els diagrames ACF per determinar la quantitat 
# de diferenciació necessària per obtenir una sèrie estacionària.
library(Ecdat)
data(Mishkin)
head(Mishkin)
tb1=log(Mishkin[,3])

# Les dades ja són una sèrie temporal per tant podem fer un plot().
par(mfrow=c(1,2))
ts.plot(tb1)
abline(h=mean(tb1), col="red", lty=2)
acf(tb1)


#Com que la sèrie no és estacionària, mirem com és la sèrie diferenciada.
tb1_diff = diff(tb1)
plot(tb1_diff)
abline(h=mean(tb1_diff), col="red",lty=2)
acf(tb1_diff)

#Fem un test Dickey per veure si la sèrie (que sembla) és o no estacionaria
adf.test(tb1_diff)

## Utilitza la funció auto.arima per determinar la millor aproximació del model 
# no estacionari ARIMA.

# Utilitzem en primer lloc el criteri "AIC" per triar el resultat.
model_aic=auto.arima(tb1, max.P=0 , max.Q=0 , ic="aic")
model_aic
coeftest(model_aic)

#Mirem l'AIC i BIC de ARIMA(3,1,3)
fit=arima(x=tb1, order=c(3,1,3))
fit
#Calculem el BIC
AIC(fit,k=log(length(tb1)))

#Ara ho fem amb el criteri "BIC" per triar el resultat
model_bic=auto.arima(tb1, max.P=0 , max.Q=0 , ic="bic")
model_bic
coeftest(model_bic)


#c) Examinem la ACF dels residus del models seleccionats.
acf(model_bic$residuals, main="ACF of ARIMA(0,1,1) residuals")
acf(fit$residuals, main="ACF of ARIMA(3,1,3) resiuals")

#Fem una prova Ljung-box
checkresiduals(model_bic, lag=100)
checkresiduals(fit, lag=100)

