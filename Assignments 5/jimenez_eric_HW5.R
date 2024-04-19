library(tidyverse)
library(car) #qqPlot
library(ggplot2)
### Exercici 1
#1.1 Camí de mostra de llançament de monedes
set.seed(1234)
coin_tossing_sample_path<-function(N){
  sample<-sample(c(0,1),size=N, replace=TRUE)
  path <- cumsum(ifelse(sample==0, -1/sqrt(N), 1/sqrt(N)))
  return (path)
}

#Per N=5
coin_tossing_sample_path(5)
#Per N=10
coin_tossing_sample_path(10)
#Per N=50
coin_tossing_sample_path(50)
#Per N=100
coin_tossing_sample_path(100)
#Per N=10000
coin_tossing_sample_path(10000)

#(b) Sample path distribution
sample_path_distribution<- function(N,m){
  sample<-c()
  for(i in 1:m){
    path<-coin_tossing_sample_path(N)
    sample[i]=path[N]
  }
  return (sample)
}

#(100,100)
sample_path_distribution_100_100=sample_path_distribution(100,100)
hist(sample_path_distribution_100_100, probability = TRUE)
lines(density(sample_path_distribution_100_100))
#Test-normalitat
shapiro.test(sample_path_distribution_100_100)
#pvalue=0.5524
qqPlot(sample_path_distribution_100_100, 
       distribution = "norm",  # Reference distribution
       xlab = "Theoretical quantiles", 
       ylab = "Sample quantiles", col.lines = "steelblue"
)

#(1000,1000)
sample_path_distribution_1000_1000=sample_path_distribution(1000,1000)
hist(sample_path_distribution_1000_1000, probability = TRUE)
lines(density(sample_path_distribution_1000_1000))
#Test-normalitat
shapiro.test(sample_path_distribution_1000_1000)
#pvalue=0.1426
qqPlot(sample_path_distribution_1000_1000, 
       distribution = "norm",  # Reference distribution
       xlab = "Theoretical quantiles", 
       ylab = "Sample quantiles", col.lines = "steelblue"
)

#(10000,10000)
sample_path_distribution_10000_10000=sample_path_distribution(10000,10000)
hist(sample_path_distribution_10000_10000, probability = TRUE)
lines(density(sample_path_distribution_10000_10000))
#Test-normalitat
shapiro.test(sample_path_distribution_10000_10000)
#No funciona el Shapiro-Wilks-Test
#Podem fer un qqplot
qqPlot(sample_path_distribution_10000_10000, 
       distribution = "norm",  # Reference distribution
       xlab = "Theoretical quantiles", 
       ylab = "Sample quantiles", col.lines = "steelblue"
) #S ajusta a la linea indicant normalitat, conluim normalitat

par(mfrow = c(1,3))
qqPlot(sample_path_distribution_100_100, 
       distribution = "norm",  # Reference distribution
       xlab = "Theoretical quantiles", 
       ylab = "Sample quantiles", col.lines = "steelblue"
)
qqPlot(sample_path_distribution_1000_1000, 
       distribution = "norm",  # Reference distribution
       xlab = "Theoretical quantiles", 
       ylab = "Sample quantiles", col.lines = "steelblue"
)
qqPlot(sample_path_distribution_10000_10000, 
       distribution = "norm",  # Reference distribution
       xlab = "Theoretical quantiles", 
       ylab = "Sample quantiles", col.lines = "steelblue"
)
### Exercici 2
#(a) Construcció d'un camí de mostra sota el model de Black-Scholes
set.seed(1234)
path_sample <- function(N, t0, tn, S0, r, sigma) {
  path <- numeric(N+1)
  path[1] = S0
  
  dt = (tn-t0)/N
  
  for (i in 2:(N+1)) {
    path[i] = path[i-1] + r*path[i-1]*dt + sigma*path[i-1]*sqrt(dt)*rnorm(1)
  }
  
  return(path)
}

# Exemple donat
N=1000
t0=0
tn=1
S0=100
r=0.01
sigma=0.3

plot(seq(from=t0, to=tn, by=1/N), path_sample(N,t0,tn,S0,r,sigma),type="l",xlab = "Temps", ylab = "Preu")

#####################################################################################
#############(PROBES PER TENIR UNA IDEA, NO APAREIXEN A L'INFORME)###################
#####################################################################################
#####################################################################################
# Plots amb diferents volatilitats 
par(mfrow = c(2, 2))
# 1. sigma=0.15
plot(seq(from=t0, to=tn, by=1/N), path_sample(N,t0,tn,S0,r,0.15),type="l",xlab = "Temps", ylab = "Preu", main="sigma=0.15")
# 2. sigma=0.5
plot(seq(from=t0, to=tn, by=1/N), path_sample(N,t0,tn,S0,r,0.6),type="l",xlab = "Temps", ylab = "Preu", main="sigma=0.6")
# 3. sigma=0.05
plot(seq(from=t0, to=tn, by=1/N), path_sample(N,t0,tn,S0,r,0.05),type="l",xlab = "Temps", ylab = "Preu",main="sigma=0.05")
# 4. sigma=0.9
plot(seq(from=t0, to=tn, by=1/N), path_sample(N,t0,tn,S0,r,0.9),type="l",xlab = "Temps", ylab = "Preu",main="sigma=0.9")
# Plots amb diferents tipus d'interès
# 1. r=0.05
plot(seq(from=t0, to=tn, by=1/N), path_sample(N,t0,tn,S0,0.0075,sigma),type="l",xlab = "Temps", ylab = "Preu", main="r=0.005")
# 2. r=0.05
plot(seq(from=t0, to=tn, by=1/N), path_sample(N,t0,tn,S0,0.02,sigma),type="l",xlab = "Temps", ylab = "Preu", main="r=0.05")
# 3. r=0.0025
plot(seq(from=t0, to=tn, by=1/N), path_sample(N,t0,tn,S0,0.000025,sigma),type="l",xlab = "Temps", ylab = "Preu",main="r=0.0025")
# 4. r=0.1
plot(seq(from=t0, to=tn, by=1/N), path_sample(N,t0,tn,S0,0.05, sigma),type="l",xlab = "Temps", ylab = "Preu",main="r=0.1")

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
## ESTUDI DEFINITIU (INFORME)

# Estudi de com varia la gràfica en funció de r i sigma
price_oscilation <- function(M,N,t0,tn,S0,r,sigma){
  sum <- 0
  for(i in 1:M){
    path <- path_sample(N,t0,tn,S0,r,sigma)
    sum = sum + (max(path) - min(path))
  }
  return(sum/M)
}

p <- c()
for(i in 1:20){
  p[i] <- price_oscilation(100,N,t0,tn,S0,r,i*0.05)
}

points <- data.frame(x = seq(0.05,1,by=0.05),
                     y = p)

ggplot(points, aes(x = x, y = y)) +
  geom_line() +         
  geom_point(size = 2) + 
  theme_minimal() +      
  labs(
    title = "Oscil·lació màxima del preu en funció de la volatilitat",
    x = "sigma",
    y = "Variació màxima"
  )

price_difference <-function(M,N,t0,tn,S0,r,sigma){
  sum <- 0
  for(i in 1:M){
    path <- path_sample(N,t0,tn,S0,r,sigma)
    sum = sum + (path[N+1] - path[1])
  }
  return(sum/M)
}

p <- c()
for(i in 1:200){
  p[i] <- price_difference(100,N,t0,tn,S0,i*0.001,sigma)
}

points <- data.frame(x = 0.001*1:200,
                     y = p)

ggplot(points, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(method = "lm",         # Añadir regresión lineal
              se = FALSE,            # No mostrar el intervalo de confianza
              color = "steelblue") + 
  theme_minimal() +      
  labs(
    title = "Diferència entre el preu final i inicial en funció de l'interés",
    x = "r",
    y = "Diferència final-inicial"
  )

abline(lm(points$y~points$x))



# (b)
#Funció de pagament d'una call-option
payoff_function=function(S,K){
  return (max(S-K,0))
}

# (c) 
#M nombre de camins de la mostra
# payoff_function: determina el benefici de l'opció
monte_carlo=function(M,N,t0,tn,S0,r,sigma,payoff_function,K){
  v <- c()
  for(i in 1:M){
    path<-path_sample(N,t0,tn,S0,r,sigma)
    v[i]<-payoff_function(path[N+1],K)
  }
  preu = mean(v)*exp(-r*(tn-t0))
  return (preu)
}

#(d) 
#Funció de pagament d'una put-option
payoff_function_put=function(S,K){
  return (max(K-S,0))
}
#Preu d'un PUT amb
N=1000
t0=0
tn=1   #1 any

r=0.05
sigma=0.4
S0=90
K=75
M=1000 #posem M=N

#Calculem una tirada
monte_carlo(M,N,t0,tn,S0,r,sigma,payoff_function_put,K)
#Mitjana fent 10 repeticions
mean(unlist(Map(function(i)monte_carlo(M,N,t0,tn,S0,r,sigma,payoff_function_put,K), 1:10)))
#Mitjana fent 100 repeticions
mean(unlist(Map(function(i)monte_carlo(M,N,t0,tn,S0,r,sigma,payoff_function_put,K), 1:100)))
