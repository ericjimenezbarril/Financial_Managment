#######################
#######################
library(quantmod)
library(tidyverse)
#######################
#######################

#EXERCICI 1

#Download data

getSymbols("META", scr = "yahoo", from = "2015-01-01", to = "2015-12-31")
Facebook <- diff(META$META.Adjusted)[-1] / META$META.Adjusted[-(length(META$META.Adjusted))]

getSymbols("AAPL", scr = "yahoo", from = "2015-01-01", to = "2015-12-31")
Apple <- diff(AAPL$AAPL.Adjusted)[-1] / AAPL$AAPL.Adjusted[1:(length(AAPL$AAPL.Adjusted) - 1)]

getSymbols("MSFT", scr = "yahoo", from = "2015-01-01", to = "2015-12-31")
Microsoft <- diff(MSFT$MSFT.Adjusted)[-1] / MSFT$MSFT.Adjusted[1:(length(MSFT$MSFT.Adjusted) - 1)]

getSymbols("GOOG", scr = "yahoo", from = "2015-01-01", to = "2015-12-31")
Google <- diff(GOOG$GOOG.Adjusted)[-1] / GOOG$GOOG.Adjusted[1:(length(GOOG$GOOG.Adjusted) - 1)]

#Returns and covariances

mu <- c(mean(Facebook), mean(Apple), mean(Microsoft), mean(Google))
mu <- matrix(mu, ncol = 1)
mu

C <- matrix(cov(data.frame(Facebook,Apple,Microsoft,Google)),ncol=4)
C

#Portfolio weights simulation
set.seed(123)

W <- runif(4)
W <- matrix(W/sum(W),ncol = 1)

mu_P <- t(W) %*% mu

sigmasq_P <- t(W) %*% C %*% W

c(mu_P, sigmasq_P)

#Feasible set

mu_P <- c()
sigmasq_P <- c()
for (i in 1:1000) {
  W <- runif(4)
  W <- matrix(W/sum(W),ncol = 1)
  
  mu_P <- c(mu_P, t(W) %*% mu)
  
  sigmasq_P <- c(sigmasq_P, t(W) %*% C %*% W)
}

sigma_P <- sqrt(sigmasq_P)

plot(sigma_P,mu_P, col="blue")


#EXERCICI 2 & 3 

N <- 500
rbase <-  seq(min(mu),max(mu),length=N)

m1 <- matrix(c(1,1,1,1),ncol =1)

fila1 <- cbind(2*C, mu, m1)
fila2 <- cbind(t(mu), 0, 0)
fila3 <- cbind(t(m1), 0, 0)

Q <- rbind(fila1,fila2,fila3)
Q

mu_frontera <- c()
sigmasq_frontera <- c()

for (r in rbase) {
  b <- matrix(c(0,0,0,0,r,1),ncol=1)
  y <- solve(Q,b)
  W <- y[1:4]
  mu_frontera <- c(mu_frontera, t(W) %*% mu)
  sigmasq_frontera <- c(sigmasq_frontera, t(W) %*% C %*% W)
}
#Frontera
#Optimitz
sigma_frontera <- sqrt(sigmasq_frontera)
which(mu_P>mu_P[which(sigma_frontera==min(sigma_frontera))])
plot(sigma_P,mu_P, col="white", main="Optimitzacio de la cartera")
lines(sigma_frontera,mu_frontera, col="black", lwd=2)
lines(sigma_frontera[which(mu_frontera>mu_P[which(sigma_P==min(sigma_P))])],mu_frontera[which(mu_frontera>mu_P[which(sigma_P==min(sigma_P))])], col="red", lwd=2)
legend("bottomright", legend = c("Frontera eficient","Frontera No Eficient"), col = c("red", "black"),pch=c(1,1) ,lty = c(1,1), cex=0.8)


#Optimització de la cartera

sigma_frontera <- sqrt(sigmasq_frontera)
which(mu_P>mu_P[which(sigma_frontera==min(sigma_frontera))])
plot(sigma_P,mu_P, col="blue", main="Optimitzacio de la cartera")
lines(sigma_frontera,mu_frontera, col="black", lwd=2)
lines(sigma_frontera[which(mu_frontera>mu_P[which(sigma_P==min(sigma_P))])],mu_frontera[which(mu_frontera>mu_P[which(sigma_P==min(sigma_P))])], col="red", lwd=2)
which(sigma_P==min(sigma_P))
points(min(sigma_P), mu_P[886], col="green", lwd=2, pch=16)
legend("bottomright", legend = c("Conjunt factible","Frontera eficient", "Portfoli de variància mínima", "Frontera"), col = c("blue", "red", "green", "black"),pch=c(16,1,16,1) ,lty = c(0,1,0,1), cex=0.8)

