# HW 8 
library(ggplot2)
library(tidyverse)
# 1
data("SP500", package = "Ecdat")
SP500
returnBM = SP500$r500[1805]
Returns_SP500= SP500$r500[(1804-2*253+1):1805]
plot.ts(Returns_SP500,ylab=("Rendibilitats S&P 500"))
title("Rendibilitats dels 2 últims anys incloent el Black Monday")
points(507, returnBM, col="black", pch=16)
rect(507-28, returnBM+0.03, 507-100, returnBM+0.05, col = "lightgray", border = "black")
text(507-20, returnBM+0.04, labels = c("Black Monday"), pos = 2, cex=0.6)
arrows(507-30, returnBM+0.03, 507-2, returnBM+0.005, length = 0.1)
# a) 
ts_wBM = SP500$r500[(1804-2*253+1):1804]
hist=hist(ts_wBM, breaks = 30, probability = TRUE, main= "Histograma de les rendiblitats",
     xlab="Temps", ylab="Returns")
lines(density(ts_wBM), col="blue", lwd=1.5)

# b) 
q1=quantile(ts_wBM, 0.01); q1
#histograma quantil
hist=hist(ts_wBM, breaks = 60, probability = TRUE, main= "Histograma de les rendibilitats",
          xlab="Temps", ylab="Rendiblitats")
lines(density(ts_wBM), col="blue", lwd=1.5)
abline(v=q1, col="red", lty=2)

#Calculem el VaR tail
mean(ts_wBM[ts_wBM<q1])


# 2)
# EXERCICI 2 
set.seed(1234)
N=1000 
t0=0 
tn=1 
S0=90 
r=0.05 
sigma=0.40 
K = 75 
M = 10000 
Prima <- 5.576904 
path_sample <- function(N,t0,tn,S0,r,sigma){ 
  output <- c(S0) 
  for(i in 1:N){ 
    output[i+1] = output[i] + r*output[i]*(tn-t0)/N + sigma*output[i]*sqrt((tn-t0)/N)*rnorm(1) 
  } 
  return(output) 
} 

monte_carlo <- function(M,N,t0,tn,S0,r,sigma,payoff_function,K){ 
  sum <- 0 
  P <- c() 
  for(i in 1:M){ 
    # 1. Simulate a path for S and take SN 
    SN <- path_sample(N,t0,tn,S0,r,sigma)[N+1] 
    # 2. Apply the payoff funtion to SN 
    P[i] <- payoff_function(SN,K) }  
  # 4. Average it and calculate the price of the option 
  return(P) } 
payoff_function <- function(S,K){ 
  return(-max(0,K-S) + Prima) 
} 

PUT <- monte_carlo(M,N,t0,tn,S0,r,sigma,payoff_function,K) 
PUT
V <- quantile(PUT, 0.01) 
V 
V_tail <- mean(PUT[PUT<V])
V_tail

ggplot(data.frame(PUT_val = PUT), aes(x = PUT_val))+ 
  geom_histogram(binwidth = 5, fill = "grey", color = "black") + 
  geom_vline(xintercept = V, color = "red", linetype = "dashed", size = 1) + 
  theme_minimal() + 
  labs(title = "Valor del Value at Risk", x = "Benefici/Pèrdua", y = "Frecuencia") 

ggplot() + 
  geom_histogram(data = subset(data.frame(PUT_val = PUT), PUT_val <= V), 
        aes(x = PUT_val), binwidth = 1, fill = "grey", color = "black", alpha = 1) + 
  geom_histogram(data = subset(data.frame(PUT_val = PUT), PUT_val > V+0.5 & PUT_val< -30), 
        aes(x = PUT_val), binwidth = 1, fill = "grey", color = "grey", alpha = 0.1) + 
  geom_vline(xintercept = V, color = "red", linetype = "dashed", size = 1, alpha = 1) + 
  geom_vline(xintercept = V_tail, color = "blue", linetype = "dashed", size = 1, alpha = 1) + 
  theme_minimal() + 
  labs(title = "Valor del tail Value at Risk", x = "Benefici/Pèrdua", y = "Frecuencia")

