### HOME WORK 0: INTRODUCTION TO FINANCIAL ENGINEERING HWO


#Abril Pérez Martí  (1600601)
#Arnau Perich Iglesias (1603567)
#Eric Jiménez Barril (1599092)´
#Joan González Martínez (1597201)
#Laia Escursell Rof   (1600578)



#The following code uses library "quantmod" to download financial datasets, 
#in particular closing prices for Google and S&P composite index.

#Stock evolution

rm(list = ls())                           #Removes all variables from workspace
library("quantmod")                       #Library to get prices from Yahoo
library("moments")                        #Library to calculate Skewness and Kurtosis
library("tidyverse")
library("dplyr")
library("fBasics")
library("carData")
library("car")

options("getSymbols, warning4.0"=FALSE)   #Set of warnings
cat("\f")                                 #Clear console

getSymbols("GOOG", scr="yahoo", from="2011-01-03", to="2014-12-31")
google_close = GOOG$GOOG.Close          #time.serie of closing prices for Google 

getSymbols("SP", scr="yahoo", from="2011-01-03", to="2014-12-31")
SP_close = SP$SP.Close                    #time.serie of closing prices for S&P composite index


# Exercise 1: Consider the daily net returns (from the Close price) of Google (GOOG) stock 
#and the S&P compositeindex (SP) from January 3, 2011 to December 31, 2014.

# (a) Compute the sample mean, standard deviation, skewness, excess kurtosis, minimum and maximum
#of each net return series.

#We are lucky that the data is in time series format, so we can operate on it as such
(net.retGOOG<- (diff(google_close)/lag(google_close))[-1])          ## Daily net returns from Close price series from GOOG
(net.retSP  <- (diff(SP_close)/lag(SP_close))[-1])                    ## Daily net returns from Close price series from GOOG

#We convert the data to numerical format to be able to work with them
numeric<-data.frame(
  GOOG= as.numeric(net.retGOOG),
  SP= as.numeric(net.retSP)
)


# Now, we are able to calculate the mean, sd, skewness, excess kurtosis, minimum and maximum of each net return series
data <- data.frame(
  mean           = as.numeric(colMeans(numeric)),       #Sample mean
  sd             = apply(numeric, 2, sd),               #Sample standard deviation
  skewness       = apply(numeric, 2, skewness),         #Sample skewness
  excess_kurtosis= apply(numeric, 2, kurtosis),         #Sample excess kurtosis
  minimum        = apply(numeric, 2, min),              #Sample minimum
  maximum        = apply(numeric, 2, max)               #Sample maximum
); data


#b) Obtain the empirical density function of the net returns of Google stock.

#We plot the empirical density function of the net returns
plot(density(net.retGOOG), main = "Denistat Empírica", lwd = 2)
polygon(density(net.retGOOG), col = "lightblue")

#We plot histogram function of the net returns
hist(net.retGOOG, breaks = 40, probability = T)
lines(density(net.retGOOG), col = "steelblue", lwd = 3)

# Are the daily net returns normally distributed?
# Why?

#We do the visual test with the qqplot to see if the data follows a normal distribution
qqnorm(net.retGOOG)
qqPlot(as.numeric(net.retGOOG), 
       distribution = "norm",  # Reference distribution
       xlab = "Theoretical quantiles", 
       ylab = "Sample quantiles", col.lines = "steelblue"
)


#Perform a normality test to justify your answer

# We perform a shafiro test to justify if the data is normal distributed

shapiro.test(numeric$GOOG) #p_value is smaller than 0.05

#c) Tranform the net retruns to log returns. Compute the sample mean, sd, skewness, excess kurtosis, min, max of each log return series.

#We transform the net returns to log returns: 

log_net.retGOOG = log(1+net.retGOOG)
log_net.retSP = log(1+net.retSP)

log_numeric<-data.frame(
  GOOG= as.numeric(log_net.retGOOG),
  SP= as.numeric(log_net.retSP)
)

log_data <- data.frame(
  mean           = as.numeric(colMeans(log_numeric)),       #Sample mean
  sd             = apply(log_numeric, 2, sd),               #Sample standard deviation
  skewness       = apply(log_numeric, 2, skewness),         #Sample skewness
  excess_kurtosis= apply(log_numeric, 2, kurtosis),         #Sample excess kurtosis
  minimum        = apply(log_numeric, 2, min),              #Sample minimum
  maximum        = apply(log_numeric, 2, max)               #Sample maximum
)
log_data

#d) Test the null hypotesis that the mean of the log returns of Google stock is zero
#We do a t-test to check if the average is 0, Although we do not know if the distribution is normal, 
#we will use a t.test to test the hypothesis, since it is the tool that best approximates what we want to test.

t.test(log_numeric$GOOG, mu=0) 

#We cannot rule out with 5% significance that mu=0, since the p-value is greater than 0.05

#e) Obtain the empirical density plot of the daily log returns of Google stock and the S&P composite index.
#Are daily log returns normally distributed?

#Empirical density plot of the daily log returns of Google stock
plot(density(log_net.retGOOG), main = "Denistat Empírica", lwd = 2)
polygon(density(log_net.retGOOG), col = "lightblue")



#We do the visual test with the qqplot to see if the data follows a normal distribution
qqnorm(log_net.retGOOG)
qqPlot(as.numeric(log_net.retGOOG), 
       distribution = "norm",  # Reference distribution
       xlab = "Theoretical quantiles", 
       ylab = "Sample quantiles", col.lines = "steelblue"
)

# We perform a shafiro test to justify if the data is normal distributed

shapiro.test(log_numeric$GOOG) 

# Since the p-value is less than 0.05, with 5% significance we can reject that the distribution is normal


#Empirical density plot of the daily log returns of S&P composite index.

plot(density(log_net.retSP), main = "Denistat Empírica", lwd = 2)
polygon(density(log_net.retSP), col = "lightblue")



#We do the visual test with the qqplot to see if the data follows a normal distribution
qqnorm(log_net.retSP)
qqPlot(as.numeric(log_net.retSP), 
       distribution = "norm",  # Reference distribution
       xlab = "Theoretical quantiles", 
       ylab = "Sample quantiles", col.lines = "steelblue"
)

# We perform a shafiro test to justify if the data is normal distributed
shapiro.test(log_net.retSP$GOOG) 

#f) Construct a 95% confideence interval for the expected daily log return of Google stock.
# Although the distribution is not normal, we will use a t.test to test the hypothesis, 
#since it is the tool that best approximates what we want to test.
t.test(log_net.retGOOG)







#Exercise 2: 
#EUR240,000 are missing to buy the perfect home. Therefore, you need a securitised loan from a bank that you repay over a period of 30 years. 
#In this specific loan they charge you 1.99% interest on the mortgage.

#a)Compute the monthly installment of the loan
#We apply the appropiate formula:
D = 600000
t = 30
R = 2.99/100
x=D*(R/12)/((1+R/12)^(12*t)-1)
(monthly.installment = (D*R/12*(1+R/12)^(12*t))/((1+R/12)^(12*t)-1))
#We will pay a monthly mortgage of EUR 885.8871.

#b) Construct a table containing the interest paid and the capital amortization portion of the installment
#for all payments (360).

table =data.frame(
  Month=1:(12*30)
)
table$debt.start[1]=D
table$Interest.accrued=D*R/12
table$capital.repayment <- x*(1+R/12)^(table$Month-1)

for (i in 2:length(table$Month)){
  table$debt.start[i]=D-sum(table$capital.repayment[1:i-1])
  table$Interest.accrued[i]=table$debt.start[i]*R/12
}
head(table)
tail(table) #We observe that in effect the debt for the last month is equal to the value of the last installment


#We do the table for each year.

table_year=data.frame(
  Years=1:30
)
for (i in 1:30){
  table_year$debt.start[i]=table$debt.start[(12*i-11)]
  table_year$Interest.accrued[i]=sum(table$Interest.accrued[(12*i-11):(12*i)])
  table_year$capital.repayment[i]= sum(table$capital.repayment[(12*i-11):(12*i)])
}
tabla_latex<-xtable(table_year)
print(tabla_latex, include.rownames=FALSE)
head(table_year)


#(c) How much have you paid back to the bank at the end of the loan?
#Like the monthly price of the mortgage, this calculation is simple
(total.paid=monthly.installment*12*t)
#The total amount paid to the bank will be EUR 318919.3

#We verify that the correct calculation and the total capital paid without interest is EUR240.000
sum(table$capital.repayment)

#That is, in total we have paid the bank EUR 78919.3in interest.
 
#(d)Assume that the interest rate applicable is a market index plus a spread.
# Initially the market index is 1% and the spread (which is kept constant durning the like off the loan) is 0.99%
# (hence the first 12 installments are the same as the previus question)
# After 1 year the market spread rises 0.1% an constinously does so until the last year,
#where the market index is 3.9% and you spread is 0.99% (which makes an effective interest rate for the final year of 4.89%)
# How much have you paid back to the bank at the end of the loan in this new scenario?

D     <- 600000
t     <- 30
R     <-1.99/100
# I have to calculate the "x" from the beginning with the debt I have left because I don't know
#if interest is going to change or when it is going to change.
table_d <- data.frame(
  Month            = 1:(12*t),
  debt.start       = c(0),
  interest.accrued = c(0),
  capital.repayment= c(0),
  rate             =c(0)
)
for (year in 1:t){
  table_d$debt.start[(year-1)*12+1]        <- D                                      # First month of each year I redefine the constants
  table_d$interest.accrued[(year-1)*12+1]  <- D*R/12
  table_d$capital.repayment[(year-1)*12+1] <- (D*R/12)/((1+R/12)^(12*t)-1) 
  table_d$rate[(year-1)*12+1]              <- R                      
  # I calculate the "x" as if the mortgage were starting from 0
  for (month in 2:12){            # The months from the second to the last 
    Month                                        <- 12*(year-1)+month
    table_d$debt.start[Month]        <- table_d$debt.start[Month-1]-table_d$capital.repayment[Month-1] 
    table_d$interest.accrued[Month]  <- table_d$debt.start[Month]*R/12
    table_d$capital.repayment[Month] <- table_d$capital.repayment[Month-1]*(1+R/12)
    table_d$rate[Month]              <- R
    }
  D <- table_d$debt.start[Month]-table_d$capital.repayment[Month] #We define debt.start from first month of next year.
  R <- R+ 0.1/100
  t <- t-1    #is the years left, with this we recalculate the "x" with the new interest rate and the debt we have left as if we were starting the mortgage again
}
head(table_d)
tail(table_d)
tabla_latex<-xtable(table_d)
print(tabla_latex, include.rownames=FALSE)
(total.paid_d=sum(table_d$interest.accrued)+sum(table_d$capital.repayment))

