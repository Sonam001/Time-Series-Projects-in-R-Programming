#Homework 2
#Sonam Devadiga
#Question 1: Chocolate Production

chocolate <- read.table("chocolate_beer_electricity.txt",header = TRUE)

# Definig and plotting time series
chocolate.ts <- ts(chocolate[,1],start =1,frequency = 12)
plot(chocolate.ts, ylab = "Chocolate")

# Log of time series transformation
log.chocolate.ts <- log(chocolate.ts)
plot(log.chocolate.ts, ylab = "log Chocolate production")


# Converting to a stationary time series
diff.log.chocolate.ts <- diff(log.chocolate.ts, differences = 1)
plot(diff.log.chocolate.ts)

# Removing the seasonality
d12.d1.log.chocolate.ts <- diff(diff.log.chocolate.ts, lag = 12)
plot(d12.d1.log.chocolate.ts)


# ACF and PACF values
par(mfrow=c(1,2))   
acf_chocolate <- acf(d12.d1.log.chocolate.ts)
pacf_chocolate <- pacf(d12.d1.log.chocolate.ts)
# 

# Fitting Best Model

n = length(chocolate.ts.log)
max.p = 2
max.d = 1
max.q = 2
max.P = 2
max.D = 1
max.Q = 2
BIC.array =array(NA,dim=c(max.p+1,max.d+1,max.q+1,max.P+1,max.D+1,max.Q+1))
AIC.array =array(NA,dim=c(max.p+1,max.d+1,max.q+1,max.P+1,max.D+1,max.Q+1))
best.bic <- 1e8
x.ts = log.chocolate.ts
for (p in 0:max.p) for(d in 0:max.d) for(q in 0:max.q) 
  for (P in 0:max.P) for(D in 0:max.D) for(Q in 0:max.Q) 
  {
    cat("p=",p,", d=",d,", q=",q,", P=",P,", D=",D,", Q=",Q,"\n")
    
    fit <- arima(x.ts, order = c(p,d,q),  
                 seas = list(order = c(P,D,Q), 
                             frequency(x.ts)),method="CSS")
    number.parameters <- length(fit$coef) + 1
    BIC.array[p+1,d+1,q+1,P+1,D+1,Q+1] = -2*fit$loglik + log(n)*number.parameters
    AIC.array[p+1,d+1,q+1,P+1,D+1,Q+1] = -2*fit$loglik + 2*number.parameters
    
    if (BIC.array[p+1,d+1,q+1,P+1,D+1,Q+1] < best.bic) 
    {
      best.bic <- BIC.array[p+1,d+1,q+1,P+1,D+1,Q+1]
      best.fit <- fit
      best.model <- c(p,d,q,P,D,Q) 
    }
    
  }

best.bic
best.fit
best.model

# Best BIC model is [111102] with BIC of -662.7087
best_chocolate_model <- arima(log.chocolate.ts, order = c(1,1,1), seasonal = list(order=c(1,0,2),period=12),method = "CSS-ML")

number.parameters <- length(best_chocolate_model$coef) + 1
-2*best_chocolate_model$loglik + log(length(log.chocolate.ts))*number.parameters


# Prediction of 36 lags using the best model
h <- 36
forecast <- predict(best_chocolate_model,n.ahead = h)

n <- length(log.chocolate.ts)
plot(c(log.chocolate.ts,rep(NA,h)),type="l",ylim=c(6.5,9.5),ylab = "Log time series")
lines((n+1):(n+h),forecast$pred,col="blue")  
lines((n+1):(n+h),forecast$pred+1.96*forecast$se,lty=2,col="red") # Confidence Interval
lines((n+1):(n+h),forecast$pred-1.96*forecast$se,lty=2,col="red")


# Plotting the actual forecast
par(mfrow=c(1,1))
n <- length(chocolate.ts)
plot(c(chocolate.ts,rep(NA,h)),type = 'l',ylab = "Chocolate production", ylim=c(1000,14000))
lines((n+1):(n+h),exp(forecast$pred),col="blue")
lines((n+1):(n+h),exp(forecast$pred+1.96*forecast$se),lty=2,col="red") # Confidence Interval
lines((n+1):(n+h),exp(forecast$pred-1.96*forecast$se),lty=2,col="red")




#######################################################################################################################





# Q2 BEER PRODUCTION


# Definig and plotting time series
par(mfrow=c(1,1))
beer.ts <- ts(production$beer,start =1958,frequency = 12)
plot(beer.ts, ylab = "Beer production")

# Log of time series transformation
beer.ts.log <- log(beer.ts)
plot(beer.ts.log, ylab = "log Beer production")


# Converting to a stationary time series
beer.ts.log.diff <- diff(beer.ts.log, differences = 1)
plot(beer.ts.log.diff)

# Removing the seasonality
beer.ts.log.diff.s <- diff(beer.ts.log.diff, lag = 12)
plot(beer.ts.log.diff.s)


# ACF and PACF values
par(mfrow=c(1,2))   # par() for creating matrix of plots. mfrow defines rows and columns
acf_beer <- acf(beer.ts.log.diff.s)
pacf_beer <- pacf(beer.ts.log.diff.s)


# Fitting Seasonal ARIMA

n = length(beer.ts.log)
max.p = 3
max.d = 1
max.q = 3
max.P = 3
max.D = 1
max.Q = 3
BIC.array =array(NA,dim=c(max.p+1,max.d+1,max.q+1,max.P+1,max.D+1,max.Q+1))
AIC.array =array(NA,dim=c(max.p+1,max.d+1,max.q+1,max.P+1,max.D+1,max.Q+1))
best.bic <- 1e8
x.ts = beer.ts.log
for (p in 0:max.p) for(d in 0:max.d) for(q in 0:max.q) 
  for (P in 0:max.P) for(D in 0:max.D) for(Q in 0:max.Q) 
  {
    cat("p=",p,", d=",d,", q=",q,", P=",P,", D=",D,", Q=",Q,"\n")
    
    fit <- arima(x.ts, order = c(p,d,q),  
                 seas = list(order = c(P,D,Q), 
                             frequency(x.ts)),method="CSS")
    number.parameters <- length(fit$coef) + 1
    BIC.array[p+1,d+1,q+1,P+1,D+1,Q+1] = -2*fit$loglik + log(n)*number.parameters
    AIC.array[p+1,d+1,q+1,P+1,D+1,Q+1] = -2*fit$loglik + 2*number.parameters
    
    if (BIC.array[p+1,d+1,q+1,P+1,D+1,Q+1] < best.bic) 
    {
      best.bic <- BIC.array[p+1,d+1,q+1,P+1,D+1,Q+1]
      best.fit <- fit
      best.model <- c(p,d,q,P,D,Q) 
    }
    
  }

best.bic
best.fit
best.model

# Best BIC model
beer_sarima_212202 <- arima(beer.ts.log, order = c(2,1,2), seasonal = list(order=c(2,0,2),period=12),method = "CSS")

number.parameters <- length(beer_sarima_212202$coef) + 1
-2*beer_sarima_212202$loglik + log(length(beer.ts.log))*number.parameters







# Prediction of 36 lags using the best model
h <- 12
forecast <- predict(beer_sarima_212202,n.ahead = h)
exp(forecast$pred)

# Plotting the forecast
par(mfrow(1,1))
n <- length(beer.ts.log)
plot(c(beer.ts.log,rep(NA,h)),type="l",ylim=c(4,5.5),ylab = "Log time series", xlab = "Index")
lines((n+1):(n+h),forecast$pred,col="blue")  
lines((n+1):(n+h),forecast$pred+1.96*forecast$se,lty=2,col="red") # Confidence Interval
lines((n+1):(n+h),forecast$pred-1.96*forecast$se,lty=2,col="red")



# Plotting the actual forecast
par(mfrow=c(1,1))
n <- length(beer.ts)
plot(c(beer.ts,rep(NA,h)),type = 'l',ylab = "Beer production", ylim=c(1,250))
lines((n+1):(n+h),exp(forecast$pred),col="blue")
lines((n+1):(n+h),exp(forecast$pred+1.96*forecast$se),lty=2,col="red") # Confidence Interval
lines((n+1):(n+h),exp(forecast$pred-1.96*forecast$se),lty=2,col="red")





#######################################################################################################################



# Q3 CO2 production from 1959 to 1997


# Definig and plotting time series

co2.ts <- ts(co2,start =1959,frequency = 12)
plot(co2.ts, ylab = "co2 production")


# Converting to a stationary time series
co2.ts.diff <- diff(co2.ts, differences = 1)
plot(co2.ts.diff)

# Removing the seasonality
co2.ts.diff.s <- diff(co2.ts.diff, lag = 12)
plot(co2.ts.diff.s)


# ACF and PACF values
par(mfrow=c(1,2))   # par() for creating matrix of plots. mfrow defines rows and columns
acf_co2 <- acf(co2.ts.diff.s)
pacf_co2 <- pacf(co2.ts.diff.s)


# Fitting Seasonal ARIMA

n = length(co2.ts)
max.p = 3
max.d = 1
max.q = 3
max.P = 3
max.D = 1
max.Q = 3
BIC.array =array(NA,dim=c(max.p+1,max.d+1,max.q+1,max.P+1,max.D+1,max.Q+1))
AIC.array =array(NA,dim=c(max.p+1,max.d+1,max.q+1,max.P+1,max.D+1,max.Q+1))
best.bic <- 1e8
x.ts = co2.ts
for (p in 0:max.p) for(d in 0:max.d) for(q in 0:max.q) 
  for (P in 0:max.P) for(D in 0:max.D) for(Q in 0:max.Q) 
  {
    cat("p=",p,", d=",d,", q=",q,", P=",P,", D=",D,", Q=",Q,"\n")
    
    fit <- arima(x.ts, order = c(p,d,q),  
                 seas = list(order = c(P,D,Q), 
                             frequency(x.ts)),method="CSS")
    number.parameters <- length(fit$coef) + 1
    BIC.array[p+1,d+1,q+1,P+1,D+1,Q+1] = -2*fit$loglik + log(n)*number.parameters
    AIC.array[p+1,d+1,q+1,P+1,D+1,Q+1] = -2*fit$loglik + 2*number.parameters
    
    if (BIC.array[p+1,d+1,q+1,P+1,D+1,Q+1] < best.bic) 
    {
      best.bic <- BIC.array[p+1,d+1,q+1,P+1,D+1,Q+1]
      best.fit <- fit
      best.model <- c(p,d,q,P,D,Q) 
    }
    
  }

best.bic
best.fit
best.model

# Best BIC model
co2_sarima_011202 <- arima(co2.ts, order = c(0,1,1), seasonal = list(order=c(2,0,2),period=12),method = "CSS")

number.parameters <- length(co2_sarima_011202$coef) + 1
-2*co2_sarima_011202$loglik + log(length(co2.ts))*number.parameters






# Prediction of 36 lags using the best model
h <- 12
forecast <- predict(co2_sarima_011202,n.ahead = h)
forecast





# Plotting the forecast
n <- length(co2.ts)
plot(c(co2.ts,rep(NA,h)),type="l",ylim=c(300,380),ylab = "co2 time series")
lines((n+1):(n+h),forecast$pred,col="blue")  
lines((n+1):(n+h),forecast$pred+1.96*forecast$se,lty=2,col="red") # Confidence Interval
lines((n+1):(n+h),forecast$pred-1.96*forecast$se,lty=2,col="red")



################################################################################################################################

