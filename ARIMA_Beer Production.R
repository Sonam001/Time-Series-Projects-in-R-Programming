#Homework 2
#Sonam Devadiga
#Question 2: beer Production

beer <- read.table("beer_beer_electricity.txt",header = TRUE)

# Definig and plotting time series
beer.ts <- ts(beer[,2],start =1,frequency = 12)
plot(beer.ts, ylab = "Beer")

# Log of time series transformation
log.beer.ts <- log(beer.ts)
plot(log.beer.ts, ylab = "log beer production")


# Converting to a stationary time series
diff.log.beer.ts <- diff(log.beer.ts, differences = 1)
plot(diff.log.beer.ts)

# Removing the seasonality
d12.d1.log.beer.ts <- diff(diff.log.beer.ts, lag = 12)
plot(d12.d1.log.beer.ts)


# ACF and PACF values
par(mfrow=c(1,2))   
acf_beer <- acf(d12.d1.log.beer.ts)
pacf_beer <- pacf(d12.d1.log.beer.ts)
# 

# Fitting Best Model

n = length(beer.ts.log)
max.p = 2
max.d = 1
max.q = 2
max.P = 2
max.D = 1
max.Q = 2
BIC.array =array(NA,dim=c(max.p+1,max.d+1,max.q+1,max.P+1,max.D+1,max.Q+1))
AIC.array =array(NA,dim=c(max.p+1,max.d+1,max.q+1,max.P+1,max.D+1,max.Q+1))
best.bic <- 1e8
x.ts = log.beer.ts
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
best_beer_model <- arima(log.beer.ts, order = c(1,1,2), seasonal = list(order=c(1,0,2),period=12),method = "CSS-ML")

number.parameters <- length(best_beer_model$coef) + 1
-2*best_beer_model$loglik + log(length(log.beer.ts))*number.parameters


# Prediction of 12 lags using the best model
h <- 12
forecast <- predict(best_beer_model,n.ahead = h)

n <- length(log.beer.ts)
plot(c(log.beer.ts,rep(NA,h)),type="l",ylim=c(2,6),ylab = "Log time series")
lines((n+1):(n+h),forecast$pred,col="blue")  
lines((n+1):(n+h),forecast$pred+1.96*forecast$se,lty=2,col="red") # Confidence Interval
lines((n+1):(n+h),forecast$pred-1.96*forecast$se,lty=2,col="red")


# Plotting the actual forecast
par(mfrow=c(1,1))
n <- length(beer.ts)
plot(c(beer.ts,rep(NA,h)),type = 'l',ylab = "beer production", ylim=c(0,300))
lines((n+1):(n+h),exp(forecast$pred),col="blue")
lines((n+1):(n+h),exp(forecast$pred+1.96*forecast$se),lty=2,col="red") # Confidence Interval
lines((n+1):(n+h),exp(forecast$pred-1.96*forecast$se),lty=2,col="red")




#######################################################################################################################

