#Homework 2
#Sonam Devadiga
#Question 3: co2 Production

# Definig and plotting time series
plot(co2, ylab = "co2")


# Converting to a stationary time series
diff.co2 <- diff(co2, differences = 1)
plot(diff.co2)

# Removing the seasonality
d12.d1.co2 <- diff(diff.co2, lag = 12)
plot(d12.d1.co2)


# ACF and PACF values
par(mfrow=c(1,2))   
acf_co2 <- acf(d12.d1.co2)
pacf_co2 <- pacf(d12.d1.co2)
# 

# Fitting Best Model

n = length(co2.log)
max.p = 2
max.d = 1
max.q = 2
max.P = 2
max.D = 1
max.Q = 2
BIC.array =array(NA,dim=c(max.p+1,max.d+1,max.q+1,max.P+1,max.D+1,max.Q+1))
AIC.array =array(NA,dim=c(max.p+1,max.d+1,max.q+1,max.P+1,max.D+1,max.Q+1))
best.bic <- 1e8
x = co2
for (p in 0:max.p) for(d in 0:max.d) for(q in 0:max.q) 
  for (P in 0:max.P) for(D in 0:max.D) for(Q in 0:max.Q) 
  {
    cat("p=",p,", d=",d,", q=",q,", P=",P,", D=",D,", Q=",Q,"\n")
    
    fit <- arima(x, order = c(p,d,q),  
                 seas = list(order = c(P,D,Q), 
                             frequency(x)),method="CSS")
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
best_co2_model <- arima(co2, order = c(0,1,1), seasonal = list(order=c(1,1,2),period=12),method = "CSS-ML")

number.parameters <- length(best_co2_model$coef) + 1
-2*best_co2_model$loglik + log(length(co2))*number.parameters


# Prediction of 12 lags using the best model
h <- 12
forecast <- predict(best_co2_model,n.ahead = h)

n <- length(co2)
plot(c(co2,rep(NA,h)),type="l",ylim=c(300,400),ylab = "Log time series")
lines((n+1):(n+h),forecast$pred,col="blue")  
lines((n+1):(n+h),forecast$pred+1.96*forecast$se,lty=2,col="red") # Confidence Interval
lines((n+1):(n+h),forecast$pred-1.96*forecast$se,lty=2,col="red")




#######################################################################################################################

