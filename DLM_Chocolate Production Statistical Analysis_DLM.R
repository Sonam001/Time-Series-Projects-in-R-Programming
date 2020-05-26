# Chocolate Database

require(dlm)

chocolate.ts <- ts(read.table("chocolate_beer_electricity.txt",header = TRUE)[,1],start=1958,freq=12)

par(mar=c(3.1,4.1,1.1,2.1),cex=0.8)
plot(chocolate.ts)

#Using log transformation to account for seasonal pattern

log.chocolate.ts <- log(chocolate.ts)
par(mar=c(3.1,4.1,1.1,2.1),cex=0.8)
plot(log.chocolate.ts)

# The series is non-stationary and has seasonality

# We are going to model this time series using 2nd order polynomial trend and a Fourier representation with all Fourier frequencies
#Define the model and MLE of observational and system variances

model <- function(parm) {
  dlmModPoly(order = 2, dV = exp(parm[1]), dW = c(exp(parm[2]),exp(parm[3]))) + dlmModTrig(s = 12, dV = 0, dW=exp(parm[4]))
}
fit.model <- dlmMLE(log.chocolate.ts, rep(0.1,4), model)
fit.model$convergence
unlist(model(fit.model$par)[c("V","W")])

#Define model using MLE

mod.MLE <- model(fit.model$par)

#Kalman filter
chocolatefilt <- dlmFilter(log.chocolate.ts, mod.MLE)

cov.filt <- with(fit.model, dlmSvd2var(U.C,D.C))

seas.term = 3

sd.seasonality.filt2 <- rep(NA,length(cov.filt))
for(i in 1:length(cov.filt)) sd.seasonality.filt2[i] = sqrt(cov.filt[[i]][seas.term,seas.term])
sd.seasonality.filt2 = ts(sd.seasonality.filt2[-(1:20)],start = c(1959,2),frequency=12)
seasonality.filt2 = ts(co2filt2$m[-(1:20),seas.term],start = c(1959,2),frequency=12)
ll = seasonality.filt2 - 1.96 * sd.seasonality.filt2
ul = seasonality.filt2 + 1.96 * sd.seasonality.filt2
llim = min(ll)
ulim = max(ul)
plot(seasonality.filt2,ylim=c(llim,ulim))
lines(ll,lty=2,col="green")
lines(ul,lty=2,col="green")
llim = min(c(min(seasonality.filt2/sd.seasonality.filt2),-1.96))
ulim = max(c(max(seasonality.filt2/sd.seasonality.filt2),1.96))
plot(seasonality.filt2/sd.seasonality.filt2,ylim=c(llim,ulim))
abline(h=1.96,lty=2)
abline(h=-1.96,lty=2)


#Analysis of the seasonal Fourier frequencies indicates
#that we may need 5 or 6 harmonics. So little or no gain would be obtained from removing model.

#One-step ahead forecast errors
model.res <- residuals(chocolatefilt, sd = FALSE)


#Plot one-step ahead forecast errors
plot(model.res, type='h'); abline(h=0)

#Plot ACF and PACG on one-step ahead forecast errors
acf(model.res, na.action = na.pass)
pacf(model.res, na.action = na.pass)

#Plot qq plot of one-step ahead forecast errors
qqnorm(model.res);qqline(model.res)
# Results show some lags are statistically significant

#Diagnostic Test
#Normality test with Shapiro-Wilk normality test
#Null hypothesis : errors are normally distributed

shapiro.test(model.res)

#P-value is large . So no departure form normality

#Ljung-Cox test to test the autocorrelation
#Null hypothesis : errors are independent

Box.test(model.res, lag=20, type="Ljung")
sapply(1:20,function(i)
  Box.test(model.res, lag = i, type ="Ljung-Box")$p.value)


# Prediction of 36 lags using the best model
h <- 36
forecast <- dlmForecast(chocolatefilt,n = h)

plot(forecast$f)

n <- length(chocolatefilt)
plot(log.chocolate.ts, type="l",ylim=c(5,10),,xlim =c(1955,1995), ylab = "Log time series")
lines(forecast$f, col="red")
lines(forecast$f+1.95*sqrt(unlist(forecast$Q)), col="blue")
lines(forecast$f-1.95*sqrt(unlist(forecast$Q)), col="blue")

