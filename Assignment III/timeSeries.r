data("sunspots")
head(sunspots)
sunspots

library(timeSeries)
library(xts)
library(forecast)

plot(sunspots)

class(sunspots)

cycle(sunspots)

frequency(sunspots)

m <- lm(sunspots ~ time(sunspots))
plot(m)


abline(m)

plot(aggregate(sunspots, FUN = mean))

boxplot(sunspots ~ cycle(sunspots))

d <- stl(sunspots, s.window = 12)

plot(d)

mean(sunspots)
g <- rollmean(sunspots, k = 100)
plot(g)

require(graphics)


library(zoo)
sunspots_zoo <- as.zoo(sunspots)
zoo_75 <- sunspots_zoo[1:2112]
zoo_25 <- sunspots_zoo[2113:length(sunspots)]

df_75 <- as.ts(zoo_75)
df_25 <- as.ts(zoo_25)
df_25

sunspotsPredict <- HoltWinters(df_75, alpha = 0.4856207, beta = 0.001282363, gamma = 0.1589522)
p <- predict(sunspotsPredict, 708, prediction.interval = TRUE)
plot(sunspotsPredict, p)

#p

p_zoo <- as.zoo(p)

p_df <- subset(p, select = c(fit))
#p_df

class(p_df)
plot(sunspotsPredict)


fitted <- matrix(p_df, ncol = 12, byrow = FALSE)
#fitted

matrix_df_25 <- as.matrix(df_25)
#matrix_df_25
actual <- matrix(matrix_df_25, ncol = 12, byrow = FALSE)
#actual

#rms <- actual - fitted
#rms

RMSE = function(fittd, actual){
  sqrt(mean((fitted - actual)^2))
}

RMSE(fiited, actual)

plot(df_25, col = "red")
lines(p_zoo$fit, col = "green")

class(df_75)

library(forecast)

components.ts = decompose(sunspots)
plot(components.ts)

library("fUnitRoots")
urkpssTest(sunspots, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
ts_stationary = diff(sunspots, differences=1)
plot(ts_stationary)

#autoCorrelation : no linear association between observations separated by larger lags
acf(sunspots,lag.max = 34) 

#Remove seasonality from the original series
ts_seasonallyAdjusted <- sunspots- components.ts$seasonal
ts_stationary <- diff(ts_seasonallyAdjusted, differences=1)

plot(ts_seasonallyAdjusted)

acf(ts_stationary, lag.max = 34)
pacf(ts_stationary, lag.max = 34)


#order: non-seasonal part(p, d, q)
#seasonal: seasonal part of ARIMA
#method: fitting model
fitARIMA <- arima(df_75, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
library(lmtest)
coeftest(fitARIMA)

confint(fitARIMA)


acf(fitARIMA$residuals)
library(FitAR)
boxresult<-LjungBoxTest (fitARIMA$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)

auto.arima(df_75, trace=TRUE)

arimaPred <- predict(fitARIMA,n.ahead = 708)
futurVal <- forecast.Arima(fitARIMA,h=10, level=c(99.5))
plot.forecast(futurVal)

#Arima RMSE
diffArima <- (sqrt(mean((df_25 - arimaPred$pred)^2)))
diffArima
