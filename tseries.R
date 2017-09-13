# Install Libraries
#install.packages("forecast")
library("forecast")

#setwd("rcourse")
rawdata <- read.table("sales.txt",h=T)
rawdata$Date <- as.Date(rawdata$Date)
head(rawdata)
plot(rawdata)

# Using build in ts object
sts <- ts(rawdata$Sales,start=1986,frequency=12)
plot(sts, col="black",xlim=c(1986,2007),ylim=c(0,220),main="ETS")

# Forecast using ETS method 
fc.ets = forecast(sts)
par(new=TRUE)
plot(fc.ets,col="black",xlim=c(1986,2007),ylim=c(0,220),main="ETS")
#lines(fc.ets$residuals,col="brown")
lines(fc.ets$fitted,col="blue")

# Forecast using ARIMA method
ar = auto.arima(sts)
ar
fc.arima = forecast(ar)
fc.arima
par(new=TRUE)
plot(fc.arima,col="black",xlim=c(1986,2007),ylim=c(0,220),main="ARIMA, ETS")
lines(fc.arima$fitted,col="green")

#Forecast using Holtwinter method
fc.holt = hw(sts)
par(new=TRUE)
plot(fc.holt,col="black",xlim=c(1986,2007),ylim=c(0,220),main="EXP, ARIMA, ETS")
lines(fc.holt$fitted,col="red")

# Errors
accuracy(fc.ets)
accuracy(fc.arima)
accuracy(fc.holt)

# Forecast using ETS method WITH INTERVALS
fc.ets = forecast(sts)
par(new=TRUE)
plot(fc.ets,col="black",xlim=c(1986,2007),ylim=c(0,220),main="ETS")
#lines(fc.ets$residuals,col="brown")
lines(fc.ets$fitted,col="blue")



#> accuracy(fc.ets)
#                     ME     RMSE      MAE       MPE     MAPE      MASE       ACF1
#Training set 0.06811288 2.555712 1.934977 0.0251826 2.286965 0.4622071 0.03040929
#> accuracy(fc.arima)
#                     ME     RMSE      MAE        MPE     MAPE      MASE        ACF1
#Training set 0.08944205 2.553664 1.931783 -0.1149925 2.288831 0.4614443 -0.05481763
#> accuracy(fc.holt)
#                     ME     RMSE      MAE        MPE     MAPE      MASE       ACF1
#Training set 0.03515907 2.906286 2.161155 -0.1649794 2.710975 0.5162341 0.07652641



# Saving to csv the fitted values
write.csv(fc.ets$fitted,"ukclothes_ets_fitted.csv")
write.csv(fc.arima$fitted,"ukclothes_arima_fitted.csv")
write.csv(fc.holt$fitted,"ukclothes_holt_fitted.csv")

# Saving to csv the forecasted values
write.csv(fc.ets$mean,"ukclothes_ets_prev_mean.csv")
write.csv(fc.arima$mean,"ukclothes_arima_prev_mean.csv")
write.csv(fc.holt$mean,"ukclothes_holt_prev_mean.csv")

