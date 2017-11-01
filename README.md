# Time-series-analysis-for-North-Ice-Sea
Project to analyze the shrink extent of ice extent of north ice sea from 1979 to 2017
library(xlsx)
library(forecast)
dat=read.xlsx("C:/Dropbox/2017 MS STAT/Stat 641/North combined.xlsx",sheetIndex = 1,heat=T)
head(dat)
dat=dat[,c(1,2,5)]
head(dat)
attach(dat)

myts <- ts(dat[,-1:-2], start=c(1979, 1), end=c(2017, 9), frequency=12) 
plot(myts)
plot(as.ts(myts))


#seasonal decomposition
nottem.stl = stl(myts, s.window="periodic")
plot(nottem.stl)


#decompose my data
library(tseries)
acf(myts)
pacf(myts)
count_d1 = diff(myts, differences = 2)
plot.ts(count_d1)

adf.test(count_d1, alternative = "stationary")


#fit auto.arima
library(forecast)
Model_Arima=auto.arima(myts)
summary(Model_Arima)

tsdisplay(residuals(Model_Arima), lag.max=30)

plot(forecast(Model_Arima,60))

qqnorm(residuals(Model_Arima))
qqline(residuals(Model_Arima))


## fit regression
fit=lm(dat$X.extent~.,data=dat)
summary(fit)
residuals(fit)
qqnorm(residuals(fit))
