
#import data
Exchange.Rates <- read.csv("~/Desktop/BA/midterm/Q4/Q4 Exchange Rates - Sheet1.csv")

str(Exchange.Rates)
head(Exchange.Rates)
summary(Exchange.Rates)

Exchange.Rates <- subset(Exchange.Rates,select = EUR.to.USD.Rate:TUR.to.USD.Rate)

#Overview of 2 currency
Exchangeseries<-ts(Exchange.Rates,start=c(2013),end=c(2017),frequency=12)
plot.ts(Exchangeseries)
summary(Exchangeseries)

#Decomposing Time Series

#Euro
ts.Euro <- ts(Exchange.Rates$EUR.to.USD.Rate,start=c(2013),end=c(2017),frequency=12)
plot.ts(ts.Euro)
Euro.decom <- decompose(ts.Euro) # decompose ,trend seasonal random
plot(Euro.decom)

#TUR
ts.TUR <- ts(Exchange.Rates$TUR.to.USD.Rate,start=c(2013),end=c(2017),frequency=12)
plot.ts(ts.TUR)
TUR.decom <- decompose(ts.TUR) # decompose ,trend seasonal random
plot(TUR.decom)

#Forecast

#Euro
Euro.holt <- HoltWinters(ts.Euro, gamma=TRUE)
Euro.holt 
plot(Euro.holt)

library(forecast)
Euro.forecasts <- forecast.HoltWinters(Euro.holt, h=10)
plot.forecast(Euro.forecasts)

#TUR
TUR.holt <- HoltWinters(ts.TUR, gamma=TRUE)
TUR.holt 
plot(TUR.holt)

library(forecast)
TUR.forecasts <- forecast.HoltWinters(TUR.holt, h=10)
plot.forecast(TUR.forecasts)



