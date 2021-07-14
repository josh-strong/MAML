# Loading
library("readxl")
library(lubridate)
library(dplyr)
library(forecast)
library(ggplot2)
library(rdatamarket)
# xls files
data <- read_excel(file.choose())

y <- forecast::msts(data$Wales, seasonal.periods=c(7,365.25))
fit <- tbats(y)
fc <- forecast(fit)
plot(fc)


msts_cons<-data$Wales %>% forecast::msts( seasonal.periods = c(7, 365.25))
msts_cons %>% forecast::mstl() %>% autoplot()    

msts_cons %>% stlf() %>% autoplot()


bestfit <- list(aicc=Inf)
for(K in seq(25)) {
  fit <- auto.arima(msts_cons, xreg=fourier(msts_cons, K=K),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}


msts_cons
fourier(msts_cons, )


y1 <- msts(data$Wales,seasonal.periods=c(365.25, 7))
z1 <- fourier(y1, K=c(2,2))


bestfit <- list(aicc=Inf)
for(K in seq(1)) {
  fit <- auto.arima(y1, xreg=fourier(y1, K=c(2,K)),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}

fc <- forecast(bestfit,
               xreg=fourier(y1, K=c(2,bestK), h=20))
autoplot(fc)

bestK
