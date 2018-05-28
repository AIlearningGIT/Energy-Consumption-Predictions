setwd("C:/Users/lavan/Desktop/DM")
d<-read.csv("final data.csv")
d1<-read.csv("final data_lag1.csv")
d2<-read.csv("final data_lag2.csv")
View(d1)
plot(d$T,d$Energy)
m1<-lm(d$Energy~d$T) # Trend Analysis
summary(m1)

m2<-lm(d$Energy~d$T+as.factor(d$D)) # Trend and seasonality
summary(m2)

m3<-lm(d1$Energy~d1$Energy_lag1+d1$T+as.factor(d1$D))  #Trend&seasonality with 1lag
summary(m3)

m4<-lm(d2$Energy~d2$Energy_lag2+d2$T+as.factor(d2$D)) #Trend&seasonality with 2lags
summary(m4)

m5<-lm(log(d1$Energy)~d1$Energy_lag1+d1$T+as.factor(d1$D)) #Trend&seasonality with 1lag and transformation of DV
summary(m5)

m6<-lm(log(d1$Energy)~log(d1$Energy_lag1)+d1$T+as.factor(d1$D)) #Trend&seasonality with 1lag and transformation of DV & IV
summary(m6)

#Cross validation of m6 by splitting the data into train and tested on 12 days of data

lmtrain<-read.csv("final data_lag1_train.csv")
m6train<-lm(log(Energy)~ log(Energy_lag1)+T+as.factor(D), data=lmtrain)
AIC(m6train)
lmtest<-read.csv("final data_lag1_test.csv")
lmpredicted<-predict(m6train,lmtest)
lmpredicted<-round(exp(lmpredicted))
lmtest$Energy-lmpredicted
n1<-length(lmtest$Energy-lmpredicted)
n1
RMSElm <- sqrt(sum((lmtest$Energy-lmpredicted)^2)/n1)
RMSElm

plot(lmtest$T,lmtest$Energy, type="l")
lines(lmtest$T,lmpredicted, col="blue")


#ARIMA Cross-validation using training data and test data
tr<-read.csv("final data_arima_train.csv")
train <- ts(tr$Energy, frequency=8)
test  <- read.csv("final data_arima_test.csv")
ARIMAmodel <- arima(log(train), c(6,0,1), seasonal=list(order=c(6,0,1), period=8))
ARIMAmodel
forecasted <- predict(ARIMAmodel, n.ahead=12*8)
forecasted <- round(exp(forecasted$pred),0)
forecasted
test$Energy
test$Energy - forecasted

n <- length(test$Energy - forecasted)
n
RMSE <- sqrt(sum((test$Energy - forecasted)^2)/n)
RMSE

plot(test$T,test$Energy, type="l")
lines(test$T,forecasted, col="blue")

####
acf(log(d1$Energy))
acf(diff(log(d1$Energy)))
acf(diff(diff(diff(diff(diff(diff(diff(diff(log(d1$Energy))))))))))
pacf(diff(log(d1$Energy)))


