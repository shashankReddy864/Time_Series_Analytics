
#Read Data
dfSA <- read.csv('AustralianWines.csv')
str(dfSA)
head(dfSA)
library('forecast')

# start: the time of the first observation
# frequency: number of times per year
xSA <- ts(dfSA$Red, start=c(1980,1), frequency=12)
xSA
plot(xSA)

# Model 1: Linear Trend Model
wineSA.lm <- tslm(xSA~trend)
summary(wineSA.lm)


# Data partition for time series data
# Use the last 24 months data as the training dataset

nValid <- 24
nTrain <- length(xSA)-nValid
length(xSA)


dfSA.train.ts <- window(xSA, start=c(1980,1), end=c(1980,nTrain))
dfSA.valid.ts <- window(xSA, start=c(1980,nTrain+1), end=c(1980, nTrain+nValid))

train.lmSA <-tslm(dfSA.train.ts ~ trend)
summary(train.lmSA)
train.lmSA.pred <- forecast(train.lmSA, h=nValid,level=0)
accuracy(train.lmSA.pred,dfSA.valid.ts)

# Visualize the linear trend model
par(mfrow = c(1, 1))
plot(train.lmSA.pred, ylim = c(400, 3500),  ylab = "Red", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1980,1996),main = "", flty = 2)
axis(1, at = seq(1980, 1996, 1), labels = format(seq(1980, 1996, 1)))
lines(train.lmSA.pred$fitted, lwd = 2, col = "blue")
lines(dfSA.valid.ts)



train.lmSA.poly.trend <- tslm(dfSA.train.ts~trend + I(trend^2))
summary(train.lmSA.poly.trend)
train.lmSA.poly.trend.pred <- forecast(train.lmSA.poly.trend, h=nValid, level=0)
# Evaluate model performance
accuracy(train.lmSA.poly.trend.pred,dfSA.valid.ts)




# A model with seasonality
# In R, function tslm() uses ts() which automatically creates the categorical Season column (called season)
# and converts it into dummy variables.

train.lmSA.season <- tslm(dfSA.train.ts ~ season)
summary(train.lmSA.season)
train.lmSA.season.pred <- forecast(train.lmSA.season,h=nValid,level=0)
accuracy(train.lmSA.season.pred,dfSA.valid.ts)



# A model with trend and seasonality

train.lmSA.trend.season <- tslm(dfSA.train.ts~trend +I(trend^2)+season)
summary(train.lmSA.trend.season)
train.lmSA.trend.season.pred <- forecast(train.lmSA.trend.season,h=nValid,level=0)
accuracy(train.lmSA.trend.season.pred,dfSA.valid.ts)



library(zoo)

xSA
maSA <-rollmean(xSA,k=12, align='right')
summary(maSA)
maSA


MAPE= mean(abs(maSA-xSA)/xSA,na.rm=T)
MAPE


# run simple exponential smoothing
# and alpha = 0.2 to fit simple exponential smoothing.
ses.SA <- ses(dfSA.train.ts, alpha=0.2, h=24)
summary(ses.SA)
autoplot(ses.SA)
accuracy(ses.SA,dfSA.valid.ts)


ses.SA1 <- ses(dfSA.train.ts, alpha=NULL, h=24)
summary(ses.SA1)
autoplot(ses.SA1)
accuracy(ses.SA1,dfSA.valid.ts)
