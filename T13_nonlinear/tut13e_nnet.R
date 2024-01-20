# ANN Model
# US Oil Prices - compare the forecasts of AR(13) to NNET(13)
# Kevin Kotze

# clear data & close graphs ----
rm(list=ls())
graphics.off()

# load libraries ----
library(NTS)
library(nnet)

# Set directory and read data ----
setwd('~/git/tsm/ts-13-tut')
#setwd("C:\\Users\\image")

# Load data ----
dat <- read.table('w-coilwti.txt', header=T)
yt <- diff(dat[,4])

# Plot data ----
plot.ts(yt)  # US weekly crude oil prices from January 3, 1986 to August 28, 2015
abline(v = length(yt)-200, col = "red")  # last 200 observations are used for forecasting 

y <- yt[1:(length(yt)-200)]  # training sample
yhat <- yt[(length(yt)-199):length(yt)]  # test sample

# Table to summarise results ----
result <- data.frame(matrix(rep(0, 4), ncol =2))
colnames(result) <- c("RMSE","MAE")
rownames(result) <- c("m1","m2")

# model 1: AR(13) ----
m1 <- arima(y, order = c(13, 0, 0), include.mean = FALSE)
predY1 <- predict(m1, 200)
er1 <- yhat - predY1$pred
result[1,1] <- sqrt(mean(er1^2))
result[1,2] <- mean(abs(er1))

# model 2: Neural Net with 13 lags ----
m_setup <- NNsetting(yt, nfore=200, lags=c(1:13))
names (m_setup)
X <- m_setup$X
y <- m_setup$y
predX <- m_setup$predX
predY <- m_setup$predY

m2 <- nnet(X, y, size=1, skip=T, linout=T, maxit=5000)
pm2 <- predict(m2, newdata=predX)
er2 <- yhat - pm2
result[2,1] <- sqrt(mean(er2^2))
result[2,2] <- mean(abs(er2))
