# SVAR Model - Blanchard & Quah Replication
# Kevin Kotze

# clear data & close graphs
rm(list=ls())
graphics.off()

# load package
library(vars)

# set working directory
#getwd()
setwd('~/git/tsm/ts-8-tut')
#setwd("C:\\Users\\image")

dat <-read.csv("BQdata.csv")

# get time series
yt <- 100*log(dat$GNP82)
dyt <- diff(yt)
unt <- dat$USUNRATEE[-1]
nt <- length(dyt)

# demean data
dyt <- dyt - mean(dyt)
unt <- unt - mean(unt)

# set up dat for VAR
dyt <- ts(dyt, start=c(1951,2), frequency=4)
unt <- ts(unt, start=c(1951,2), frequency=4)
vardat0 <- cbind(dyt, unt)

# plot processes
plot.ts(vardat0, main= "")

# estimate VAR(8) as in BQ
model0 <- VAR(vardat0, p=8, type="none")
summary(model0)

# estimate SVAR imposing BQ long-run restrictions
model1 <- BQ(model0)
summary(model1)

# get IRF for dyt and unt
irf.dyt <- irf(model1, impulse="dyt", boot=FALSE, n.ahead=40)
irf.unt <- irf(model1, impulse="unt", boot=FALSE, n.ahead=40)

# get supply shocks (dyt)
# to get shocks to yt we need the cumulative sum
supply <- cbind(cumsum(irf.dyt$irf$dyt[,1]), irf.dyt$irf$dyt[,2])

# get demand shocks (unt)
# to get shocks to yt we need the cumulative sum
# to match BQ graphs multiply by -1
# (negative shock rather than positive)
demand <- cbind(-1*cumsum(irf.unt$irf$unt[,1]), -1*irf.unt$irf$unt[,2])

# plot IRFs - demand shocks
plot.ts(demand[,1], col = "black", lwd = 2, ann = FALSE,
				xlim = c(0,40), ylim = c(-.6,1.4))
lines(demand[,2], col = "blue", lwd = 2)
abline(h = 0)
legend(x = "topright", c("Output response", "Unemployment response"),
       col=c("black","blue"), lwd = 2, bty = "n")

# plot IRFs - supply shocks
plot.ts(supply[,1], col = "black", lwd = 2, ann = FALSE,
				xlim = c(0,40), ylim = c(-.6,2))
lines(supply[,2], col = "blue", lwd = 2)
abline(h = 0)
legend(x = "topright", c("Output response","Unemployment response"),
					col = c("black", "blue"), lwd = 2, bty = "n")

