# CVAR Model
# Replicate results of MacDonald & Ricci (2004) - SAJE
# Kevin Kotze

# clear data & close graphs
rm(list=ls())
graphics.off()

# load package
library(vars)

# set directory and read data
setwd("~/git/tsm/ts-10-tut/tut")
#setwd("C:\\Users\\image")

# load data and create time series object
dat <- read.csv(file="Mac_Ric.csv")

plot(ts(dat$LREERS,start=c(1970,1),freq=4),ylab="LREERS") # logarithm of real effective exchange rate
plot(ts(dat$RIRR,start=c(1970,1),freq=4),ylab="RIRR") # real interest rate relative to trading partners
plot(ts(dat$LRGDPPCR,start=c(1970,1),freq=4),ylab="LRGDPPCR") # logarithm of real GDP per capita relative to trading partners
plot(ts(dat$LPR2COMM5,start=c(1970,1),freq=4),ylab="LPR2COMM5") # real commodity prices
plot(ts(dat$OPENY,start=c(1970,1),freq=4),ylab="OPENY") # openness - ratio to GDP of exports and imports
plot(ts(dat$FBYA,start=c(1970,1),freq=4),ylab="FBYA") # ratio of fiscal balance to GDP
plot(ts(dat$NFAOFPY,start=c(1970,1),freq=4),ylab="NFAOFPY") # ratio to GDP of net foreign assets of the banking system

# ESTIMATE VAR

dat.VAR <- cbind(dat$LREERS,dat$RIRR,dat$LRGDPPCR,dat$LPR2COMM5,dat$OPENY,dat$FBYA,dat$NFAOFPY)
colnames(dat.VAR) <- c("LREERS","RIRR","LRGDPPCR","LPR2COMM5","OPENY","FBYA","NFAOFPY")
dat.EXO <- cbind(dat$SDUMC1,dat$SDUMC2,dat$SDUMC3,dat$DUMRER1,dat$DUMRER2,dat$DUMFBYA,dat$DUMNFAOFPY)
colnames(dat.EXO) <- c("SDUMC1","SDUMC2","SDUMC3","DUMRER1","DUMRER2","DUMFBYA","NFAOFPY")

VAR.est <- VAR(dat.VAR,p=4,type="const",season=NULL,exog=dat.EXO)
summary(VAR.est)

H1.trace <- ca.jo(dat.VAR,ecdet=c("const"),type="trace",K=4,season=NULL,dumvar=dat.EXO)
summary(H1.trace) # r<=1 : 94.47  < 102.14

H1.eigen <- ca.jo(dat.VAR,ecdet=c("const"),type="eigen",K=4,season=NULL,dumvar=dat.EXO)
summary(H1.eigen) # r<=1 : 28.06 < 40.30

beta <- H1.trace@V
alpha <- H1.trace@W

A1 <- t(matrix(c(0,0,0,0,0,0, 1,0,0,0,0,0, 0,1,0,0,0,0,
               0,0,1,0,0,0, 0,0,0,1,0,0, 0,0,0,0,1,0,
               0,0,0,0,0,1), nrow=6, ncol=7))
print(A1) # note the first column is missing from the diagnol matrix

H2 <- (alrtest(z=H1.trace, A=A1, r=1))
summary(H2) # pval = 0.37. Cannot reject the null that the first variable (exchange rate) is weakly exogenous

# The exchange rate does not react to the equilibrium exchange rate???
