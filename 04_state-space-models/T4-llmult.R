## SS multivariate model

## clear data & close graphs
rm(list=ls())
graphics.off()

## Install packages
# install.packages('devtools', repos='https://cran.rstudio.com/', dependencies=TRUE)
# install.packages('dlm', repos='https://cran.rstudio.com/', dependencies=TRUE)
# library(devtools)
# devtools::install_github("KevinKotze/tsm")

## We can now make use of the library
library('tsm')
library('dlm')

## change directory
setwd("C:\\University\\Time Series Analysis\\4_StateSpace\\tut")
#setwd("C:\\Users\\image")

## set datapath and get data

## set datapath and get data
dat_tmp <- read.csv(file="tut/US_dat.csv")
dat <- ts(dat_tmp, start=c(1951,1), frequency=4)
#plot.ts(dat)


#####################################################################################################################
# Model 1 with covariance terms
#####################################################################################################################

build1 <- function(par) {
    m <- dlmModPoly(2)
    m$FF <- m$FF %x% diag(2)
    m$GG <- m$GG %x% diag(2)
    W1 <- diag(1e-7, nr = 2)
    W2 <- diag(exp(par[1 : 2]))
    W2[1,2] <- W2[2,1] <- tanh(par[3]) *
        prod(exp(0.5 * par[1 : 2]))
    m$W <- bdiag(W1, W2)
    V <- diag(exp(par[4 : 5]))
    V[1,2] <- V[2,1] <- tanh(par[6]) *
        prod(exp(0.5 * par[4 : 5]))
    m$V <- V
    m$m0 <- rep(0,4)
    m$C0 <- diag(4) * 1e7
    return(m)
}

mle <- vector("list", 2)
init <- cbind(as.matrix(expand.grid(rep(list(c(-3, 3)), 4))),
              matrix(0, nc = 2, nr = 16))
init <- init[,c(1, 2, 5, 3, 4, 6)]; dimnames(init) <- NULL
for (i in 1:length(mle)) {
    mle[[i]] <- try(dlmMLE(dat, parm = init[i,], build = build1,
                           control = list(trace = 3, REPORT = 2)))
}

good <- seq(along=mle)[sapply(mle, function(x) length(x) > 1 && x$conv == 0)]
mleX <- sapply(mle[good], function(x) unlist(x[1 : 2]))
mleX <- mleX[, order(mleX["value", ])]

fit1 <- build1(mleX[-NROW(mleX), 1])
loglik1 <- dlmLL(dat, fit1)
aic1 <- (2*(loglik1))+2*(sum(8)) #dlmLL caculates the neg. LL

#####################################################################################################################
# Model 2 without covariance terms
#####################################################################################################################

build2 <- function(par) {
    m <- dlmModPoly(2)
    m$FF <- m$FF %x% diag(2)
    m$GG <- m$GG %x% diag(2)
    W1 <- diag(1e-7, nr = 2)
    W2 <- diag(exp(par[1 : 2]))
    #W2[1,2] <- W2[2,1] <- tanh(par[3]) *
    #    prod(exp(0.5 * par[1 : 2]))
    m$W <- bdiag(W1, W2)
    V <- diag(exp(par[3 : 4]))
    V[1,2] <- V[2,1] <- tanh(par[5]) *
        prod(exp(0.5 * par[3 : 4]))
    m$V <- V
    m$m0 <- rep(0,4)
    m$C0 <- diag(4) * 1e7
    return(m)
}

mle <- vector("list", 2)
init <- cbind(as.matrix(expand.grid(rep(list(c(-3, 3)), 4))),
              matrix(0, nc = 2, nr = 16))
init <- init[,c(1, 2, 4, 3, 5)]; dimnames(init) <- NULL
for (i in 1:length(mle)) {
    mle[[i]] <- try(dlmMLE(dat, parm = init[i,], build = build2,
                           control = list(trace = 3, REPORT = 2)))
}

good <- seq(along=mle)[sapply(mle, function(x) length(x) > 1 && x$conv == 0)]
mleX <- sapply(mle[good], function(x) unlist(x[1 : 2]))
mleX <- mleX[, order(mleX["value", ])]

fit2 <- build2(mleX[-NROW(mleX), 1])
loglik2 <- dlmLL(dat, fit2)
aic2 <- (2*(loglik2))+2*(sum(6)) #dlmLL caculates the neg. LL

cat("Model 1 - AIC: ", aic1)
cat("Model 2 - AIC: ", aic2)

## Run Smoother for first model
modelSmooth <- dlmSmooth(dat, mod = fit1)
Smooth.level.1 <- dropFirst(modelSmooth$s[,1])
Smooth.level.2 <- dropFirst(modelSmooth$s[,2])
Smooth.level.3 <- dropFirst(modelSmooth$s[,3])
Smooth.level.4 <- dropFirst(modelSmooth$s[,4])

summary(modelSmooth$s)

## Draw graph
fig.1 <- ts.union(window(dat[,1]),
                  window(Smooth.level.1))

fig.2 <- ts.union(window(dat[,2]),
                  window(Smooth.level.2))

par(mfrow=c(2,1),mar=c(2.2,2.2,1,1),cex=0.8)
plot(fig.1, plot.type = "single", col = c("blue", "green"), ylab="")
legend("topright", legend = c("Inflation","Smoothed"), lty = 1, col = c("blue", "green"), bty = 'n')
plot(fig.2, plot.type = "single", col = c("blue", "green"), ylab="")
legend("bottomright", legend = c("Ouput","Smoothed"), lty = 1, col = c("blue", "green"), bty = 'n')

