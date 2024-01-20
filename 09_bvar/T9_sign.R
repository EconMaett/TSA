# VAR Models - Sign restrictions
# Kevin Kotze

# Housekeeping -------
rm(list = ls())
graphics.off()
set.seed(12345)

library(VARsignR)
data(uhligdata)

# Uhlig Rejection -----
constr <- c(+4,-3,-2,-5)

model1 <- uhlig.reject(Y=uhligdata, nlags=12, draws=200, subdraws=200, nkeep=1000, KMIN=1,
                       KMAX=6, constrained=constr, constant=FALSE, steps=60)

summary(model1)

nam <- c("GDP","GDP Deflator","Comm Price Index","Fed Funds Rate",
        "NB Reserves", "Total Reserves")

irfs1 <- model1$IRFS
irfplot(irfdraws=irfs1, type="median", labels=nam, save=FALSE, bands=c(0.16, 0.84),
        grid=TRUE, bw=FALSE)

fevd1 <- model1$FEVDS
fevdplot(fevd1, label=nam, save=FALSE, bands=c(0.16, 0.84), grid=TRUE,
         bw=FALSE, table=FALSE, periods=NULL)

fevd.table <- fevdplot(fevd1, table=TRUE, label=nam, periods=c(1,10,20,30,40,50,60))
print(fevd.table)

shocks <- model1$SHOCKS
ss <- ts(t(apply(shocks,2,quantile,probs=c(0.5, 0.16, 0.84))), frequency=12, start=c(1960,1))

plot(ss[,1], type="l", col="blue", ylab="Interest rate shock", ylim=c(min(ss), max(ss)))
abline(h=0, col="black")
lines(ss[,2], col="red")
lines(ss[,3], col="red")

# Rubio rejection ------

model2 <- rwz.reject(Y=uhligdata, nlags=12, draws=200, subdraws=200, nkeep=1000,
                    KMIN=1, KMAX=6, constrained=constr, constant=FALSE, steps=60)

irfs2 <- model2$IRFS
irfplot(irfdraws=irfs2, type="median", labels=nam, save=FALSE, bands=c(0.16, 0.84),
        grid=TRUE, bw=FALSE)

# Uhlig penalty -------

model3 <- uhlig.penalty(Y=uhligdata, nlags=12, draws=2000, subdraws=1000,
                        nkeep=1000, KMIN=1, KMAX=6, constrained=constr,
                        constant=FALSE, steps=60, penalty=100, crit=0.001)

irfs3 <- model3$IRFS
irfplot(irfdraws=irfs3, type="median", labels=nam, save=FALSE, bands=c(0.16, 0.84),
        grid=TRUE, bw=FALSE)

# Alternative restrictions --------
constr5 <- c(+4,-3,-2,-5,-6)

model5c <- uhlig.reject(Y=uhligdata,  nlags=12, draws=200, subdraws=200, nkeep=1000,
                        KMIN=1, KMAX=6, constrained=constr5, constant=FALSE, steps=60)

irf5c <- model5c$IRFS
irfplot(irf5c, labels=nam)

# Comparison ------
fp.target(Y=uhligdata, irfdraws=irfs1,  nlags=12,  constant=F, labels=nam, target=TRUE,
          type="median", bands=c(0.16, 0.84), save=FALSE,  grid=TRUE, bw=FALSE,
          legend=TRUE, maxit=1000)




# Appendix ------
model0 <- rfbvar(Y=uhligdata, nlags=12, draws=1000, constant=FALSE,
                steps=60, shock=4)

irfs0 <- model0$IRFS

irfplot(irfdraws=irfs0, type="median", labels=nam, save=FALSE, bands=c(0.16, 0.84),
        grid=TRUE, bw=FALSE)
