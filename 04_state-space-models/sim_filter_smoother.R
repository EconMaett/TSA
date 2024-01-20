# State-space model for simulated random-walk process

rm(list=ls())

set.seed(123456)
num = 50 # simulate 50 observations

xi = rnorm(num+1,0,1)     # error in state equation
epsilon = rnorm(num,0,1)  # error in measurement equation

alpha = cumsum(xi)        # state equation takes form of random walk
yt = alpha[-1] + epsilon  # obs: yt[1] ,..., yt[50]

## Objective now is to estimate values for alpha

# setup filter and smoother
alpha0 = 0    # assume alpha_0 = N(0,1)
sigma0 = 1    # assume alpha_0 = N(0,1)
Gt = 1        # coefficient in state equation
Ft = 1        # coefficient in measurement equation
cW = 1        # state variance-covariance matrix
cV = 1        # measurement variance-covariance matrix

## Kalman filter
W = t(cW)%*%cW  # convert scalar to matrix
V = t(cV)%*%cV  # convert scalar to matrix

Gt = as.matrix(Gt)
pdim = nrow(Gt)
yt = as.matrix(yt)
qdim = ncol(yt)
alphap = array(NA, dim=c(pdim,1,num))     # alpha_p= a_{t+1}
Pp = array(NA, dim=c(pdim,pdim,num))      # Pp=P_{t+1}
alphaf = array(NA, dim=c(pdim,1,num))     # alpha_f=a_t
Pf = array(NA, dim=c(pdim,pdim,num))      # Pf=P_t
innov = array(NA, dim=c(qdim,1,num))      # innovations
sig = array(NA, dim=c(qdim,qdim,num))     # innov var-cov matrix
Kmat = rep(NA, num)                       # store Kalman gain

# initialize (because R can't count from zero)
alpha00 = as.matrix(alpha0, nrow=pdim, ncol=1)  # state: mean starting value
P00 = as.matrix(sigma0, nrow=pdim, ncol=pdim)   # state: variance start value
alphap[,,1] = Gt%*%alpha00                      # predicted value a_{t+1}
Pp[,,1] = Gt%*%P00%*%t(Gt)+W                    # variance for predicted state value
sigtemp = Ft%*%Pp[,,1]%*%t(Ft)+V                # variance for measurement value
sig[,,1] = (t(sigtemp)+sigtemp)/2               # innov var - make sure it's symmetric
siginv = solve(sig[,,1])                        # 1 / innov var
K = Pp[,,1]%*%t(Ft)%*%siginv                    # K_t = predicted variance / innov variance
Kmat[1] = K                                     # store Kalman gain
innov[,,1] = yt[1,]-Ft%*%alphap[,,1]            # epsilon_t = y_t - a_t
alphaf[,,1] = alphap[,,1]+K%*%innov[,,1]        # a_{t+1} = a_t + K_t(epsilon_t)
Pf[,,1] = Pp[,,1]-K%*%Ft%*%Pp[,,1]              # variance of forecast
sigmat = as.matrix(sig[,,1], nrow=qdim, ncol=qdim)   # collect variance of measurement errors
like = log(det(sigmat)) + t(innov[,,1])%*%siginv%*%innov[,,1]   # calculate -log(likelihood)

########## start filter iterations ###################
for (i in 2:num){
  if (num < 2) break
  alphap[,,i] = Gt%*%alphaf[,,i-1]        # predicted value a_{t+2}
  Pp[,,i] = Gt%*%Pf[,,i-1]%*%t(Gt)+W      # variance of predicted state estimate
  sigtemp = Ft%*%Pp[,,i]%*%t(Ft)+V        # variance of measurement error
  sig[,,i] = (t(sigtemp)+sigtemp)/2       # innov var - make sure it's symmetric
  siginv = solve(sig[,,i])
  K = Pp[,,i]%*%t(Ft)%*%siginv            # K_t = predicted variance / innov variance
  Kmat[i] = K                             # store Kalman gain
  innov[,,i] = yt[i,]-Ft%*%alphap[,,i]    # epsilon_t = y_t - a_t
  alphaf[,,i] = alphap[,,i]+K%*%innov[,,i]  # a_{t+1} = a_t + K_t(epsilon_t)
  Pf[,,i] = Pp[,,i]-K%*%Ft%*%Pp[,,i]      # variance of forecast
  sigmat = as.matrix(sig[,,i], nrow=qdim, ncol=qdim)  # collect variance of measurement errors
  like = like + log(det(sigmat)) + t(innov[,,i])%*%siginv%*%innov[,,i]  # calculate -log(likelihood)
}
like=0.5*like

kf=list(alphap=alphap,Pp=Pp,alphaf=alphaf,Pf=Pf,like=like,innov=innov,sig=sig,Kn=K)

## Kalman smoother
pdim = nrow(as.matrix(Gt))
alphas = array(NA, dim=c(pdim,1,num))      # alpha_s = smoothed alpha values
Ps = array(NA, dim=c(pdim,pdim,num))       # Ps=P_t^s
J = array(NA, dim=c(pdim,pdim,num))        # J=J_t
alphas[,,num] = kf$alphaf[,,num]           # starting value for a_T^s
Ps[,,num] = kf$Pf[,,num]                   # starting value for prediction variance

########## start smoother iterations ###################
for(k in num:2)  {
  J[,,k-1] = (kf$Pf[,,k-1]%*%t(Gt))%*%solve(kf$Pp[,,k])
  alphas[,,k-1] = kf$alphaf[,,k-1]+J[,,k-1]%*%(alphas[,,k]-kf$alphap[,,k])
  Ps[,,k-1] = kf$Pf[,,k-1]+J[,,k-1]%*%(Ps[,,k]-kf$Pp[,,k])%*%t(J[,,k-1])
}
# and now for the initial values because R can't count backward to zero
alpha00=alpha0
P00=sigma0
J0=as.matrix((P00%*%t(Gt))%*%solve(kf$Pp[,,1]), nrow=pdim, ncol=pdim)
alpha0n=as.matrix(alpha00+J0%*%(alphas[,,1]-kf$alphap[,,1]), nrow=pdim, ncol=1)
P0n= P00 + J0%*%(Ps[,,k]-kf$Pp[,,k])%*%t(J0)

ks = list(alphas=alphas,Ps=Ps,alpha0n=alpha0n,P0n=P0n,J0=J0,J=J,alphap=kf$alphap,
          Pp=kf$Pp,alphaf=kf$alphaf,Pf=kf$Pf,like=kf$like,Kn=kf$K)

## Figures

par(mfrow=c(3,1),mar=c(2,5,2,1))
Time = 1:num
plot(Time, alpha[-1], main="Prediction", ylim=c(-5,20), xlab="", ylab="")
lines(ks$alphap)
lines(ks$alphap+2*sqrt(ks$Pp), lty="dashed", col="blue")
lines(ks$alphap-2*sqrt(ks$Pp), lty="dashed", col="blue")
plot(Time, alpha[-1], main="Filter", ylim=c(-5,20), xlab="", ylab="")
lines(ks$alphaf)
lines(ks$alphaf+2*sqrt(ks$Pf), lty="dashed", col="blue")
lines(ks$alphaf-2*sqrt(ks$Pf), lty="dashed", col="blue")
plot(Time, alpha[-1], main="Smoother", ylim=c(-5,20), xlab="", ylab="")
lines(ks$alphas)
lines(ks$alphas+2*sqrt(ks$Ps), lty="dashed", col="blue")
lines(ks$alphas-2*sqrt(ks$Ps), lty="dashed", col="blue")

pred.error = ks$alphap[1,1,]-ks$alphaf[1,1,]
par(mfrow=c(2,1),mar=c(2,2,2,1),cex=0.65)
plot(Kmat, main="Kalman Gain", xlab="", ylab="")
plot(pred.error, main="Prediction Errors", xlab="", ylab="",type="l")

# use autocorrelation function
ac(pred.error)
par(mfrow=c(1,1),mar=c(2,5,2,1))
hist(pred.error,main="")

alpha[1]
ks$alpha0n
sqrt(ks$P0n) # initial value info

