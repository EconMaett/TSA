# Multivariate-GARCH Model
# Using SP500 & FTSE data
# Kevin Kotze

# Mean equation: \epsilon_{1,t} = \upsilon_{1,t} \sqrt{h_{11,t}}
# Mean equation: \epsilon_{2,t} = \upsilon_{2,t} \sqrt{h_{22,t}}

# Volatility:    h_{11,t} = c_10 + \alpha_{11}\epsilon_{1,t-??1}^2 + \beta_{11}h_{11,t-??1}
# Volatility:    h_{22,t} = c_20 + \alpha_{22}\epsilon_{2,t-??1}^2 + \beta_{22}h_{11,t-??1}
# Volatility:    h_{12,t} = \alpha_{12}\epsilon_{1,t-??1}\epsilon_{2,t-??1} + \beta_{12}h_{12,t-??1}

## clear data & close graphs
rm(list=ls())
graphics.off()

# set working directory
setwd('~/git/tsm/ts-12-tut/tut')
#setwd("C:\\Users\\image")

# load package
library('ccgarch')

dat <- read.csv(file="sp_ftse.csv")

mv.dat <- cbind(dat$SP500,dat$FTSE)

plot.ts(mv.dat[,1]) # returns of SP500
plot.ts(mv.dat[,2]) # returns of FTSE

# you need starting values for the parameters
c <- c(0.003, 0.001)     # initial values for constants
A <- diag(c(0.2,0.15))   # initial values for ARCH
B <- diag(c(0.75, 0.8))  # initial values for GARCH
dcc.para <- c(0.01,0.98) # initial values for conditional correlation

dcc.results <- dcc.estimation(inia=c, iniA=A, iniB=B, ini.dcc=dcc.para, dvar=mv.dat, model="diagonal")

dcc.results$out # all parameters are positive and standard errors are relatively small

h11 <- sqrt(252*dcc.results$h[,1]) # h11_t series which is standardized
h22 <- sqrt(252*dcc.results$h[,2]) # h22_t series which is standardized
h12.dcc <- (dcc.results$DCC[,2]/(sqrt(dcc.results$h[,1]))*(sqrt(dcc.results$h[,2]))) # cross correlation

plot.ts(cbind(h11,h22,h12.dcc),ylab=c("sp500","ftse","dcc")) # correlation in volatility has increased form obs 1000

plot.ts(dcc.results$std.resid) # check the residuals to ensure that they are white noise