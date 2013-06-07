# 5.1-------------
set.seed(123456)
e <- rnorm(500)
# pure random walk
rw.nd <- cumsum(e)
# trend
trd <- 1:500
# random walk with drift
rw.wd <- 0.5*trd + cumsum(e)
# deterministic trend and noise
dt <- e + 0.5*trd
# plotting
par(mar=rep(5,4))
plot.ts(dt, lty=1, ylab='', xlab='')
lines(rw.wd, lty=2)
par(new=T)
plot.ts(rw.nd, lty=3, axes=FALSE)
axis(4, pretty(range(rw.nd)))
lines(rw.nd, lty=3)
legend(10, 18.7, legend=c('det. trend + noise (ls)', 
                          'rw drift (ls)', 'rw (rs)'), lty=c(1, 2, 3))

#DW---------------------
library(urca)
library(xtable)
data(Raotbl3)
attach(Raotbl3)
lc <- ts(lc, start=c(1966,4), end=c(1991,2), frequency=4)
lc.ct <- ur.df(lc, lags=3, type='trend')
# Slotnames will determine the names for elements in the table. 
slotNames(lc.ct)
class(lc.ct@teststat)
class(lc.ct@cval)
# Put the test-statistics and the critical values together
a <- cbind(t(lc.ct@teststat), lc.ct@cval)
ta <- xtable(a, digits = 2)
ta
summary(lc.ct)

