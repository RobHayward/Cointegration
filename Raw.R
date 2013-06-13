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
lc.ct@teststat
lc2 <- diff(lc)
lc2.ct <- ur.df(lc2, type = "trend", lags = 3)
summary(lc2.ct)
# De-trend--------------------
# Testing the de-trend code on note 1 page 53 Pfaff
detrended <- residuals(lm(lc ~ seq(along = lc)))
plot(detrended, type = 'l', main = "De-trended series")
# KPSS-----------
library(urca)
data(nporg)
ir <- na.omit(nporg[, "bnd"])
wg <- log(na.omit(nporg[, "wg.n"]))
ir.kpss <- ur.kpss(ir, type = "mu", use.lag=8)
wg.kpss <- ur.kpss(wg, type = "tau", use.lag=8)
summary(ir.kpss)
summary(wg.kpss)
# jj------------
set.seed(12345)
e1 <- rnorm(250, 0, 0.5)
e2 <- rnorm(250, 0, 0.5)
e3 <- rnorm(250, 0, 0.5)
u1.ar1 <- arima.sim(model = list(ar = 0.75),
                    innov = e1, n = 250)
plot(u1.ar1)
u2.ar1 <- arima.sim(model = list(ar = 0.3),
                    innov = e2, n = 250)
plot(u2.ar1)
y3 <- cumsum(e3)
y1 <- 0.8 * y3 + u1.ar1
y2 <- -0.3 * y3 + u2.ar1
y.mat <- data.frame(y1, y2, y3)
vecm <- ca.jo(y.mat)
class(vecm)
summary(vecm)
jo.results <- summary(vecm)
vecm.r2 <- cajorls(vecm, r = 2)
vecm.r2
class(jo.results)
slotNames(jo.results)
jo.results@teststat
require(xtable)
print(xtable(a <- cbind(jo.results@teststat,jo.results@cval),digits = 2))

(jo.results@teststat)
