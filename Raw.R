# read data--------
da <- read.csv("coint6.csv", header = TRUE)
head(da)
plot(da$y, lty=1, ylab='', xlab='', ylim = c(-10,2), main = 'Plot of y, z and w',
     type = 'l')
lines(da$z, lty=2)
lines(da$w, lty = 3)
legend('bottomleft', legend=c('y', 'z', 'w'), 
            lty=c(1, 2, 3))
# Engle-Granger and test of residuals
eq1 <- lm(y ~ z + w, data = da)
resy <- eq1$residuals
# embed will create the lag
lresy <- embed(resy,2)     
dresy <- diff(resy)
dresyl4 <- embed(dresy, 5)
resym <- cbind(lresy, dresy)
resym2 <-cbind(resym[5:99,], dresyl4)
eqresy <- lm(resym[,3] ~ 0 + resym[,2])
eqresy2 <- lm(resym2[,3] ~ resym2[,2] + resym2[,5:8])
M <-matrix(0, nrow = 6, ncol = 2)
M[1:2,1] <- t(coefficients(summary(eqresy))[c(1,3)])
M[1:2,2] <- t(coefficients(summary(eqresy2))[2,c(1,3)])

eq2 <- lm(z ~ y + w, data = da)
resz <- eq2$residuals
# embed will create the lag
lresz <- embed(resz,2)     
dresz <- diff(resz)
dreszl4 <- embed(dresz, 5)
reszm <- cbind(lresz, dresz)
reszm2 <-cbind(reszm[5:99,], dreszl4)
eqresz <- lm(reszm[,3] ~ 0 + reszm[,2])
eqresz2 <- lm(reszm2[,3] ~ reszm2[,2] + reszm2[,5:8])
M[3:4,1] <- t(coefficients(summary(eqresz))[c(1,3)])
M[3:4,2] <- t(coefficients(summary(eqresz2))[2,c(1,3)])
eq3 <- lm(w ~ z + y, data = da)
resw <- eq3$residuals
# embed will create the lag
lresw <- embed(resw,2)     
dresw <- diff(resw)
dreswl4 <- embed(dresw, 5)
reswm <- cbind(lresw, dresw)
reswm2 <-cbind(resym[5:99,], dreswl4)
eqresw <- lm(reswm[,3] ~ 0 + reswm[,2])
eqresw2 <- lm(reswm2[,3] ~ reswm2[,2] + reswm2[,5:8])
M[5:6,1] <- t(coefficients(summary(eqresw))[c(1,3)])
M[5:6,2] <- t(coefficients(summary(eqresw2))[2,c(1,3)])
xtable(M, caption = 'Estimated $a_1$')
require(urca)
qunitroot(0.05, N = 100)     

     
     str(deqresy$summary) 
     resy <- eq1$residuals
     eq2 <- lm(da$z ~ da$y + da$w)
     resz <- eq2$residuals
     eq3 <- lm(da$w ~ da$z + da$y)
     resw <- eq3$residuals)

     
     
require(urca)
plot(eq1$residuals)
hist(eq1$residuals)
test <- ur.df(eq1$residuals, type = "none", selectlags = "AIC")
summary(test)
qunitroot(0.01, N = 100, trend = "nc")
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
