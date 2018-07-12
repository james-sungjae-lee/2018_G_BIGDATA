# 20180712
# Sungjae Lee 

#

library(car)

setwd('/Users/sungjae/desktop')
quartz(width = 7, height = 7, bg = 'transparent')
quartz.save('1.png', type = 'png', device = dev.cur())

fat.data <- read.csv('Ch07Exam13.csv', skip = 5, header = T)
plot(fat.data, pch = 19)
cor(fat.data)

lm.result <- lm(fat ~ triceps + thigh + wrist, data = fat.data)
1/vif(lm.result)
vif(lm.result)

lm.result <- lm(fat ~ thigh + wrist, data = fat.data)
1/vif(lm.result)
vif(lm.result)

summary(lm.result)

detach('package:car', unload = T)

#

library(MASS)

select(lm.ridge(fat~triceps + thigh + wrist, data = fat.data))
select(lm.ridge(fat ~ triceps + thigh + wrist, data = fat.data,
                lambda = seq(0, 1, length.out = 100)))

ridge.res <- lm.ridge(fat ~ triceps + thigh + wrist, data = fat.data,
                      lambda = 0.020202)
lm(fat~triceps + thigh + wrist, data = fat.data)
ridge.res

select(lm.ridge(fat ~ triceps + thigh, data = fat.data,
                lambda = seq(0, 10, length.out = 100)))
ridge.res <- lm.ridge(fat ~ triceps + thigh, data = fat.data,
                      lambda = 2.323232)
ridge.res

detach('package:MASS', unload = T)

#

quartz(width = 7, height = 7, bg = 'transparent')
quartz.save('3.png', type = 'png', device = dev.cur())
fat.data <- read.csv('Ch07Exam14.csv', skip = 5, header = T)

lm.result <- lm(fat ~ triceps + thigh + wrist, data = fat.data)
h.val <- hatvalues(lm.result)
c.dist <- cooks.distance(lm.result)
h.val
c.dist

p <- summary(lm.result)$fstatistic[2]
n <- sum(summary(lm.result)$fstatistic[2:3], 1)
h.val >= (2*(p+1)/n)
c.dist >= 1

layout(matrix(1:4, ncol = 2))
plot(lm.result, pch = 19)

e <- residuals(lm.result)
shapiro.test(e)

library(lmtest)

bptest(lm.result)
dwtest(lm.result, alternative = 'two.sided')

detach('package:lmtest', unload = T)

library(car)

par(family = 'AppleGothic')
crPlots(lm.result, pch = 19, main = '편잔차도표')

detach('package:car', unload = T)

#

quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')
quartz.save('5.png', type = 'png', device = dev.cur())

pl.data <- read.table('Ch07Exam15.txt', skip = 3, header = T)
pl.data <- data.frame(pl.data, lnY = log(pl.data$Y))
pl.data

attach(pl.data)

lm.result <- lm(Y ~ X, data = pl.data)
hatY <- fitted(lm.result)
e <- residuals(lm.result)
s <- rstandard(lm.result)
r <- rstudent(lm.result)

cbind(X, hatY, e, s, r)
layout(matrix(1:4, ncol = 2, byrow = T))
plot(X, Y, pch = 19, main = '변환전 산점도')
abline(lm.result)
plot(hatY, e, pch = 19, main = '잔차 산점도 : e')
abline(h = 0, lty = 2)
plot(hatY, r, pch = 19, main = '외표준화잔차 산점도 : r')
abline(h = 0, lty = 2)
qqnorm(e, pch = 19)
qqline(e)

#

tran.result <- lm(lnY ~ X, data = pl.data)
tran.hatY <- fitted(tran.result)
tr.e <- residuals(tran.result)
tr.s <- rstandard(tran.result)
tr.r <- rstudent(tran.result)
cbind(X, tran.hatY, tr.e, tr.s, tr.r)

layout(matrix(1:4, ncol = 2, byrow = T))
plot(X, Y, pch = 19, main = '변환전 산점도')
abline(tran.result)
plot(tran.hatY, tr.e, pch = 19, main = '잔차 산점도 : e')
abline(h = 0, lty = 2)
plot(tran.hatY, tr.r, pch = 19, main = '외표준화잔차 산점도 : r')
abline(h = 0, lty = 2)
qqnorm(tr.e, pch = 19)
qqline(tr.e)

#

quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')
quartz.save('6.png', type = 'png', device = dev.cur())

library(quantreg)

x <- rnorm(500, 15, 5)
y <- 10 + 1.5*x + rnorm(500, 0, 10)
y <- 10 + 1.5*x + rcauchy(500, location = 0, scale = 1)

par(mai = c(0.5, 0.5, 0.75, 0.2), mgp = c(1, 1, 0))
plot(x, y, cex = 0.75,
     main = 'Least Square Estimate Regression vs. Quantile Regression',
     xlab = 'x', ylab = 'y')

abline(lm(y~x), lty = 1, lwd = 2, col = 'red')
abline(rq(y~x, tau = 0.5), lty = 2, lwd = 2, col = 'blue')

#

VOD <- read.csv('Ch07Exam16.csv', skip = 6, header = T)
lm.result <- lm(Y ~ X1 + X2 + X3 + X4, data = VOD)
cor(VOD)

library(car)

1/vif(lm.result)
vif(lm.result)

detach('package:car', unload = T)

summary(lm.result)

summary(step(lm.result, direction = 'both'))
