# 20180711
# Sungjae Lee

#

ad.data <- read.table('Ch07Exam06.txt', skip = 4, sep = '\t', header = T)
quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')
quartz.save('1.png', type = 'png', device = dev.cur())

attach(ad.data)

lm.result <- lm(sales~ad.cost)
anova(lm.result)
summary(lm.result)$r.squared

plot(ad.cost, sales, main = '광고비와 판매액의 관계', pch = 19)
abline(coef = coef(lm.result))
abline(h = mean(sales), v = mean(ad.cost), lty = 2)

detach(ad.data)

#

ad.data2 <- read.table('Ch07Exam07.txt', skip = 4, sep = '\t', header = T)
quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')
quartz.save('3.png', type = 'png', device = dev.cur())

ad.data2
attach(ad.data2)

lm.result2 <- lm(sales ~ ad.cost)
summary(lm.result)
confint(lm.result)

summary(lm.result)$coefficients[2, 3]^2
summary(lm.result)$fstatistic[1]

detach(ad.data2)

#

ad.data3 <- read.table('Ch07Exam08.txt', skip = 4, sep = '\t', header = T)

attach(ad.data3)

lm.result3 <- lm(sales ~ ad.cost)
new.cost <- data.frame(ad.cost = c(2.5, 3, 3.5))

predict(lm.result3, newdata = new.cost,
        interval = 'confidence')
predict(lm.result3, newdata = new.cost,
        interval = 'prediction')
predict(lm.result3, newdata = new.cost)

sta <- min(ad.cost)
fin <- max(ad.cost)
all.cost <- data.frame(ad.cost = seq(-10, 20, length = 50))

conf.result <- predict(lm.result, newdata = all.cost,
                       interval = 'confidence')
pred.result <- predict(lm.result, newdata = all.cost,
                       interval = 'prediction')
ylim.min <- min(conf.result, pred.result)
ylim.max <- max(conf.result, pred.result)

with(ad.data,
     plot(ad.cost, sales, xlim= c(-10, 20), ylim = c(ylim.min, ylim.max), pch = 19))
abline(lm.result, lwd = 2)

lines(all.cost$ad.cost, conf.result[,2], lty = 2, lwd = 2)
lines(all.cost$ad.cost, conf.result[,3], lty = 2, lwd = 2)

lines(all.cost$ad.cost, pred.result[,2], lty = 4, lwd = 2)
lines(all.cost$ad.cost, pred.result[,3], lty = 4, lwd = 2)

detach(ad.data3)

#

time.data <- read.csv('Ch07Exam09.csv', skip = 3, header = T)
quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')
quartz.save('6.png', type = 'png', device = dev.cur())

attach(time.data)

lm.result <- lm(survival.time ~ population)
summary(lm.result)

plot(population, survival.time)
abline(lm.result)

e <- residuals(lm.result)
s <- rstandard(lm.result)
r <- rstudent(lm.result)
cbind(population, e, s, r)

layout(matrix(1:4, ncol = 2))
plot(population, e, pch = 19,
     main = '설명변수에 대한 잔차 산점도 : e')
abline(h = 0, lty = 2)
plot(population, s, pch = 19,
     main = '설명변수에 대한 잔차 산점도 : s')
abline(h = 0, lty = 2)
plot(population, r, pch = 19,
     main = '설명변수에 대한 잔차 산점도 : r')
abline(h = 0, lty = 2)
qqnorm(e, pch = 19)
qqline(e)

layout(matrix(1:4, ncol = 2))
plot(lm.result, pch = 19)

shapiro.test(e)

library('lmtest')

bptest(lm.result)
dwtest(lm.result, alternative = 'two.sided')

detach('package:lmtest', unload = T)
detach(time.data)

#

cosmetic <- read.table('Ch07Exam10.txt', skip = 4, header = T)
attach(cosmetic)

reduced.model <- lm(Y ~ 1)
full.model <- lm(Y ~ X1 + X2)

summary(full.model)
anova(full.model)
anova(reduced.model, full.model)

#

steel <- read.table('Ch07Exam11.txt', skip = 4, header = T)
attach(steel)

reduced.model <- lm(Y ~ 1)
full.model <- lm(Y ~ X1 + X2)

summary(full.model)


