# 20180712
# Sungjae Lee

#

g.data <- read.csv('Ch08gene.csv', skip = 3, header = T)

total <- with(g.data, table(temp))
g.table <- with(g.data, table(temp, gender))
total; g.table

m.prop <- g.table[, 2]/total
m.prop

temp <- with(g.data, unique(temp))
temp
lm.result <- lm(m.prop ~ temp)
summary(lm.result)

new.temp <- data.frame(temp = c(25.5, 27.5, 28.5, 30.5))
predict.lm(lm.result, newdata = new.temp)

#

quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')
quartz.save('7.png', type = 'png', device = dev.cur())

total <- with(g.data, table(temp))
g.table <- with(g.data, table(temp, gender))
total; g.table

m.prop <- g.table[,2]/total
odds <- m.prop/(1-m.prop)
logit <- log(odds)
temp <- with(g.data, unique(temp))
lm.result <- lm(logit ~ temp)
summary(lm.result)
new.temp <- data.frame(temp = c(25.5, 27.5, 28.5, 30.5))
logit.hat <- predict.lm(lm.result, newdata = new.temp)
prop.hat <- exp(logit.hat)/(1 + exp(logit.hat))
prop.hat

all.x <- data.frame(temp = seq(27, 30, by = 0.1))
all.logit.hat <- predict.lm(lm.result, newdata = all.x)
all.prop.hat <- exp(all.logit.hat)/(1+exp(all.logit.hat))

plot(temp, m.prop, ylim = c(0, 1), pch = 19, xlab = '온도', ylab = '수컷의 비율',
     main = '부화온도에 따른 수컷의 비율')
lines(all.x$temp, all.prop.hat, lwd = 2)
axis(side = 2, at = seq(0, 1, by = 0.1))

#

logit.result <- glm(gender ~ temp, family = binomial, data = g.data)
new.X <- data.frame(temp = c(27, 28))
p.hat <- predict(logit.result, newdata = new.X, type = 'response')
p.hat / (1-p.hat)

exp(coefficients(logit.result))

#

s.data <- read.table('Ch08smoking.txt', skip = 4, header = T)
logit.result <- glm(smoking ~ fam.num + income, family = binomial, data = s.data)
summary(logit.result)

exp(coefficients(logit.result))
p.hat <- predict(logit.result, type = 'response')
hat.data <- cbind(s.data, hat.Y = as.numeric(p.hat >= 0.5))
with(hat.data, table(smoking, hat.Y), hat.Y)

#

s.data <- read.csv('Ch08survival.csv', skip = 3, header = T)
fit.model <- glm(survival ~ carbon, family = binomial, data = s.data)
summary(fit.model)

exp(fit.model$coefficients)

