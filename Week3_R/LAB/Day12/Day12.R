# 20180710
# Sungjae Lee

#

getwd()
setwd('/Users/sungjae/desktop')

quartz(width = 7, height = 7, family = 'AppleGothic', bg = 'transparent')
quartz.save('3.png', type = 'png', device = dev.cur())

b.data <- read.table('Exam08.txt', skip = 4, header = T, sep = '\t')
layout(matrix(c(1,2), nrow = 2))

with(b.data, interaction.plot(position, size, sales))
with(b.data, interaction.plot(size, position, sales))

aov.result <- aov(sales ~ position + size + position:size, data = b.data)
anova(aov.result)

aov.result2 <- aov(sales ~ position + size, data = b.data)
anova(aov.result2)

#

wine.data <- read.table('Ch7Exam01_2.txt', skip = 4, header = T, sep = '\t')

quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')

quartz.save('4.png', type = 'png', device = dev.cur())

attach(wine.data)
plot(wine, heartdisease,
     main = '포도주와 심장질환간의 관계',
     xlab = '포도주 섭취량', ylab = '심장질환 사망자수',
     pch = 20, col = 'blue')
detach(wine.data)

#

quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')
quartz.save('5.png', type = 'png', device = dev.cur())

car.data <- read.csv('Ch07Exam02.csv', skip = 3, header = T)

attach(car.data)
cor(velocity, fuel)
cor(car.data)

plot(log(velocity), fuel, main = '속도와 연료 소모량',
     xlab = '속도', ylab = '연료 소모량', pch = 19)
detach(car.data)

#

quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')
quartz.save('6.png', type = 'png', device = dev.cur())

point.data <- read.table('Ch07Exam03.txt', skip = 3, header = T, sep = '\t')

attach(point.data)

plot(statistics, calculus, main = '통계학과 미적분학',
     xlab = '통계학 점수', ylab = '미적분학 점수',
     pch = 19)

cor.test(statistics, calculus)

detach(point.data)

#

d.data <- read.csv('Ch07Exam04.csv', skip = 3, header = T)

lm(response ~ alcohol, data = d.data)
lm(formula = response ~ alcohol, data = d.data)

#

quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')
quartz.save('9.png', type = 'png', device = dev.cur())

d.data <- read.csv('Ch07Exam05.csv', skip = 3, header = T)

attach(d.data)

lm.result <- lm(response ~ alcohol)

summary(lm.result)$r.squared
cor(alcohol, response)
cor(alcohol, response)^2

anova(lm.result)
anova(lm.result)[2, 3]

plot(alcohol, response,
     main = '음주량과 반응시간의 관계',
     pch = 19)

abline(coef = coef(lm.result))
abline(h = mean(response), v = mean(alcohol), lty = 2)

detach(d.data)
