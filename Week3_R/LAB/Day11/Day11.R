# 20180709
# Sungjae Lee

t.test(1:10, y = NULL, alternative = 'two.sided', mu = 0,
       paired = F, conf.level = 0.95)

setwd('/Users/sungjae/desktop')
tire.G <- read.table('Exam04G.txt', skip = 3, header = T, sep = '\t')
t.test(WR ~ Tire, data = tire.G, alternative = 'greater', paired = T)

#

n.success <- c(94, 112)
n.trial <- c(200, 200)
result <- prop.test(x = n.success, n = n.trial,
                    alternative = 'two.sided', conf.level = 0.95,
                    correct = F)
result

p1 <- result$estimate[1]
p2 <- result$estimate[2]
sign <- ifelse(p1 < p2, -1, 1)
z <- sign * sqrt(result$statistic)
names(z) <- NULL
z

#

s.data <- read.csv('Exam02.csv', skip = 3, header = T)
s.data
aov.result <- aov(sales ~ wrapper, data = s.data)
summary(aov.result)

TukeyHSD(aov.result)

#

b.data <- read.table('Exam04_2.txt', skip = 3, header = T, sep = '\t')
aov.result <- aov(blood ~ group, data = b.data)
summary(aov.result)

b_test <- bartlett.test(blood ~ group, data = b.data)
b_test

#

e.data <- read.table('Exam06.txt', skip = 3, header = T, sep = '\t')
aov.result <- aov(efficiency ~ car + additive, data = e.data)

anova(aov.result)
TukeyHSD(aov.result)

quartz(width = 7, height = 7, family = 'AppleGothic')
plot(TukeyHSD(aov.result, 'car'))
plot(TukeyHSD(aov.result, 'additive'))

quartz.save('2.png', type = 'png', device = dev.cur())

