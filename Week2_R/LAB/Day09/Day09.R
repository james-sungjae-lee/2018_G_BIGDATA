# 20180705
# Sungjae Lee

# Skewness and Kurtosis with moments package

library(moments)

Sat.Data <- read.table('G_BIG/Week2_R/LAB/Day07/satisfaction.txt', skip = 5, sep=',', header = T)
x <- rnorm(100, 30, 5)
x2 <- rchisq(100, 2)

skewness(Sat.Data$S) ; kurtosis(Sat.Data$S)
skewness(x) ; kurtosis(x)
skewness(x2) ; kurtosis(x2)

# Skewness and Kurtosis with psych package

library(psych)

describe(Sat.Data[, 4:6])
describe(x)

# 

x_table <- cut(x, breaks = c(10, 20, 30, 40, 50, 60), 
               labels = NULL, include.lowest = F, right = T)
summary(x_table)

#

Inter <- cut(Sat.Data$S, breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
             include.lowest = T, right = T)
table(Inter)
summary(Inter)

Inter2 <- cut(Sat.Data$S, breaks = 7, include.lowest = T, right = T)
table(Inter2)

#

apply(Sat.Data[, 4:7], MARGIN = 2, FUN = mean)
apply(Sat.Data[, 4:7], MARGIN = 2, FUN = summary)
lapply(Sat.Data[, 4:7], FUN = mean)

for (i in 4:7){
  print(mean(Sat.Data[,i]))
}

apply(head(Sat.Data[, 5:7]), MARGIN = 1, FUN = sum)

for(i in 1:6){
  cat(sum(Sat.Data[i, 5:7]), ' ')
}

sapply(head(Sat.Data[, 5:7]), FUN = summary, simplify = T)

aggregate(Sat.Data[, 4:7], by = list(Sat.Data$City), FUN = mean)

apply(Sat.Data[Sat.Data$City == '서울시', 4:7], MARGIN = 2, FUN = mean)

# Graphic Device

quartz(width = 7, height = 7, bg = 'transparent')

barplot(c(1:10))
plot(x)
pie(1:10)

dev.list()
dev.set()
dev.cur()

quartz.save(file = 'x.png', type = 'png', device = dev.cur())

graphics.off()
dev.list()
dev.off()

dev.next(2)
dev.prev(2)

# Setting Graphic Device

par()
par(no.readonly = T)

par(pch = 20)
pch.old <- par(pch = 1)
pch.old2 <- par(pch = 3, cex = 1.5)
par(pch.old2)

plot(1:10, pch=19, cex=0.5)

x <- rnorm(100, 50, 5)
y <- 1.5 + 2.5*x + rnorm(100, 0, 5)
plot(x, y, main = 'pch = 1, cex = 1', pch = 1, cex = 1)
plot(x, y, main = 'pch = 3, cex = 1.5', pch = 3, cex = 1.5)

# Pie and Bar Graph

pie(c(1, 1, 1), labels = c('a', 'b', 'c'), main = 'my_pie_graph')

barplot(c(2, 5, 3, 1.5, 0.5, 4), main = 'my_barplot_graph', xlab = 'x label', ylab = 'y label')

barplot(c(1:10))
plot(x)
pie(1:10)

# 

stem_data = c(17, 12, 37, 40, 31, 41, 35, 27, 28, 34,
           28, 33, 53, 18, 48, 16, 35, 34, 29, 30,
           31, 42, 29, 40, 26)
stem(stem_data)
stem(1:99)

boxplot(stem_data, horizontal = T)

hist(stem_data, breaks = 5, include.lowest = T, right = T, labels = F)

plot(x, type = 'b')

# Graph Testing with Dataset

attach(Sat.Data)

C.Table <- table(Class)
barplot(C.Table, main = 'Class', xlab = 'Grade')

Cross.Table <- table(Class, City)
barplot(Cross.Table, beside = T, legend.text = T) 

D.Table <- table(Dept)
pie(D.Table, main = 'Dept')

boxplot(S~City, horizontal = T, main = 'location')
box_result <- boxplot(S~City * Class)

plot(Q1, S, pch = 20, main='종합만족도와 추천의도', xlab = '추천의도', ylab = '종합만족도')

plot(Sat.Data[,4:7], pch = 20, main = '산점도 행렬')

plot(cov(Sat.Data[4:7]))

par(family = 'AppleGothic')
getwd()
setwd('/Users/sungjae/desktop')

#

install.packages('foreign')
library(foreign)

Profit <- read.spss('Profit.sav', use.value.labels = T, to.data.frame = T, reencode='euc-kr')

quartz(width = 7, height = 7, bg = 'transparent')

attach(Profit)

#

par(fig=c(0, 1, 0.5, 1))
plot(Asset, Property)

par(fig = c(0, 0.5, 0, 0.5), new = T)
hist(Asset)

par(fig = c(0.5, 1, 0, 0.5), new = T)
hist(Property)

#

quartz(width = 7, height = 7, bg = 'transparent')

split.screen(fig = c(2, 2))
split.screen(figs = c(1, 2), screen = 2)
screen(n = 5)
hist(Asset)
screen(n = 3)
hist(Finance, col = topo.colors(15))
screen(n = 6)
hist(Property, col = rainbow(15))
screen(n = 1)
plot(Asset, Finance)

erase.screen(n = 2)
screen(n = 6)
plot(Asset, Finance)
dev.off(4)

#

quartz(width = 7, height = 7, bg = 'transparent')

layout(matrix(c(1, 1, 0, 3, 1, 1, 0, 4, 2, 2, 2, 4, 2, 2, 2, 5),
              ncol = 4, byrow = T))

layout(matrix(c(1, 1, 2, 2, 1, 1, 2, 2, 3, 3, 4, 4, 3, 3, 4, 4),
              ncol = 4, byrow = T))
layout.show(5)

layout(matrix(c(1, 0, 2, 3, 0, 4), ncol = 3, byrow = T),
              heights = c(4, 1), widths = c(3, 1, 3))
layout.show(4)
Asset.count <- hist(Asset,
                    breaks = seq(min(Asset), max(Asset), length.out = 10),
                    plot = F)$count
Finance.count <- hist(Finance,
                      breaks = seq(min(Finance), max(Finance), length.out = 10),
                      plot = F)$count
max.count <- max(c(Asset.count), Finance.count)
par(mar = c(0, 2, 2, 0))
hist(Asset,
     breaks = seq(min(Asset), max(Asset), length.out = 10),
     ylim = c(0, max.count), col = rainbow(10))

par(mar = c(0, 2, 2, 0))
hist(Finance,
     breaks = seq(min(Finance), max(Finance), length.out = 10),
     ylim = c(0, max.count), col = topo.colors(10))

par(mar = c(2, 2, 0, 0))
boxplot(Asset, axes = F, horizontal = T)

par(mar = c(2, 2, 0, 0))
boxplot(Finance, axes = F, horizontal = T)

# Chart Naming

quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')

x <- rbinom(1000, 3, 0.7)
x.table <- table(x)

pie(x.table, labels = c('FRESHMAN', 'SOPHOMORE', 'JUNIOR', 'SENIOR'),
    col = rainbow(4, s = 0.5, v = 1), edges = 200, radius = 0.7,
    density = 50, cex = 0.7, angle = 45)

title(main = '학년', cex.main = 2, col.main = 'darkblue',
      font.sub = 3)

box(which = 'plot', lty = 'solid', lwd = 1, col = 'navy')

legend(-1.1, 1, legend = c('FRESHMAN', 'SOPHOMORE', 'JUNIOR', 'SENIOR'),
       fill = rainbow(4, s = 0.5, v = 1), col = topo.colors(4),
       lty = 3, lwd = 2, angle = 45, density = 50, bty = 'o',
       bg = 'navy', cex = 1, xjust = 0, yjust = 1, text.width = 0.5,
       text.col = cm.colors(4), plot = T, ncol = 2, horiz = F)

plot(1:10, type='n', main = 'text',
     xlab = '', ylab = '', axes = F)

text(4, 5, labels = 'cex = 1.5, col = \'blue\', font = 4',
     cex = 1.5, col = 'blue', font = 4)

points(2, 4)
text(2, 4, adj = 0, labels = 'adj = 0')
points(2, 3)
text(2, 3, adj = 0.5, labels = 'adj = 0.5')
points(2, 2)
text(2, 2, adj = 1, labels = 'adj = 1')

quartz.save(file = '25.png', type = 'png', device = dev.cur())

