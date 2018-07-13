# 20180713
# Sungjae Lee

#

library(MASS)
setwd('/Users/sungjae/desktop')
skull <- read.table('Ch08skull.txt', head = T, skip = 6, sep = '\t')
head(skull, n = 3)

attach(skull)
n <- dim(skull)[1]
X <- skull[, 1:4]
head(X, n = 3)
tail(X, n = 3)

install.packages('klaR')
install.packages('biotools')
library(klaR)
library(biotools)

quartz(width = 7, height = 7, bg = 'transparent')
quartz.save('8.png', type = 'png', device = dev.cur())

ldahist(X1, g = Year, type = 'histogram', col = 'black', border = 'white')

ldahist(X2, g = Year, type = 'density')

ldahist(X3, g = Year, type = 'both', col = 'black', border = 'white')

ldahist(X4, g = Year, type = 'both', col = 'black', border = 'white')

ld <- lda(Year ~ X1 + X2 + X3 + X4, data = skull)
ld
pred.ld <- predict(ld, X)
attributes(pred.ld)
pred.LG <- pred.ld$class
pred.LG

pred.LG <- as.numeric(pred.LG)
pred.LG[pred.LG == 1] <- (-4000)
pred.LG[pred.LG == 2] <- 150
pred.LG

Lc.Table <- table(Year, pred.LG)
Lc.Table
ld.correct.rate <- sum(diag(Lc.Table))/n
ld.error.rate <- 1 - ld.correct.rate
Lc.Table ; ld.correct.rate ; ld.error.rate

partimat(X, factor(Year, labels = c('B', 'A')), data = skull, method = 'lda')

partimat(X, factor(Year, labels = c('B', 'A')), data = skull, method = 'lda',
         plot.matrix = T, imageplot = F)

#

qd <- qda(X, Year)
pred.qd <- predict(qd)
attributes(pred.qd)
pred.QG <- pred.qd$class
pred.QG

pred.QG <- as.numeric(pred.QG)
pred.QG[pred.QG == 1] <- (-4000)
pred.QG[pred.QG == 2] <- (-150)

Qc.Table <- table(Year, pred.QG)
qd.correct.rate <- sum(diag(Qc.Table))/n
qd.error.rate <- 1 - qd.correct.rate
Qc.Table ; qd.correct.rate; qd.error.rate

partimat(X, factor(Year, labels = c('B', 'A')),
         data = skull, method = 'qda')
partimat(X, factor(Year, labels = c('B', 'A')),
         data = skull, method = 'qda',
         plot.matrix = T, imageplot = F)

#

credit <- read.table('Ch08credit.txt', head = T, skip = 13, sep = ',')
credit

credit$G <- factor(credit$G, labels = c('양호', '모호', '불량'))
head(credit, n = 3)
tail(credit, n = 3)

attach(credit)
n <- dim(credit)
X <- credit[, 2:11]

head(X, n = 3)
tail(X, n = 3)

library(MASS)
library(klaR)

ld <- lda(X, G)
ld

pred.ld <- predict(ld, X)
attributes(pred.ld)
pred.G <- pred.ld$class
c.table <- table(G, pred.G)
ld.correct.rate <- sum(diag(c.table))/n
ld.error.rate <- 1 - ld.correct.rate

addmargins(c.table, margin = 2, FUN = sum)
prop.table(c.table, margin = 1)
ld.correct.rate; ld.error.rate

stepclass(X, G, method = 'lda')

ld <- lda(G ~ X1 + X2 + X8)
ld

pred.ld <- predict(ld, X)
attributes(pred.ld)
pred.G <- pred.ld$class
c.table <- table(G, pred.G)
ld.correct.rate <- sum(diag(c.table))/n
ld.error.rate <- 1 - ld.crrect.rate

addmargins(c.table, margin = 2, FUN = sum)
prop.table(c.table, margin = 1)
ld.correct.rate ; ld.error.rate

#

result <- stepclass(X, G, method = 'qda', improvement = 0.001)
qd <- qda(result$formula)
result$formula
qd <- qda(G ~ X1 + X2 + X3 + X8)

pred.qd <- predict(qd, X)
pred.G <- pred.qd$class
c.table <- table(G, pred.G)

qd.correct.rate <- sum(diag(c.table))/n
qd.error.rate <- 1 - qd.correct.rate

addmargins(c.table, margin = 2, FUN = sum)
prop.table(c.table, margin = 1)
qd.correct.rate; qd.error.rate

quartz(width = 10, height = 10, bg = 'transparent')
par(family = 'AppleGothic')
quartz.save('8.png', type = 'png', device = dev.cur())

partimat(X[, result$model[,2]], G, data = credit, method = 'qda')
 
