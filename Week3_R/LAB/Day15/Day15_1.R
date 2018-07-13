# 20180713
# Sungjae Lee

#

s.data <- read.csv('Ch08survival.csv', skip = 3, header = T)
attach(s.data)

alive <- table(carbon[survival == 0])
dead <- table(carbon[survival == 1])

Y.table <- cbind(dead, alive)
Y.table
X.carbon <- unique(carbon)
X.carbon

fit.model <- glm(Y.table ~ X.carbon, family = binomial)
summary(fit.model)

beta.hat <- fit.model$coefficients
X <- cbind(1, X.carbon)
Y <- dead
k <- length(Y)
m <- margin.table(Y.table, margin = 1)
L1 <- c()
L2 <- c()
for(i in 1:k){
  L1[i] <- Y[i] * (X[i,]%*%beta.hat) - m[i]*log(1+exp(X[i,]%*%beta.hat))
  L2[i] <- Y[i]*log(Y[i]/m[i])+(m[i]-Y[i])*log(1-Y[i]/m[i])
  }
log.LF <- sum(L1)
log.LS <- sum(L2)

d0 <- (-2)*(log.LF - log.LS)
d0

#

library(nnet)
library(pscl)

iris.data <- read.csv('Ch08iris.csv')
head(iris.data)

result <- multinom(Species ~., data = iris.data, model = T)
summary(result)

coef(result)/summary(result)$standard.errors
exp(coef(result))
head(fitted(result))

new.g <- predict(result)
new.p <- predict(result, type = 'probs')
head(new.g)
head(new.p)

xtabs(~new.g + iris.data$Species)
sum(new.g == iris.data$Species)/NROW(iris.data)

pR2(result)

detach('package:nnet', unload = T)
detach('package:pscl', unload = T)

#

library(MASS)

h.data <- read.table('Ch08body.txt', skip = 4, header = T)

lda.result <- lda(h.data[,c('height', 'weight')],
                  grouping = h.data$gender,
                  prior = c(0.5, 0.5))
lda.result

Male <- subset(h.data, subset = (gender == '남자'),
               select = c(height, weight))
Female <- subset(h.data, subset = (gender == '여자'),
               select = c(height, weight))
predict(lda.result)

detach('package:MASS', unload = T)

#

library(MASS)

p.data <- read.table('Ch08farm.txt', skip = 4, header = T)
pur <- subset(p.data, subset = (Group == '구매'),
              select = c(X1, X2))
n.pur <- subset(p.data, subset = (Group == '비구매'),
                select = c(X1, X2))

#

library(biotools)
s.data <- read.table('Ch08salmon.txt', skip = 4, header = T)

Al <- subset(s.data, subset = (Area == 'Alaska'),
             select = c(X1, X2))
Ca <- subset(s.data, subset = (Area == 'Canada'),
             select = c(X1, X2))

boxM(s.data[,2:3], grouping = s.data[,1])

library(MASS)
result <- qda(s.data[,2:3], grouping = s.data[,1],
              prior = c(0.5, 0.5))
result

predict(result)
quartz(width = 7, height = 7, bg = 'transparent')

#

new.X1 <- seq(min(s.data$X1), max(s.data$X1), by = 0.5)
new.X2 <- seq(min(s.data$X2), max(s.data$X2), by = 0.5)
new.X <- cbind(rep(new.X1, length(new.X2)),
               rep(new.X2, each = length(new.X1)))
score <- c()

#

s.data ; Al ; Ca

Al.index <- sort(sample(1:45, size = 27))
Ca.index <- sort(sample(46:90, size = 27))

tra.data <- s.data[c(Al.index, Ca.index),]
val.data <- s.data[-c(Al.index, Ca.index),]

tra.data
val.data

library(MASS)

result <- qda(tra.data[,2:3], grouping = tra.data[,1],
              prior = c(0.5, 0.5))
qda(tra.data[,2:3], grouping = tra.data[,1], prior = c(0.5, 0.5))

val.Group <- predict(result, newdata = val.data[,2:3])
detach('package:MASS', unload = T)
table(val.data$Area, val.Group$class)

sum(val.data$Area == val.Group$class)/nrow(val.data)

#

library(MASS)

result <- qda(s.data[,2:3], grouping = s.data[,1], prior = c(0.5, 0.5), CV = T)
result
table(s.data$Area, result$class)

sum(s.data$Area==result$class)/nrow(s.data)

detach('package:MASS', unload = T)
