# 20180704
# Sungjae Lee

#

Pxy <- matrix(c(0, 1/4, 1/4, 1/4, 1/4, 0), ncol = 3, byrow = T)
dimnames(Pxy) <- list(0:1, 0:2)
Py <- margin.table(Pxy, margin = 1)
Px <- margin.table(Pxy, margin = 2)
Pxy[2,]/Py[2]

#

fx1 <- function(x) { (2*x + 4) / 5 }
integrate(fx1, lower = 0, upper = 1/2)

#

X2 <- 0:2
Px2 <- c(0.25, 0.5, 0.25)
names(Px2) <- X2
Ex2 <- sum(X2 * Px2)
Ex2_2 <- sum((X2 ** 2) * Px2)
VarX2 <- Ex2_2 - Ex2 ** 2
VarX2 ** 0.5

#
n <- 15
p <- 0.4
dbinom ( 10, n, p )
pbinom ( 2, n, p )
Ex <- n*p
VarX <- n*p*(1-p)

#

pnorm(45, 30, 5) - pnorm(40, 30, 5)
1-pnorm(40, 30, 5)
pnorm(40, 30, 5, lower.tail = F)
pnorm(25, 30, 5) # P(X <= 25)

#

pnorm(3) - pnorm(2)
pnorm(2, lower.tail = F)
pnorm(-1)
pnorm(1, lower.tail = F)

pnorm(60, 70, 10)

# Chi Square

pchisq(1.25, 3, lower.tail = F)
pchisq(0.5, 9)
qchisq(0.025, 16)
qchisq(0.95, 10, lower.tail = F)
qchisq(0.05, 10)

# T

dt(1.5, 3)
pt(1.5, 3, lower.tail = F)
pt(1.75, 10) - pt(-0.5, 10)
qt(0.99, 20)
qt(0.1, 7, lower.tail = F)
qt(0.9, 7)

#

Sat.Data <- read.table('G_BIG/Week2_R/LAB/Day07/satisfaction.txt', skip = 5, sep=',', header = T)
head(Sat.Data, n = 10)
Mode <- function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mean(Sat.Data[ ,4])
mean(Sat.Data$S)
mean(Sat.Data$S, trim=0.01)

median(Sat.Data$Q1)
median(Sat.Data$S)

Mode(Sat.Data$S)
Mode(Sat.Data$Q2)

#

var(Sat.Data[ ,6])
sd(Sat.Data$Q2)

cov(Sat.Data$Q1, Sat.Data$Q2)
cov(Sat.Data[ ,5:7])

range(Sat.Data$S)
quantile(Sat.Data$S)
quantile(Sat.Data$S, probs = c(0.1, 0.3, 0.6, 0.9))

summary(Sat.Data$S)
summary(Sat.Data)
