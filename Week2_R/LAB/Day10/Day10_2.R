# 20180706
# Sungjae Lee

#

quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')

t.test(0:10, alternative = 'two.sided', mu = 0, conf.level = 0.95)

shapiro.test(1:10)
qqnorm(1:10, datax = F)
qqline(1:10, data = F)

#

drug <- scan('Exam04.txt')

mu0 = 0.5
qt(0.05, 27, lower.tail = F)
t.test(drug, alternative = 'greater', mu = mu0, conf.level = 0.95)

t.test(drug, mu = mu0, conf.level = 0.95)

shapiro.test(drug)
qqnorm(drug)
qqline(drug)

# 

milk <- scan('Exam05.txt')
alpha <- 0.05
n <- length(milk)
s0 = 15
varx <- var(milk)

v0 <- (n-1) * varx / s0
rj <- qchisq(alpha, n-1)
pval <- pchisq(v0, n-1)

v0 ; rj ; pval

#

n <- 2500
x <- 160
hatp <- x/n
p0 <- 0.075
alpha <- 0.1

za <- qnorm(alpha/2, lower.tail = F)
pL <- hatp - za * sqrt(hatp * (1 - hatp) / n)
pU <- hatp + za * sqrt(hatp * (1 - hatp) / n)

hatp ; pL ; pU

result <- prop.test(x = x, n = n, p = p0, conf.level = 0.9, correct = F)
result

#

ra <- read.csv('Exam02.csv', skip = 3, header = T)

shapiro.test(ra$A)
qqnorm(ra$A)
qqline(ra$A)

shapiro.test(ra$B)
qqnorm(ra$B)
qqline(ra$B)

t.test(ra$A, ra$B, var.equal = F, alternative = 'greater')

#

ra.G <- read.csv('Exam02G.csv', skip = 3, header = T)

for(i in c('A', 'B')){
  X <- ra.G$score[ra.G$class == i]
  shapiro.test(X)
  print(shapiro.test(X))
  quartz(width = 7, height = 7, bg = 'transparent')
  qqnorm(X)
  qqline(X)
}

#

BP <- read.table('Exam03.txt', skip = 3, header = T)

shapiro.test(BP$G1)
quartz(width = 7, height = 7, bg = 'transparent')
qqnorm(BP$G1)
qqline(BP$G1)

shapiro.test(BP$G2)
qqnorm(BP$G2)
qqline(BP$G2)

boxplot(BP$G1, BP$G2)

t.test(BP$G1, BP$G2, alternative = 'greater', var.equal = T)

#

BP.G <- read.table('Exam03G.txt', skip = 3, header = T)

for(i in c('G1', 'G2')){
  X <- BP.G$diffBP[BP]
}

quartz.save(file = '8.png', type = 'png', device = dev.cur())

