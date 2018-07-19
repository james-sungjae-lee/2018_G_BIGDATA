# 20180719
# Sungjae Lee

# Ex1

setwd('/Users/sungjae/desktop')
HP.Data <- read.csv('Ch15HousePrice.csv', skip = 7, header = T)
attach(HP.Data)
quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')
quartz.save('6.png', type = 'png', device = dev.cur())

plot(NRoom, Nbath, xlab = '방의 수', ylab = '욕실 수')
sunflowerplot(NRoom, Nbath, xlab = '방의 수', ylab = '욕실 수')

Lab.palette <- colorRampPalette(c('white', 'orange', 'red'), space = 'Lab')
smoothScatter(NRoom, Nbath, nbin = 150, colramp = Lab.palette,
              xlab = '방의 수', ylab = '욕실 수')

sunflowerplot(jitter(NRoom), jitter(Nbath), xlab = '방의 수', ylab = '욕실 수')


# Ex2

quartz.save('6.png', type = 'png', device = dev.cur())

pairs(~PricePerM2012 + SubWayDist + MarketDIst + NRoom + Nbath,
      panel = function(...)smoothScatter(..., add = T),gap = 0.5)

detach(HP.Data)

# Ex3

library(scatterplot3d)
quartz.save('8.png', type = 'png', device = dev.cur())
mt.cars <- read.csv('Ch15mtcars.csv', skip = 12, header = T)
attach(mt.cars)

lm.result <- lm(mpg ~ wt + disp)
s3d <- scatterplot3d(mpg ~ wt + disp,
                     highlight.3d = T, type = 'h',
                     pch = 19, xlab = 'weight', ylab = 'gas', zlab = 'km/fuel')
s3d$plane3d(lm.result)
summary(lm.result)

# Ex4

r <- sqrt(mpg/pi)
symbols(wt, disp, mpg, inches = 0.3, fg = 'white',
        bg = 'lightblue', xlab = 'weight', ylab = 'gas')
text(wt, disp, rownames(mt.cars))
detach(mt.cars)

# Ex5

quartz.save('10.png', type = 'png', device = dev.cur())
library(corrgram)
round(cor(mt.cars), 3)

corrgram(mt.cars, order = T,
         lower.panel = panel.ellipse,
         upper.panel = panel.pts,
         diag.panel = panel.minmax)

corrgram(mt.cars, order = T,
         lower.panel = panel.shade,
         upper.panel = panel.pie,
         diag.panel = panel.density)

# Ex6
# GGPLOT : Grammer of Graphics Plot



















