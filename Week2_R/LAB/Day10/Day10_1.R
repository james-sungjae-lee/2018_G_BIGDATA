# 20180706
# Sungjae Lee

library(foreign)

setwd('/Users/sungjae/desktop')
IH <- read.spss('IH.sav', use.value.labels = T, to.data.frame = T, reencode='euc-kr')

detach('package:foreign', unload=T)
attach(IH)

quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')
layout(matrix(c(1, 0, 0, 2, 0, 0, 3, 4, 5), ncol = 3, byrow = T),
       heights = c(1, 1, 7), widths = c(7, 1, 1))
layout.show(5)

Income.hist <- hist(Income, 
                    breaks = seq(min(Income), max(Income), length.out = 10), 
                    plot = F)
HArea.hist <- hist(HArea,
                   breaks = seq(min(HArea), max(HArea), length.out = 10),
                   plot = F)

top <- max(c(Income.hist$counts, HArea.hist$counts))
Income.range <- c(min(Income), max(Income))
HArea.range <- c(min(HArea), max(HArea))

reg <- lm(HArea~Income)
a <- reg$coefficient[1]
b <- reg$coefficient[2]

par(mar = c(0, 3, 1, 0))
barplot(Income.hist$counts, axes = F, ylim = c(0, top), space = 0)

par(mar = c(0, 5, 0, 0))
box.Income <- boxplot(Income, horizontal = T, axes = F)
text(box.Income$out, box.Income$group,
     labels = round(box.Income$out, 2), pos = 3)

par(mar = c(5, 5, 0, 0))
plot(Income, HArea, xlab = '월 평균 수입',
     ylab = '거주지 면적')

abline(a, b, lty = 4, col = 4)
abline(h = mean(HArea), lty = 11, col = 11)
abline(v = mean(Income), lty = 11, col = 11)
legend(min(Income), max(HArea),
       legend = c('회귀직선', '평균선'),
       lty = c(4, 11), col = c(4, 11), bty = 'n')

par(mar = c(5, 0, 0, 0))
box.HArea <- boxplot(HArea, axes = F)
text(box.HArea$group, box.HArea$out,
     labels = round(box.HArea$out, 2), pos = 3)

par(mar = c(3, 0, 0, 1))
barplot(HArea.hist$counts, axes = F, xlim = c(0, top), space = 0, horiz = T)

detach(IH)

quartz.save(file = '1.png', type = 'png', device = dev.cur())

