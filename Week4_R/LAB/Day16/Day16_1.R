# 20180716
# Sungjae Lee

# Setting

setwd('/Users/sungjae/desktop')

quartz(width = 7, height = 7, bg = 'transparent')
par(family = 'AppleGothic')
quartz.save('4.png', type = 'png', device = dev.cur())

dist.mat <- matrix(c(0, 4, 12, 15, 22, 4, 0, 10, 7, 12, 12, 10,
                     0, 11, 12, 15, 7, 11, 0, 3, 22, 12, 12, 3, 0),
                   nrow = 5, byrow = T)
dist.mat <- as.dist(dist.mat)

# Ex1

result <- hclust(d = dist.mat, method = 'single')
plot(result, hang = -1, main = 'Method : single linkage')

# Ex2

result2 <- hclust(d = dist.mat, method = 'complete')
plot(result2, hang = -1, main = 'Method : complete linkage')

# Ex3

result3 <- hclust(d = dist.mat, method = 'average')
plot(result3, hang = -1, main = 'Method : average linkage')

# Ex4

j.data <- read.table('Ch09Exam04.txt', skip = 5, header = T)

scale.data <- scale(j.data[, 1:4])
dist.mat <- dist(scale.data)
result4 <- hclust(dist.mat, method = 'complete')
plot(result4, hang = -1, main = 'Cluster of jet fighters')

quartz.save('4.png', type = 'png', device = dev.cur())

g <- cutree(result4, k = 3)
j.data <- cbind(j.data, Group = g)

aggregate(j.data[, 1:5], by = list(j.data$G), FUN = mean)
apply(j.data[,1:5], FUN = mean, MARGIN = 2)

# Ex5

k.data <- read.csv('Ch09Exam05.csv', header = T)
result5 <- kmeans(k.data, center = 2)

data.frame(k.data, Cluster = result5$cluster)

# Ex6

iris.data <- read.csv('Ch09Exam06.csv', skip = 6, header = T)

scale.data <- scale(iris.data[, 1:4])
set.seed(7777)
result <- kmeans(scale.data, center = 3)

table(iris.data$Species, result$cluster)



