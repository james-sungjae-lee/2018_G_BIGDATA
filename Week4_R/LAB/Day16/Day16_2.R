# 20180716
# Sungjae Lee

# Ex1

plant.data <- read.table('Ch10Exam01.txt', skip = 2, header = T)

plant.obs <- table(plant.data)
plant.prop <- c(1/6, 2/6, 3/6)
rbind(plant.obs, plant.prop)

gof <- chisq.test(plant.obs, p = plant.prop)
gof$expected


# Ex2

bp.data <- read.table('Ch10Exam02.txt', skip = 3, header = T)

obs <- with(bp.data, table(child, father))
obs

table.gof <- chisq.test(obs)
table.gof$expected
table.gof

raw.gof <- with(bp.data, chisq.test(child, father))
raw.gof$expected

qchisq(0.95, 4)
qchisq(0.05, 4, lower.tail = F)


# Ex3

obs <- matrix(c(120, 30, 50, 10, 75, 15, 10, 30, 60),
              nrow = 3, byrow = T)
dimnames(obs) <- list(c('20', '30', '40'),
                      LETTERS[1:3])

table.gof <- chisq.test(obs)
table.gof$expected
table.gof


# Ex4

g.data <- read.table('Ch10Exam04.txt', skip = 3, header = T)

raw.gof <- with(g.data, chisq.test(gender, grade))
obs <- with(g.data, table(gender, grade))

table.gof <- chisq.test(obs)
table.gof$expected
table.gof
