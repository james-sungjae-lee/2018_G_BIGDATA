# 20180701
# Sungjae Lee

for (i in 1:10){
  print(i)
}

plot(1:10)

x1 = c('KIM', 'LEE', 'CHOI', 'DDD')
x2 = c('a', 'b', 'c', 'd')
DF <- data.frame(name=x1, point=x2, row.names = c(95031101, 95031102, 95031103, 95031104))
DF

mytext = read.table('desktop/G_BIG/Week2_Math_R/mytext.txt', sep = '\t', header = T)
mytext

mydata = read.table('desktop/G_BIG/Week2_Math_R/mydata.txt', sep = '\t', header = T)
mydata

avg_data = mean(mydata$height)
avg_data

mydata$weight -> test
test

sex <- c(1, 2, 2, 1, 1, 1, 2, 1) # sex 1 - male / 2 - female
sex.factor <- factor(sex, labels = c('male', 'female'))
sex.factor
rm(sex_factor)

ages <- c(1, 2, 3, 1, 2, 3, 3, 2, 1, 1)
ages.ordered <- ordered(ages, levels = c(1, 2, 3, 4), labels = c('10th', '20th', '30th' ,'40th'))
age.table <- table(ages.ordered)
ages.as.char <- as.character(ages.ordered)

ages.ordered
age.table
ages.as.char

# Create Matrix and Rename

x <- c(1, 2, 3, 4, 5, 6)
x.matrix <- matrix(data = x, ncol = 3, byrow = F)
x.matrix
x.matrix2 <- matrix(data = x, nrow = 3, byrow = T)
x.matrix2
x.matrix3 <- matrix(data = x, nrow = 2, ncol = 2, byrow = T)
x.matrix3
rownames(x.matrix3) <- c("R1", "R2")
colnames(x.matrix3) <- c("C1", "C2")
x.matrix3

x1 <- c(1, 2)
x2 <- c(3, 4)
x12.matrix <- cbind(x1, x2)
x12.matrix2 <- rbind(x1, x2)
x12.matrix
x12.matrix2

# Create Array and Rename

Arr <- array(1:24, dim = c(3, 4, 2))
Arr

dimnames(Arr) <- list(c("Row1", "Row2", "Row3"), 
                      c("Col1", "Col2", "Col3", "Col4"), 
                      c("Layer1", 'Layer2'))
Arr

# Create Dataframe

dfCol1 <- c('Kim', 'Lee', 'Cho', 'Choi')
dfCol2 <- c(75, 95, 29, 20)
DF1 <- data.frame(name = dfCol1, point = dfCol2, row.names = (c("1234", "1235", "1236" ,"1237")))
DF1

# Create List

x.list <- list(c(1:10), c('A', 'B', 'C'), DF1)
x.list

names(x.list) <- c('numbers', 'chars', 'dataframe')
x.list

# Search List Components

x.list[1]
x.list$chars
x.list$dataframe[1, 1]
x.list$dataframe$name[1]
x.list$dataframe$name[-1]

# Handling Vector

x <- seq(0, 20, length = 9)
x[4]
x[-4]
x[2:4]
x[-2:-4]
length(x)
x[2:length(x)]
x[seq(1, 7, by = 2)]
x[c(2, 6, 1)]
x.index = c(6, 2, 4, 9)
x[x.index]

# Handling Matrix

M <- matrix(seq(1, 100, by = 9), ncol = 4, byrow = T)
M
M[,2]
M[2,]
M[, 4]
M[-1, 4]
M[1, c(-2, -4)]
M[1, -c(2, 4)]

# Handling List

x.vec <- c('Kim', 'Lee', 'Cho')
x.mat <- M
x.list2 <- list(age = c(4, 2, 5), data = x.mat, name = x.vec)
x.list2

x.list2$name[2]
x.list2[[2]][3, 2]
x.list2[[2]][3,2]

# Handling Dataframe

DF1
DF1[ ,2]
DF1$name
DF1$point
DF1['point']
DF1[c('point', 'name')]

DF1[2,]
DF1['1235',]

# Attach Function

attach(DF1)
name
point
table(name)
table(point)
detach(DF1)

# With function

with(DF1, table(name))

with(DF1, {print(table(sex))
  print(summary(point))})

# Check Datatype

is.list(x.list2)

as.factor(x.list2[[1]])
as.ordered(x.list2[[1]])
x.list2[[1]]
is.ordered(x.list2[[1]])
is.factor(x.list2[[1]])

typeof(x.list2[[1]])

# Get Data from Files

Table.Data1 = read.table('desktop/G_BIG/Week2_Math_R/LAB1/data_tab.txt', 
                         sep = '\t', col.names = c('height', 'weight'), 
                         skip = 5)
Table.Data1

Table.Data2 = read.table('desktop/G_BIG/Week2_Math_R/LAB1/data_space.txt',
                         sep = ' ', col.names = c('height', 'weight'),
                         skip = 5, nrow = 2)

Table.Data3 = read.csv('desktop/G_BIG/Week2_Math_R/LAB1/data_csv.csv')
Table.Data3

# Object Information

str(x.list)
ls()









