# 20180703
# Sungjae Lee

# Operations in R

c(2, 4) + c(2, 3, 5)
c(2, 4) + c(2, 3, 5, 9)

x <- seq(length.out = 10, to = 19, from = 1)
y <- seq(1, 19, length.out = 10)
z <- seq(1, 19, , 10)

# String Function

nchar('abc')
paste(LETTERS[3:10], 3:10, sep='&')
substr('abcde', 2, 4)

# Numeric Function

rep(3,4)
rep(c(1:3), 4)
rep(c(1:3), c(0, 2, 3))

# Operate Function

abs(-1) 
sqrt(4)
4 ** 0.5

exp(1)
exp(0)

log2(2)

# Handling NA values

natest <- c(1, 2, 3, NA, 4, 5)

sum(natest, na.rm = F)
sum(natest, na.rm = T)

natest[!is.na(natest)]
subset(natest, !is.na(natest))

natest[(natest <= 3)&(!is.na(natest))]

sat <- read.table('desktop/satisfaction.txt', skip = 5, head = T, sep = ',')
sat_Seoul <- sat$S[sat$City == '서울시']
avg_sat_seoul <- sum(sat_Seoul) / length(sat_Seoul)
subset(sat, City == '서울시' & Dept == '컴퓨터과', c(Q1, S, Class))

# Sampling Values

sample(1:5, size = 3, replace = T)

# Matrix Function

diag_matrix <- diag(1, 5, 5)
diag_matrix
diag(diag_matrix)
diag_matrix2 <- diag(c(1:4), 4)
diag_matrix2

solve_matrix <- matrix(c(1:4), 2, 2, byrow = T)
solve(solve_matrix)
t(solve_matrix)

# Loop

for (i in 1:5){
  cat(i); cat(' ')
}

x_rand <- rnorm(10, 3, 4)

m <- 1
for (i in x_rand){
  m <- m * i
}

m <- 1
for (i in 1:10){
  m <- m * x_rand[i]
}

sat_names <- names(sat)
for (j in sat_names){
  print(sat[c(1:5), j])
}

# Conditional

x.if <- 50
if (x.if < 10){xx <-'x is small'
}else if(x.if > 100){print('x is big')
}else {print('x is 50')}

xx<-ifelse(x.if < 10, 'x is small',
           ifelse(x.if > 100, 'x is big',
                  'x is 50'))

sample_x <- sample(1:5000, 10)
m <- 0
for(i in sample_x){
  m <- m + i
  cat('m = ', m, ' , i = ', i, '\n')
  if(i%%2 == 0){
    break
  }  
}

# User Definition Function

myfunction <- function(x){
  for(i in x){
    print(i)
  }
}
myfunction(c(1:10))

Grade <- function(Mid = 0, Final = 0, HW = 0, AT = 0){
  if(any(Mid < 0, Mid > 30)){
    cat('Mid value has to 0 to 30\n')
    return()
  }
  if(any(Final < 0, Final > 40)){
    cat('Final value has to 0 to 40\n')
    return()
  }
  if(any(HW < 0, HW > 20)){
    cat('HW value has to 0 to 20\n')
    return()
  }
  if(any(AT < 0, AT > 10)){
    cat('AT value has to 0 to 10\n')
    return()
  }
  T = Mid + Final + HW + AT
  if (T >= 90){
    G = 'A'
  }else if(T >= 80){
    G = 'B'
  }else if(T >= 80){
    G = 'C'
  }else if(T >= 70){
    G = 'D'
  }else{
    G = 'F'
  }
  grade <- list(Total = T, Grade = G)
  return(grade)
}

result_grade <- Grade(4, 20, 1, 5)
result_grade2 <- Grade(9, 20, 1, 5)
Grade()




