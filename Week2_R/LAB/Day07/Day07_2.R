# 20180701
# Sungjae Lee

X <- 0:2
Px <- c(0.25, 0.5, 0.25)
Fx <- cumsum(Px)
X; Px; Fx;
barplot(Px, names.arg = X)
barplot(Fx, names.arg = X) 

# Integrate

fx <- function(x){2*x - 2}
integrate(fx, lower = 1, upper = 3/2) 

# Comulative Distribution Function

x <- seq(1, 2, length.out = 100)
Fx <- NULL
for(i in 1:100){
  Fx[i] <- integrate(fx, lower = 1, upper = x[i])$value
}
plot(x, Fx)

# Join Probability Function

Pxy <- matrix(c(0, 1/4, 1/4, 1/4, 1/4, 0), ncol = 3, byrow = T)
dimnames(Pxy) <- list(0:1, 0:2)
Py <- margin.table(Pxy, margin = 1)
Px <- margin.table(Pxy, margin = 2)
Pxy; Px; Py

