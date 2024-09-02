library(Matrix)

ax <- 1
ay <- 2
az <- 3

a <- matrix(c(ax, ay, az), ncol = 1)

au <- a / sqrt(sum(a^2))

bx <- 4
by <- 5
bz <- 6

b <- matrix(c(bx, by, bz), ncol = 1)

bu <- b / sqrt(sum(b^2))

R <- bu %*% t(au)

c <- R %*% a

cu <- c / sqrt(sum(c^2))

print(bu - cu)

print(sqrt(sum(c^2) - sum(a^2)))

