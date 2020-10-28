setwd("C://users//leejiwon//Desktop")
getwd()

library(readxl)
sweat <- read_excel("sweat.xlsx")
is.data.frame(sweat)
sweat

S <- cov(sweat)
S

colMeans(sweat)

library(MASS)
inverse <- solve(S)
inverse

A <- matrix(c(0.64, -2.6, -1.03), ncol = 1)
A

B <- matrix(c(0.64, -2.6, -1.03), ncol = 3)
B

20 %*% B %*% inverse %*% A

consumer <- read_excel("economics.xlsx")
producer <- read_excel("producer.xlsx")

consumer
producer

conS <- cov(consumer)
conS

proS <- cov(producer)
proS

MeanC <- colMeans(consumer)
MeanP <- colMeans(producer)

C <- MeanC - MeanP
C

D <- conS %/% 9
E <- proS %/% 10

F <- D + E
inverse2 <- solve(F)
inverse2

A <- matrix(c(-41.69, 2.17, -6.34, -0.15), ncol = 4)
A

B <- matrix(c(-41.69, 2.17, -6.34, -0.15), ncol = 1)
B

A %*% inverse2 %*% B

pf(q=0.05, df1=8, df2=9, lower.tail = TRUE)



x <- read_excel("x.xlsx")
x

y <- read_excel("y.xlsx")
y

xS <- cov(x)
xS

yS <- cov(y)
yS

Meanx <- colMeans(x)
Meany <- colMeans(y)

C <- Meany - Meanx
C

D <- xS %/% 16
E <- yS %/% 16

F <- D + E
inverse2 <- solve(F)
inverse2

A <- matrix(c(49.5, 106.875), ncol = 2)
A

B <- matrix(c(49.5, 106.875), ncol = 1)
B

A %*% inverse2 %*% B
