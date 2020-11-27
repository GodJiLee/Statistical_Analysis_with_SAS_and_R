A <- matrix(c(2, 2, -7, 2, 1, 2, 0, 1, -3), ncol = 3)
A

eig <- eigen(A)
eig

e1 <- matrix(c(0.074, -0.224, 0.971))
e1

e2 <- matrix(c(-0.666, -0.333, 0.666))
e2

e3 <- matrix(c(-0.436, 0.218, 0.872))
e3

te1 <- t(e1)
te2 <- t(e2)
te3 <- t(e3)

e1e1 <- te1 %*% e1
e2e2 <- te2 %*% e2
e3e3 <- te3 %*% e3

e1e2 <- te1 %*% e2
e2e3 <- te2 %*% e3
e1e3 <- te1 %*% e3

cos1 <- e1e2 / (sqrt(e1e1) * sqrt(e2e2))
cos1

cos2 <- e2e3 / (sqrt(e2e2) * sqrt(e3e3))
cos2

cos3 <- e1e3 / (sqrt(e1e1) * sqrt(e3e3))
cos3

res <- cos1 + cos2 + cos3
res

library(readxl)
setwd("C://Users//leejiwon//Desktop")
getwd()

pilot <- read_excel("Pilot_201021.xls")
pilot

GSP <- read_excel("GSP_201021.xls")
GSP

mY1 <- colMeans(pilot[1:20, 2:7])
mY2 <- colMeans(pilot[21:40, 2:7])
mY1
mY2

cov(mY1)


covY1 <- cov(pilot[1:20, 2:7])
covY2 <- cov(pilot[21:40, 2:7])
covY1
covY2

Y1Y2 <- mY1 - mY2
tY1Y2 <- t(Y1Y2)

S1S2 <- (covY1 / 20) + (covY2 / 20)
solS1S2 <- solve(S1S2)
solS1S2

tY1Y2 %*% solS1S2 %*% Y1Y2



mY11 <- colMeans(pilot[1:20, 2])
mY11
mY21 <- colMeans(pilot[21:40, 2])
mY21

s11 <- cov(pilot[1:20, 2])
s21 <- cov(pilot[21:40, 2])

Y1 <- pilot[1:20, 2]
(Y1 - mY11) / (s11 / sqrt(20))

Y2 <- pilot[21:40, 2]
#(mY11 - mY21) - / 19

GSP
colnames(GSP) <- c("states" , "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13")


p1 <- GSP[, "x1"] / (GSP[, 2] + GSP[, 3] + GSP[, 4] +GSP[, 5] + GSP[, 6] + GSP[, 7] + GSP[, 8] + GSP[, 9] +GSP[, 10] + GSP[, 11] + GSP[, 12] + GSP[, 13])
p2 <- GSP[, "x2"] / (GSP[, 2] + GSP[, 3] + GSP[, 4] +GSP[, 5] + GSP[, 6] + GSP[, 7] + GSP[, 8] + GSP[, 9] +GSP[, 10] + GSP[, 11] + GSP[, 12] + GSP[, 13])
p3 <- GSP[, "x3"] / (GSP[, 2] + GSP[, 3] + GSP[, 4] +GSP[, 5] + GSP[, 6] + GSP[, 7] + GSP[, 8] + GSP[, 9] +GSP[, 10] + GSP[, 11] + GSP[, 12] + GSP[, 13])
p4 <- GSP[, "x4"] / (GSP[, 2] + GSP[, 3] + GSP[, 4] +GSP[, 5] + GSP[, 6] + GSP[, 7] + GSP[, 8] + GSP[, 9] +GSP[, 10] + GSP[, 11] + GSP[, 12] + GSP[, 13])
p5 <- GSP[, "x5"] / (GSP[, 2] + GSP[, 3] + GSP[, 4] +GSP[, 5] + GSP[, 6] + GSP[, 7] + GSP[, 8] + GSP[, 9] +GSP[, 10] + GSP[, 11] + GSP[, 12] + GSP[, 13])
p6 <- GSP[, "x6"] / (GSP[, 2] + GSP[, 3] + GSP[, 4] +GSP[, 5] + GSP[, 6] + GSP[, 7] + GSP[, 8] + GSP[, 9] +GSP[, 10] + GSP[, 11] + GSP[, 12] + GSP[, 13])
p7 <- GSP[, "x7"] / (GSP[, 2] + GSP[, 3] + GSP[, 4] +GSP[, 5] + GSP[, 6] + GSP[, 7] + GSP[, 8] + GSP[, 9] +GSP[, 10] + GSP[, 11] + GSP[, 12] + GSP[, 13])
p8 <- GSP[, "x8"] / (GSP[, 2] + GSP[, 3] + GSP[, 4] +GSP[, 5] + GSP[, 6] + GSP[, 7] + GSP[, 8] + GSP[, 9] +GSP[, 10] + GSP[, 11] + GSP[, 12] + GSP[, 13])
p9 <- GSP[, "x9"] / (GSP[, 2] + GSP[, 3] + GSP[, 4] +GSP[, 5] + GSP[, 6] + GSP[, 7] + GSP[, 8] + GSP[, 9] +GSP[, 10] + GSP[, 11] + GSP[, 12] + GSP[, 13])
p10 <- GSP[, "x10"] / (GSP[, 2] + GSP[, 3] + GSP[, 4] +GSP[, 5] + GSP[, 6] + GSP[, 7] + GSP[, 8] + GSP[, 9] +GSP[, 10] + GSP[, 11] + GSP[, 12] + GSP[, 13])
p11 <- GSP[, "x11"] / (GSP[, 2] + GSP[, 3] + GSP[, 4] +GSP[, 5] + GSP[, 6] + GSP[, 7] + GSP[, 8] + GSP[, 9] +GSP[, 10] + GSP[, 11] + GSP[, 12] + GSP[, 13])
p12 <- GSP[, "x12"] / (GSP[, 2] + GSP[, 3] + GSP[, 4] +GSP[, 5] + GSP[, 6] + GSP[, 7] + GSP[, 8] + GSP[, 9] +GSP[, 10] + GSP[, 11] + GSP[, 12] + GSP[, 13])
p13 <- GSP[, "x13"] / (GSP[, 2] + GSP[, 3] + GSP[, 4] +GSP[, 5] + GSP[, 6] + GSP[, 7] + GSP[, 8] + GSP[, 9] +GSP[, 10] + GSP[, 11] + GSP[, 12] + GSP[, 13])

res1 <- p1 * 100
res2 <- p2 * 100
res3 <- p3 * 100
res4 <- p4 * 100
res5 <- p6 * 100
res6 <- p6 * 100
res7 <- p7 * 100
res8 <- p8 * 100
res9 <- p9 * 100
res10 <- p10 * 100
res11 <- p11 * 100
res12 <- p12 * 100
res13 <- p13 * 100

resres <- cbind(res1, res2, res3, res4, res5, res6, res7, res8, res9, res10, res11, res12, res13)
det(cor(resres))

cov(resres)
eigen(cov(resres))

range(res11)








