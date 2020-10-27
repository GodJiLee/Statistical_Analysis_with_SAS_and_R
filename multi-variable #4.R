A <- matrix(c(-0.875, 0.25, -0.625), ncol = 1)
A

transA <- t(A)
transA

B <- matrix(c(0.125, -0.75, -0.625), ncol = 1)
B

transB <- t(B)
transB

C <- matrix(c(0.125, 0.25, 0.375), ncol = 1)
C 

transC <- t(C)
transC

AA <- A %*% transA
BB <- B %*% transB
CC <- C %*% transC

`10` <- matrix(c(10, 10, 10, 10, 10, 10, 10, 10, 10), ncol = 3)
AA <- `10` %*% AA
AA

`20` <- matrix(c(20, 20, 20, 20, 20, 20, 20, 20, 20), ncol = 3)
BB <- `20` %*% BB
BB

`80` <- matrix(80, ncol = 3, nrow = 3)
`80`
CC <- `50` %*% CC
CC

fin <- AA + BB + CC
`80` %*% fin

getwd()
setwd("C://Users//leejiwon//Desktop")
data <- read.csv("fish.csv")
data






H <- matrix(c(1.053, 2.12, -1.37, -0.76, 2.12, 4.61, -2.34, -1.24, -1.37, -2.34, 2.38, 1.38, -0.76, -1.24, 1.38, 0.81), ncol = 4)
H

E <- matrix(c(13.41, 7.77, 8.67, 5.86, 7.78, 8.1, 7.36, 6.27, 8.68, 7.36, 11.61, 7.04, 5.86, 6.27, 7.04, 10.57), ncol = 4)
E

detE <- det(E)
detHE <- det(H+E)

detE / detHE
