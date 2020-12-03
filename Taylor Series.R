install.packages("pracma")
library(pracma)

f <- function(x) {log(x^2+2*x+1)}


t2 <- taylor(f, x0 = 2, n = 2)
t4 <- taylor(f, x0 = 2, n = 4)

plot(f)
par(new = TRUE)
plot(t2, col = "red")
par(new = TRUE)
plot(t4, col = "blue")


f2 <- function(x) {x * exp(-x)}
t2 <- taylor(f2, x0 = 1, n = 2)
t4 <- taylor(f2, x0 = 1, n = 4)

plot(f2)
par(new = TRUE)
plot(t2, col = "red")
par(new = TRUE)
plot(t4, col = "blue")
