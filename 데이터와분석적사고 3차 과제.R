# 문항 1 - (a)
A <- matrix(c(0, 2, -1, 1), ncol = 2)
B <- matrix(c(4, 2, 3, 1), ncol = 2)
B
X <- (4 * B %*% A - 3 * A) / 2
X
2 * X + 3 * A 
4 * B %*% A

# 문항 1 - (b)
A <- matrix(c(-3, 3, -2, 3), ncol = 2)
B <- matrix(c(5, 1, 3, 2), ncol = 2)
tB <- t(B)
C <- matrix(c(0, 1, 0, 1), ncol = 2)
X <- solve(A) %*% C %*% solve(tB)
X

A %*% X %*% tB
C

# 문항 1 - (c)
A <- matrix(c(1, 2, -5, 2), ncol = 2)
B <- matrix(c(3, 1, 1, 3), ncol = 2)
X <- B %*% solve(A)
X
X %*% A
B

# 문항 1 - (d)
A <- matrix(c(3, 2, 1), ncol = 3)
B <- matrix(c(1, 2, 4, 0, 1, 2), ncol = 2)
C <- matrix(c(-1, 1), ncol = 1) 
X <- A %*% B %*% C
X

# 문항 1 - (e)
A <- matrix(c(3, 1, 1, 2, 2, 0, 1, 3, 1), ncol = 3)
B <- matrix(c(1, 0, 1, 0, 1, 0, 0, 1, -1), ncol = 3)
X <- B %*% solve(A)
X %*% A
B
A

# 문항 1 - (f)
A <- matrix(c(3, 1, 1, 2, 2, 0, 1, 3, 1), ncol = 3)
B <- matrix(c(1, 0, 1, 0, 1, 0, 0, 1, -1), ncol = 3)
X <- solve(A) %*% B 
A %*% X 
B

# 문항 2
A <- matrix(c(1, 1, 2, 2, 4, 3), ncol = 3)
B <- matrix(c(1, 2, 3, 1, 2, 4), ncol = 2)
C <- matrix(c(1, 0, 1, -2), ncol = 2)
ABC <- A %*% B %*% C
t(ABC)
tC <- t(C)
tB <- t(B)
tA <- t(A)

tC %*% tB %*% tA


AB <- A %*% B
A
B
C
AB
solve(AB) %*% solve(C)

# 문항 3 - (b) 
A <- matrix(c(1, 1, 1, 3), ncol = 2)
B <- matrix(c(-10, 10), ncol = 1)
X <- -1 * solve(A) %*% B
X
A
B

# 문항 4 - (b)
A <- matrix(c(1/2, 2, 10, 1/3, 2, 1, 1, 1, -1), ncol = 3)
B <- matrix(c(-1, -1, 1), ncol = 1)
X <- -1 * solve(A) %*% B
X

# 문항 5

# 문항 6

# 문항 7






