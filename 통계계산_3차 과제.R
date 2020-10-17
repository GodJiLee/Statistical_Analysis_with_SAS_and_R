# 1-(a)=================
X <- matrix(c(rep(1, 6), c(1, 2, 3, 5, 5, 7, 1, 3, 3, 4, 4, 5)), ncol = 3)
X
X1 = t(X) # 행과 열 전환
X1

H1 <- X1 %*% X
H1 

# Choleski decomposition
H1.chol = chol(H1) # 촐레스키 분해
H1.chol

crossprod(H1.chol, H1.chol) # 원래 행렬로 복원 (H3와 동일)
chol2inv(H1.chol)	# inverse

# H_1 x = b, b = (1,2,3)'
b = 1:3
y = forwardsolve(t(H1.chol), b) 	# forward elimination (lower)
backsolve(H3.chol,y)			# back substitution (upper) # 전방대입법으로 해 구하기

# 촐레스키 분해
choleskyfactorization = function(A){
  n = nrow(A)
  L = matrix(0,nrow=n,ncol=n)
  for (i in (1:n)){
    L[i,i] = A[i,i]
    if (i > 1){
      for (k in (1:(i-1))){
        L[i,i] = L[i,i] - L[i,k]*L[i,k]
      }
    }
    L[i,i] = (L[i,i])^(1/2)
    if (i < n){
      for (j in ((i+1):n)){
        L[j,i] = A[j,i]
        if (i > 1){
          for (k in (1:(i-1))){
            L[j,i] = L[j,i] - L[j,k]*L[i,k]
          }
        }
        L[j,i] = L[j,i]/L[i,i]
      }
    }
  }
  return(L)
}

choleskyfactorization(H1)
#=========================

# 1-(b)===============
# Singular value decomposition
H1.svd = svd(H1)
H1.svd # $d는 s의 대각원소값, u, v 행렬을 얻음
H1.svd$u %*% diag(H1.svd$d) %*% t(H1.svd$v) # U*S*VT = H3 원래 행렬과 같아짐 
H1.svd$v %*% diag(1/H1.svd$d) %*% t(H1.svd$u) # inverse # 일반화 역행렬

EA = eigen(H1, symmetric = T)
EA
# ===================

#1-(c)====================
# QR decomposition
H1.qr = qr(H1)
H1.qr
Q = qr.Q(H1.qr)
Q
R = qr.R(H1.qr)
R # 명시적으로 Q, R을 보기 위한 command
Q %*% R # 원래 matrix로 복원
qr.solve(R) %*% t(Q)	# inverse

qrX = qr(t(X)%*%X)
qrQ=qr.Q(qrX)
y <- matrix(c(2, 4, 5, 8, 8, 9), ncol = 1)
y
tXQ = t(X%*%Q) 
XQY = tXQ%*%y
backsolve(qr.R(qrX), XQY)
? lm

lm(y~X)
# =======================

# 2===================
# solving linear equations # 선형방정식의 해법
A = matrix(c(5, 3, 4, 2, 1, 1, 1, 1, -1), ncol = 3)
A
b = c(9,2,3)
solve(A,b) # vector까지 같이 주어 선형방정식을 풀도록 만듦

Ab <- cbind(A, b)
Ab  
install.packages("arules")
library(arules)

colnames(Ab) <- NULL

# 가우스 소거법(부분 피버팅)
gaussianeliminationpartial = function(Ab){
  n = nrow(Ab)
  for (k in (1:(n-1))){
    pivotindex = k
    for (i in ((k+1):n)){
      if (abs(Ab[i,k]) > abs(Ab[pivotindex,k])){
        pivotindex = i
      }
    }
    if (pivotindex != k){
      for (j in (k:(n+1))){
        buffer = Ab[k,j]
        Ab[k,j] = Ab[pivotindex,j]
        Ab[pivotindex,j] = buffer
      }
    }
    for (i in ((k+1):n)){
      mik = Ab[i,k]/Ab[k,k]
      Ab[i,k] = 0
      for (j in ((k+1):(n+1))){
        Ab[i,j] = Ab[i,j] - mik*Ab[k,j]
      }
    }
  }
  return(Ab)
}

gaussianeliminationpartial(Ab)
# ======================

# 3 ==================
# LU decomposition
set.seed(1)
library(Matrix)
mm = Matrix(c(1, 1, 2, 2, -2, -1, -1, 1, 1), nrow=3) # 정규분포를 따르는 행렬 generate
mm

lum = lu(mm)
str(lum)

elu = expand(lum)
elu # 구체적인 L, U, P 값을 볼 수 있음

with(elu, P %*% L %*% U)

det(mm)

S <- matrix(c(4, 2, 0, 2, 9, 6, 0, 6, 16), ncol = 3)
S

lambda_S <- eigen(S)
lambda_S






