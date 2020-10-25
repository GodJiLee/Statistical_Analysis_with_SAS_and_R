# Singular value decomposition
H3.svd = svd(H3)
H3.svd # $d는 s의 대각원소값, u, v 행렬을 얻음
H3.svd$u %*% diag(H3.svd$d) %*% t(H3.svd$v) # U*S*VT = H3 원래 행렬과 같아짐 
H3.svd$v %*% diag(1/H3.svd$d) %*% t(H3.svd$u) # inverse # 일반화 역행렬

# Choleski decomposition
H3.chol = chol(H3) # 촐레스키 분해
H3.chol
crossprod(H3.chol, H3.chol) # 원래 행렬로 복원 (H3와 동일)
chol2inv(H3.chol)	# inverse
# H_3 x = b, b = (1,2,3)'
b = 1:3
y = forwardsolve(t(H3.chol), b) 	# forward elimination (lower)
backsolve(H3.chol,y)			# back substitution (upper) # 전방대입법으로 해 구하기

# QR decomposition
H3.qr = qr(H3)
H3.qr
Q = qr.Q(H3.qr)
Q
R = qr.R(H3.qr)
R # 명시적으로 Q, R을 보기 위한 command
Q %*% R # 원래 matrix로 복원
qr.solve(R) %*% t(Q)	# inverse

# LU decomposition
set.seed(1)
library(Matrix)
mm = Matrix(round(rnorm(9),2), nrow=3) # 정규분포를 따르는 행렬 generate
mm

lum = lu(mm)
str(lum)

elu = expand(lum)
elu # 구체적인 L, U, P 값을 볼 수 있음

with(elu, P %*% L %*% U)

# spectral decomposition # 분광분해
A = matrix(c(5,25,35,25,155,175,35,175,325), ncol = 3)
A
EA = eigen(A, symmetric = T) # 고유값, 고유벡터
EA
det(A) # 고유값들의 합
prod(EA$values)

# rank: SV and eigenvalue
rk(A) # 계수 계산 # full column rank # 역행렬 존재
A.svd = svd(A)
sum(A.svd$d != 0)
sum(EA$values != 0)

# inner and outer product
x1 = 1:5
outer(x1, x1, "/") # 나눗셈 외적 #1/1, 1/2, 1/3, 1/4,....
outer(x1, x1, "-") # 뺄셈 외적
y = 5:10
outer(x1, y, "+") # 덧셈 외적

# real symmetric and positive definite matrix
A1 = matrix(c(4, 345, 193, 297), 2) # (2*2)
eigen(A1)$value # 양수 1개, 음수 1개 (양정치는 아님)
library(matrixcalc)
is.positive.definite(A1) # A1 자체가 symetric matrix가 아님
A2 = (A1 + t(A1)) / 2 # symetric matrix화
eigen(A2)$value
is.positive.definite(A2)

## 해법
# 전방 대입법 
forwardsub = function(L,b){
  x = c(0)
  n = nrow(L)
  for (i in (1:n)){
    x[i] = b[i]
    if (i > 1){
      for (j in (1:(i-1))){
        x[i] = x[i] - L[i,j]*x[j]
      }
    }
    x[i] = x[i]/L[i,i]
  }
  return(cbind(x))
}

# 후방 대입법
backwardsub = function(U,b){
  x = c(0)
  n = nrow(U)
  for (i in (n:1)){
    x[i] = b[i]
    if (i < n){
      for (j in ((i+1):n)){
        x[i] = x[i] - U[i,j]*x[j]
      }
    }
    x[i] = x[i]/U[i,i]
  }
  return(cbind(x))
}

# 가우스 소거법
gaussianelimination = function(Ab){
  n = nrow(Ab)
  for (k in (1:(n-1))){
    for (i in ((k+1):n)){
      mik = Ab[i,k]/Ab[k,k]
      Ab[i,k]=0
      for (j in ((k+1):(n+1))){
        Ab[i,j] = Ab[i,j] - mik*Ab[k,j]
      }
    }
  }
  return(Ab)
}

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

# LU 분해 
lufactorization = function(A){
  n = nrow(A)
  L = matrix(0,nrow=n,ncol=n)
  for (k in (1:(n-1))){
    for (i in ((k+1):n)){
      L[i,k] = A[i,k]/A[k,k]
      A[i,k] = 0
      for (j in ((k+1):n)){
        A[i,j] = A[i,j] - L[i,k]*A[k,j]
      }
    }
  }
  for (k in (1:n)) L[k,k] = 1
  return(cbind(L,A))
}

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

# 가우스 조던 소거법
gaussjordanelimination = function(Ab){
  n = nrow(Ab)
  for (k in (1:n)){
    if (k > 1){
      for (i in (1:(k-1))){
        mik = Ab[i,k]/Ab[k,k]
        Ab[i,k] = 0
        for (j in ((k+1):(n+1))){
          Ab[i,j] = Ab[i,j] - mik*Ab[k,j]
        }
      }
    }
    if (k < n){
      for (i in ((k+1):n)){
        mik = Ab[i,k]/Ab[k,k]
        Ab[i,k] = 0
        for (j in ((k+1):(n+1))){
          Ab[i,j] = Ab[i,j] - mik*Ab[k,j]
        }
      }			
    }
  }
  return(Ab)
}

