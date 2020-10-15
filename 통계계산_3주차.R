## 최적화

f = function(x)
{
  f = (1 - x[1])^2 + 100*(x[2] - x[1]^2)^2
}

df = function(x)
{
  df1 = -400*x[1]^3 +400*x[1]*x[2] - 2*x[1] - 2  # x1으로 미분
  df2 = 200*(x[2] - x[1]^2) # x2로 미분
  df = c(df1, df2)
  return(df)
}

Norm = function(u)
{
  return(sqrt(sum(u^2)))
}

# 최대하강법
m = 100
par(mfrow=c(1,2), pty="s")
x1 = x2 = seq(-10.5, 10.5, length=m)
xg = expand.grid(x1, x2)
z = matrix(apply(xg, 1, f), m, m)
xh = NULL; fh = NULL
x0 = c(0, 3); fx0 = f(x0); ni = 0
for (i in 1:100)
{  
  xh = rbind(xh, x0); fh = c(fh, fx0); ni = ni+1
  cat("iteration=", round(i,2))
  cat("  x0=", round(x0,2), "  f(x0)=", round(f(x0),3), "\n")
  d = df(x0)
  for (iters in 1:20)
  {
    x = x0 - d; fx = f(x)
    if (fx < fx0) break
    d = d / 2
  }
  x0 = x; fx0 = fx
}
contour(x1, x2, z, levels=round(fh, 2))
for (i in 1:(ni-1))
{
  points(xh[i,1], xh[i,2], pch=as.character(i))
  x1=xh[i,1]; y1=xh[i,2]; x2=xh[i+1,1]; y2=xh[i+1,2]
  arrows(x1, y1, x2, y2, length=0.1, col="red", lwd=0.5)
}
points(xh[ni,1], xh[ni,2], pch=as.character(ni))





## 최적화
f = function(x) # 최적화할 함수
{
  f = (1 - x[1])^2 + 100(y - x^2)^2
}

 df = function(x, y)
{
  df1 = -400*x^3 +400*x*y - 2*x - 2  # x1으로 미분
  df2 = 200*(y - x^2) # x2로 미분
  df = c(df1, df2)
  return(df)
}

Norm = function(u) # 유클리디안 norm 으로 정의
{
  return(sqrt(sum(u^2)))
}

# 최대하강법
m = 20 # 반복수
par(mfrow=c(1,2), pty="s") # 해의 궤적 표시를 위해 2영역으로 분할
x1 = x2 = seq(-10.5, 10.5, length = m) # 100구간으로 분할
xg = expand.grid(x1, x2) # (x1, x2)의 모든 조합 (100*100개) 
z = matrix(apply(xg, 1, f), m, m) # grid 영역에서의 함숫값 행렬을 할당
xh = NULL; fh = NULL # update될 값
x0 = c(0, 3); fx0 = f(x0); ni = 0 # 초깃값 설정 (ni = 반복값)
for (i in 1:100) # 수렴조건 체크 안 한 rough한 설정
{  
  xh = rbind(xh, x0); fh = c(fh, fx0); ni = ni+1 # 궤적 추적 및 ni 업데이트
  cat("iteration=", round(i,2))
  cat("  x0=", round(x0,2), "  f(x0)=", round(f(x0),3), "\n")
  d = df(x0)
  for (iters in 1:20) # step-halving # 적당히 설정 
  {
    x = x0 - d; fx = f(x)
    if (fx < fx0) break # stop 조건 
    d = d / 2 # 1/2로 조정함 # step-halving
  }
  x0 = x; fx0 = fx
} # -2까지 줄어듦 

contour(x1, x2, z, levels=round(fh, 2)) # 해의 궤적 표현  

for (i in 1:(ni-1))
{
  points(xh[i,1], xh[i,2], pch=as.character(i))
  x1=xh[i,1]; y1=xh[i,2]; x2=xh[i+1,1]; y2=xh[i+1,2]
  arrows(x1, y1, x2, y2, length=0.1, col="red", lwd=0.5) # 빨간색 화살표로 궤적 표시
}
points(xh[ni,1], xh[ni,2], pch=as.character(ni))




# 뉴튼 랩슨 알고리즘 # d값 계산만 달라짐
x1 = x2 = seq(-10.5, 10.5, length=m)
xg = expand.grid(x1, x2)
z = matrix(apply(xg, 1, f), m, m)
xh = NULL; fh = NULL
x0 = c(-10, -3); fx0 = f(x0); ni = 0
df2 = matrix(c(2,-1,-1,2),2,2); v = solve(df2) # 이차도함수
for (i in 1:10)
{  
  xh = rbind(xh, as.vector(x0)); fh = c(fh, fx0); ni = ni+1
  cat("iteration=", round(i,2))
  cat("  x0=", round(x0,2), "  f(x0)=", round(f(x0),3), "\n")
  #   d = df(x0)
  d = v %*% df(x0) # 이차도함수인 v를 곱해준다는 점 **
  for (iters in 1:20)
  {
    x = x0 - d; fx = f(x)
    if (fx < fx0) break
    d = d / 2
  }
  if (abs(fx-fx0) < 1e-5) break # stopping rule 구현
  x0 = x; fx0 = fx
} # iterate 2번만에 끝남 # gradient descent보다 훨씬 빠름
contour(x1, x2, z, levels=round(fh, 2))
for (i in 1:(ni-1))
{
  points(xh[i,1], xh[i,2], pch=as.character(i))
  x1=xh[i,1]; y1=xh[i,2]; x2=xh[i+1,1]; y2=xh[i+1,2]
  arrows(x1, y1, x2, y2, length=0.1, col="red", lwd=0.5)
}
points(xh[ni,1], xh[ni,2], pch=as.character(ni))



## 기타 최적화: 선형계획법
library(lpSolve) # 오래된 패키지
eg.lp = lp(objective.in=c(5,8), #공해물질 최적화 문제 # C = 5x1+8x2
           const.mat=matrix(c(1,1,1,2),nrow=2), # constraint matrix화 
           const.rhs=c(2,3), # 200만, 300만 단위
           const.dir=c(">=","="), direction="min") # direction 정의
eg.lp$solution

# degeneracy
degen.lp = lp(objective.in = c(3,1), # 특수한 case; degenerate example
              const.mat = matrix(c(1,1,1,4,1,2,3,1), nrow=4),
              const.rhs = c(2,3,4,4), const.dir = rep(">=",4))
degen.lp
degen.lp$solution	# still finds the optimum

# infeasibility # 특수한 case; 가용 영역에 해가 없는 경우
eg.lp = lp(objective.in=c(5,8),
           const.mat=matrix(c(1,1,1,1),nrow=2),
           const.rhs=c(2,1),
           const.dir=c(">=","<="))
eg.lp

# unboundedness # 특수한 case; status 3 (status 0만 success)
eg.lp = lp(objective.in=c(5,8),
           const.mat=matrix(c(1,1,1,2),nrow=2),
           const.rhs=c(2,3),
           const.dir=c(">=",">="), direction="max")
eg.lp


## 기타 최적화: 이차계획법 # XT*X
library(quadprog) # 역시 오래된 라이브러리 
x = c(.45, .08, -1.08, .92, 1.65, .53, .52, -2.15, -2.20,
      -.32, -1.87, -.16, -.19, -.98, -.20, .67, .08, .38,
      .76, -.78)
y = c(1.26, .58, -1, 1.07, 1.28, -.33, .68, -2.22, -1.82,
      -1.17, -1.54, .35, -.23, -1.53, .16, .91, .22, .44,
      .98, -.98)
X = cbind(rep(1,20), x)
XX = t(X) %*% X
Xy = t(X) %*% y
A = matrix(c(0, 1), ncol = 1)
b = 1
solve.QP(Dmat = XX, dvec = Xy, Amat = A, bvec = b)

# 주식 투자 전략 - 이차 계획법 예제 
# xTBeta(당일 이익) - k/2 BetaTDBeta(변동성 고려) 최적화 (max) 
# beta_i : ratio of investment, nonnegative and sum to 1
# x: daily return for stocks: 0.002, 0.005, 0.01
# D: variability in the returns (covariance) # 수익률 변동성

A = cbind(rep(1,3), diag(rep(1,3)))
D = matrix(c(.01,.002,.002,.002,.01,.002,.002,.002,.01), nrow=3)
x = c(.002,.005,.01) # 수익률
b = c(1,0,0,0)
solve.QP(2*D, x, A, b, meq=1) # 등제약 조건 (meq = 1)
# QP는 min가 기본값 -> max 문제의 부호만 바꿔줌

# optimal strategy: invest 10.4%, 29.2%, 60.4% for stocks 1,2,3
# optimal value is 0.002


## 비선형 함수의 해법

# 이분법
Bisection = function(x0, x1, epsilon = 1e-5)
{
  fx0 = f(x0)
  fx1 = f(x1)
  if (fx0 * fx1 >0)  
    return("wrong initial values")
  error = abs(x1 - x0)
  N = 1
  while (error > epsilon)
  {
    N = N + 1
    error = error / 2
    x2 = (x0 + x1) / 2
    fx2 = f(x2)
    if (fx0 * fx2 < 0)
    {
      x1 = x2; fx1 = fx2
    } else
    {
      x0 = x2; fx0 = fx2
    }
  }
  
  return(list(x = x2, n = N))
}

# 뉴턴법
Newton = function(x0, epsilon = 1e-5, n = 100)
{
  e = 1
  N = 1
  d = epsilon
  while (e > epsilon)
  {
    N = N + 1
    if (N > n) 
      return("not converge after 100 iterations")
    x1 = x0 - f(x0) * d / (f(x0 + d) - f(x0))
    e = abs(x1 - x0)
    x0 = x1
  }
  
  return(list(x = x1, n = N))
}


## 수치적분

# 직사각형법
Integral = function(a, b, n)
{
  integral = 0
  h = (b - a) / n
  for (i in 1:n)
    integral = integral + h * f(a + (i-1/2) * h)
  
  return(integral)
}

# 사다리꼴법
Trapezoid = function(a, b, n = 50)
{
  h = (b - a) / n
  integral = (f(a) + f(b)) / 2
  
  x = a
  n1 = n - 1
  for (i in 1:n1)
  {
    x = x + h
    integral = integral + f(x)
  }
  integral = integral * h
  
  return(integral)
}

# 심슨 적분법
Simpson = function(a, b, n = 12)
{
  h = (b - a) / n
  integral = f(a) + f(b)
  x2 = a
  x3 = a + h
  even = 0
  odd = f(x3)
  h2 = 2 * h
  n1 = n / 2 - 1
  for (i in 1:n1)
  {
    x2 = x2 + h2
    x3 = x3 + h2
    even = even + f(x2)
    odd = odd + f(x3)
  }
  integral = (integral + 4 * odd + 2 * even) * h / 3
  
  return(integral)
}



# 예 3-1
f = function(x) {x^2-3}
result = Bisection(1,2)
result

Newton(1)


# 예 3-2
f = function(x) dnorm(x)
Trapezoid(-1,1)
Simpson(-1,1)
2*(pnorm(1)-0.5)

# 예 3-3

Trapezoid(3,4)
Simpson(3,4)
pnorm(4)-pnorm(3)

Trapezoid(3, 4, n=100)
Simpson(3, 4, n=24)


# 예 3-4
zq = function(p, x0=0, epsilon = 1e-5, n=100) {
  f = function(x) dnorm(x)
  F = function(x){Simpson(-4, x, n=24)-p}
  e=1
  N=1
  while (e > epsilon) {
    N = N+1
    if (N>n) return("not converge after 100 iterations")
    x1 = x0 - F(x0) / f(x0)
    e = abs(x1-x0)
    x0 = x1
  }
  return(list(x1, N))
}

zq(0.9)
qnorm(0.9)
