## 비선형 함수의 해법 

# 이분법
Bisection = function(x0, x1, epsilon = 1e-5) # 10^-5로 초기 오차항 설정 
{
  fx0 = f(x0) # 함숫값 설정
  fx1 = f(x1)
  if (fx0 * fx1 >0) # 부호 체크 (음수여야 함)
    return("wrong initial values") # 양수일 때
  error = abs(x1 - x0)
  N = 1 # 반복 수
  while (error > epsilon)
  {
    N = N + 1
    error = error / 2 # 오차를 halving
    x2 = (x0 + x1) / 2 # 중점
    fx2 = f(x2) # 함숫값 계산
    if (fx0 * fx2 < 0) # 부호 체크
    {
      x1 = x2; fx1 = fx2 # 구간 변경
    } else
    { 
      x0 = x2; fx0 = fx2 # 반대편으로 구간 변경
    }
  }
  
  return(list(x = x2, n = N)) # 결과 리턴
}

# 뉴턴법
Newton = function(x0, epsilon = 1e-5, n = 100) # 초기 x0, 오차, 반복수 설정
{
  e = 1
  N = 1
  d = epsilon
  while (e > epsilon) # 오차항과 epsilon 비교
  {
    N = N + 1 
    if (N > n) 
      return("not converge after 100 iterations") # 최대 반복수 stop 조건
    x1 = x0 - f(x0) * d / (f(x0 + d) - f(x0)) # 수치 미분값으로 변경
    e = abs(x1 - x0)
    x0 = x1 # update
  }
  
  return(list(x = x1, n = N)) # 수렴 & 반복 값 결과 출력
}


## 수치적분 (integral)

# 직사각형법
Integral = function(a, b, n) # 양 끝점 및 구간 개수
{
  integral = 0
  h = (b - a) / n # 간격 계산
  for (i in 1:n)
    integral = integral + h * f(a + (i-1/2) * h) # 넓이 (중점 높이로 결정)
  
  return(integral)
}

# 사다리꼴법
Trapezoid = function(a, b, n = 50) # default 구간 수 = 50개
{
  h = (b - a) / n # 간격
  integral = (f(a) + f(b)) / 2 # 적분 초깃값 = 끝점
  
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
  integral = f(a) + f(b) # 초깃값
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
  # 짝수 값은 2번, 홀수 값은 4번 계산됨
  return(integral)
}



# 예 3-1
f = function(x) {x^2-3} # 근읜 +3^1/2, -3^1/2
result = Bisection(1,2) # (1,2)를 초기 구간으로 잡음
result # 18번 만에 10^-5의 오차한계 만족 #이분법

Newton(1) # 뉴튼법으로 계산 시 6번만에 수렴 (더 빠름)


# 예 3-2
f = function(x) dnorm(x)
# (-1, 1) 구간 내에서 적분 
# 사다리꼴법 
# 구간 50개
Trapezoid(-1,1)
# 심슨 적분 
# 구간 12개 
# 더 정확
Simpson(-1,1)
# R에서 수치적분으로 구한 적분값
2*(pnorm(1)-0.5)

# 예 3-3
# (3, 4)로 구간 조정 
# 정규분포 상 확률이 적어 수치적으로 맞추기가 힘듦
Trapezoid(3,4)
Simpson(3,4) # 적은 구간이라도 정밀함
pnorm(4)-pnorm(3)

Trapezoid(3, 4, n=100)
Simpson(3, 4, n=24)


# 예 3-4
zq = function(p, x0=0, epsilon = 1e-5, n=100) { # 백분위수 계산 함수
  f = function(x) dnorm(x) # 뉴튼-랩스로 구현 # f(x) <- dnrom(x)
  F = function(x){Simpson(-4, x, n=24)-p} #F(x)(=cdf) <- simpson으로 
  e=1
  N=1
  while (e > epsilon) {
    N = N+1
    if (N>n) return("not converge after 100 iterations")
    x1 = x0 - F(x0) / f(x0) # 뉴튼-랩스로 수치 적분
    e = abs(x1-x0)
    x0 = x1
  }
  return(list(x1, N))
}

zq(0.9)
qnorm(0.9)
