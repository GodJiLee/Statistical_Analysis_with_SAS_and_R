## E|X_1 - X_2|의 추정, X_1, X_2 ~ N(0,1) 
# 범위에 대한 추정
m <- 1000 # 반복 수
g <- numeric(m)

for (i in 1:m) { 
  x <- rnorm(2) # 정규분포로부터 2개의 sample 생성
  g[i] <- abs(x[1]-x[2]) # 표본의 범위 1000번 반복
}

hist(g, prob = TRUE)
est <- mean(g) #추정값
est


## 절사평균의 MSE 
n <- 20
m <- 1000 # 반복 수 
tmean <- numeric(m) # trimed mean

for (i in 1:m) {
  x <- sort(rnorm(n)) # 오름차순 정렬
  tmean[i] <- sum(x[2:(n-1)])/(n-2) # 절사
}

g <- tmean^2
mse <- mean(g) # 제곱의 평균 값
se <- sqrt(sum((g-mean(g))^2))/m # se of mse
mse
se


# 중앙값의 MSE # median
n <- 20 # 9개씩 버려야 함
m <- 1000
tmean <- numeric(m)

for (i in 1:m) {
  x <- sort(rnorm(n))
  tmean[i] <- median(x)
}

g <- tmean^2
mse <- mean(g)
se <- sqrt(sum((g-mean(g))^2))/m  # se of mse
mse
se

# 오염된 정규분포에서 k차 절사평균의 MSE
# 표준 정규분포와 분산이 큰 정규분포를 mix한 분포 (p로 확률 나눔)
n <- 20
K <- n/2-1 # median
m <- 1000
mse <- matrix(0,n/2,6)

trimmed.mse <- function(n, m, k, p) {
  tmean <- numeric(m) # 절사평균의 mse를 계산함
  for (i in 1:m) {
    sigma <- sample(c(1,10), size = n, # 1~10에서 복원 추출
                    replace = TRUE, prob = c(p,1-p)) # 확률 확인
    x <- sort(rnorm(n, 0, sigma))
    tmean[i] <- sum(x[(k+1):(n-k)])/(n-2*k)
  }
  
  g <- tmean^2
  mse.est <- mean(g)
  se.mse <- sqrt(mean((g-mean(g))^2))/sqrt(m)
  return(c(mse.est, se.mse))
}


for (k in 0:K) {
  mse[k+1, 1:2] <- trimmed.mse(n=n, m=m, k=k, p=1.0)
  mse[k+1, 3:4] <- trimmed.mse(n=n, m=m, k=k, p=.95) # 분산이 큰 값으로 오염
  mse[k+1, 5:6] <- trimmed.mse(n=n, m=m, k=k, p=.9)
}

mse # 오염된 정규분포 상 로버스트 통계량

## N(mu, 1)에서 mu에 대한 평균의 신뢰구간과 신뢰수준
n <- 20
alpha <- .05 # 신뢰도
x <- rnorm(n, mean=3, sd=1) # 분포 설정

UCL <- mean(x)-qt(1-alpha/2,df=n-1)*sqrt(var(x))/sqrt(n) # 상한
LCL <- mean(x)+qt(1-alpha/2,df=n-1)*sqrt(var(x))/sqrt(n) # 하한 # t 검정 공식으로 변환
c(LCL, UCL)

n <- 20
alpha <- .05
CL <- replicate(1000, expr = { # 1000번 반복
  x <- rnorm(n, mean=3, sd=1)
  LCL <- mean(x)-qt(1-alpha/2,df=n-1)*sqrt(var(x))/sqrt(n) # t신뢰 상한
  UCL <- mean(x)+qt(1-alpha/2,df=n-1)*sqrt(var(x))/sqrt(n) # t신뢰 하한
  c(LCL,UCL)
} )

# 상한, 하한에 대한 조건을 걸었을 때
sum(CL[1,] < 3 & CL[2,] > 3) # 1000번 중 몇 번인지 
mean(CL[1,] < 3 & CL[2,] > 3) # 확률 # 반복 수 늘리면 95%에 가까워질 것


## 경험적 1종의 오류율 계산
# 정규분포의 랜덤표본에 대하여 alpha=0.05일 때 H_0: mu=500 vs H_1: mu>500에 대한 t-검정
n <- 20
alpha <- .05
mu0 <- 500 # 이 분포에서 샘플 생성
sigma <- 100
m <- 10000          #number of replicates
p <- numeric(m)     #storage for p-values

for (j in 1:m)
{
  x <- rnorm(n,mu0,sigma)
  ttest <- t.test(x, alternative="greater", mu=mu0)
  p[j] <- ttest$p.value
}

# 가설 검정
p.hat <- mean(p < alpha) # 기각 비율
se.hat <- sqrt(p.hat*(1-p.hat)/m) # 표준편차 추정값
print(c(p.hat,se.hat))


## 경험적 검정력 추정
n <- 20
m <- 1000
mu0 <- 500
sigma <- 100
mu <- c(seq(450, 650, 10)) # alternatives # 범위로 구함 
M <- length(mu)
power <- numeric(M)

for (i in 1:M) {
  mu1 <- mu[i]
  pvalues <- replicate(m, expr={
    
    # simulate under alternative mu1
    x <- rnorm(n, mean=mu1, sd=sigma)
    ttest <- t.test(x,alternative="greater", mu=mu0)
    ttest$p.value} )
  power[i] <- mean(pvalues <= .05)
}


install.packages("Hmisc")
library(Hmisc)   # for errbar

plot(mu, power)
abline(v=mu0, lty=1)
abline(h=.05, lty=1)

# add standard errors
se <- sqrt(power*(1-power)/m)
errbar(mu, power, yplus=power+se, yminus=power-se, xlab=bquote(theta))
lines(mu, power, lty=3)

detach(package:Hmisc)
