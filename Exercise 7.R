# 통계적 모의 실험 --> 난수 발생(랜덤 샘플링)을 통해 문제 해결
# 몬테칼로 실험: 2차 세계대전 원자폭탄 관련 모의실험 연구에 몬테칼로라는 암호가 붙여진 것에 유래 


# pseudo random number
## R에서의 random number generator 
runif(5) # 5개의 uniform 난수 생성
runif(10, min=-3, max=-1) # -3~-1 사이 10개 생성
set.seed(32789) # 동일한 난수 생성
runif(5)
set.seed(32789) # 재현성 확보 가능
runif(5)

# sample function # 유한 모집단에 대한 복원/비복원 추출
x = sample(1:3, size=10000, replace=T, prob=c(.2, .3, .5)) # 복원 추출 option 선택
table(x) # 1이 나올 확률 0.2, 2는 0.3, 3은 0.5로 복원 추출
# size를 늘릴 수록 이론적 비율과 비슷해짐

# Buffon's needle
Buffon = function(n, lofneedle, distance) # n = 실행 수, lofneedle = 바늘 길이, distance = 총 거리
{
  lofneedle = lofneedle / 2
  distance = distance / 2 # 중점 이용
  r1 = runif(n) # uniform 난수 생성 
  r2 = runif(n)
  prob = mean(r1*distance < lofneedle*sin(r2*pi)) # r1*distance = a (d/2) 
  # 식 그대로 쓴 것 # mean은 상대도수 # probability에 대한 추정값
  return(prob)
}

Buffon(5000,15,20) # 5000번, l = 15, d = 20 # 0.48의 확률로 걸침
