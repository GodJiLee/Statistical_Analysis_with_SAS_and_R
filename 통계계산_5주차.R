# constructing matrix
H3 = 1/cbind(1:3, 2:4, 3:5) # 벡터 binding, 역수 
matrix(seq(1,12), nrow = 3) # 1~12의 벡터, 행 3개 # byrow = FASLE가 default -> 열단위로 처리
x = seq(1,3)
x2 = x^2
X = cbind(x, x2)
X # column별로 들어감
X = matrix(c(1,2,3,1,4,9), ncol = 2) # 열단위
X

# accessing matrix elements
X[3,2] # 인덱스로 접근
X[3,]
X[,2]
colnames(X) # 열이름
rownames(X) # 행이름
rownames(X) = c("obs1","obs2","obs3")
X

# matrix properties
mat = rep(1:4, rep(3,4)) # 3번씩 4차례 반복 # 벡터 생성
dim(mat) = c(3,4) # demension을 조정
mat
dim(mat)

# diagonal matrix
D = diag(c(1,2,3)) # 대각행렬 # 벡터를 대각원소로
D
diag(D)
I = diag(3) # scala 차원의 identity matrix 생성
I

# triangluar matrix
lower.tri(H3) # 하삼각행렬
Hnew = H3
Hnew[upper.tri(H3)] = 0 # 상삼각행렬 원소를 0으로
Hnew # 해삼각행렬 완성

# transpose
A = matrix(c(rep(1,5), -1, 1, -1), byrow = T, ncol = 4)
A
A1 = t(A) # 행과 열 전환
A1

# matrix arithmetic
Y = 2 * X # 산술 연산
Y
Y + X # element-wise
t(Y) + X # 차원의 불일치
X * Y # element-wise

# matrix multiplication
t(Y) %*% X # 행렬곱 (2*3) * (3*2) = (2*2)
Y %*% X # 차원 불일치
crossprod(Y,X) # YT*X

# determinant and inverse
A = matrix(10:13, 2)
B = matrix(c(3,5,8,9), 2)
det(A)
det(t(A)) # transpose해도 행렬식은 동일
det(A %*% B) 
det(A) * det(B) # 각각에 대한 det과 동일

H3inv = solve(H3) # 역행렬 함수 # 원래는 선형방정식 푸는 함수 (AX+B 형태로 만들어주면 됨)
H3inv
H3inv %*% H3 # 역행렬인지 확인
zapsmall(H3inv %*% H3) # identity # 수치적으로 작은 값은 0으로

# rank
library(fBasics)
set.seed(45)
A = sample(11:26)
A = matrix(A, 4) # random하게 permutation
rk(A) # 역행렬이 존재하는 행렬
B = A
B[1,] = rep(0, ncol(B)) # 1행의 값을 0으로 다 채워넣음
rk(B) # rank = 3
 
# generalized inverse # 일반화 역행렬
library(limSolve)
A = matrix(c(1:8, 6, 8, 10, 12), nrow=4, ncol=3)
B = 0:3
X = Solve(A, B) # 대문자 Solve! # 선형방정식의 해 구하기
A %*% X - B # 해가 맞는지 => 수치적으로 거의 0 (correct!)
(gA = Solve(A)) # 일반화 역행렬값 # 정방행렬이 아닌 행렬에 대한 역행렬 값값

# solving linear equations # 선형방정식의 해법
A = matrix(c(4,2,1,-2,-1,3,3,1,-1), ncol = 3)
A
b = c(9,3,4)
solve(A,b) # vector까지 같이 주어 선형방정식을 풀도록 만듦
