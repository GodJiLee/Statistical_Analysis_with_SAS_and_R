# 문항 1) 
.2 == .3-.1
isTRUE(all.equal(.2, .3-.1))

# 문항 2)

## f(x) 시각화 function 생성 

evaluationfunctionc <- function(xmin, xmax, n) {
  x <- c(0)
  f <- c(0)
  for (i in (0:n)){
    x[i+1] <- xmin + i*(xmax-xmin)/n
    f[i+1] <- sin(x[i+1])/x[i+1]
  }
  plot(x, f, type = "l", col = "blue", xlab = "x", ylab = "function")
}


# -10~ 10까지 100개의 구간으로 나누어 시각화 
evaluationfunctionc(-10, 10, 100)

## 0 근방을 확대해서 관찰
evaluationfunctionc(-10^-20, 10^-20, 100)


evaluatefunction2withcheck = function(xmin,xmax,n,epsilon){
  x = c(0)
  f = c(0)
  for (i in (0:n)){
    x[i+1] = xmin + i*(xmax-xmin)/n
    if (abs(x[i+1]) > epsilon){
      f[i+1] = sin(x[i+1])/x[i+1]
    }
    else{
      f[i+1] = 1 # 여기에서 
    }
  }
  plot(x,f,type="l",col="blue",xlab="x",ylab="function")
}

## 수치적 문제 해결되었음 
evaluatefunction2withcheck(-10^-20, 10^-20, 100, 10^-30)



# 문항 3) 

# 피보나치 수열 계산 알고리즘
## 재귀 프로그램

fiborecursive <- function(i) {
  if (i <= 2){
    value <- 1
  }
  else {
    return(fiborecursive(i-1) + fiborecursive(i-2))
  }
}



## 반복 프로그램
fiboiterative <- function(i){
  if (i <= 2){
    value <- 1
  }
  else{
    value1 <- 1
    value2 <- 1
    for (j in 3:i){
      value <- value1 + value2
      value1 <- value2
      value2 <- value
    }
  }
}

system.time(fiboiterative(10))
system.time(fiborecursive(10))

system.time(fiborecursive(20))
system.time(fiboiterative(20))

system.time(fiborecursive(30))
system.time(fiboiterative(30))

system.time(fiborecursive(40))
system.time(fiboiterative(40))



