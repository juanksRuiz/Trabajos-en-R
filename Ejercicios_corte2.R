#Ejercicio 7.1
z1 <- c(10,5,7,19,11,8)
Z <- cbind(rep(1,length(z1)), z1)

Y <- c(15,9,3,25,7,13)
Bg <- solve(t(Z)%*%Z)%*%t(Z)%*%Y


Yg <- Z%*%Bg
Eps <- Y-Yg
SSE <- t(Eps)%*%Eps

#Ejercicio 7.2
Z <- cbind(c(10,5,7,19,11,18),c(2,3,3,6,7,9))
Y <- c(15,9,3,25,7,13)

Z[,1] <- (Z[,1]-mean(Z[,1]))/sqrt(5*sd(Z[,1]))
Z[,2] <- (Z[,2]-mean(Z[,2]))/sqrt(5*sd(Z[,2]))

Y <- (Y-mean(Y))/sqrt(5*sd(Y))
Bg <- solve(t(Z)%*%Z)%*%t(Z)%*%Y


Yg <- Z%*%Bg
Eps <- Y-Yg

#=================================================================
# Ejercicio 7.9
z1 <- c(-2,-1,0,1,2)
Y1 <- c(5,3,4,2,1)
Y2 <- c(-3,-1,-1,2,3)
Z <- cbind(rep(1,length(z1)),z1)
Y <- cbind(Y1, Y2)

Bg <- solve(t(Z)%*%Z)%*%t(Z)%*%Y
Yg <- Z%*%Bg
Eps <- Y-Yg
t(Eps)%*%Eps


A = t(Y)%*%Y
B = t(Yg)%*%Yg + t(Eps)%*%Eps

A == B

#================================================
# 7.10
# a)
int_conf_E_Y0 <- function(z0, BetaG, alfa, Z, s2){
  n = dim(Z)[1]
  r = dim(Z)[2]
  t = qt((1-alfa)/2, df=n-r-1, lower.tail = FALSE)
  a = t(z0)%*%BetaG - t*sqrt(t(z0)%*%solve(t(Z)%*%Z)%*%z0*s2)
  b = t(z0)%*%BetaG + t*sqrt(t(z0)%*%solve(t(Z)%*%Z)%*%z0*s2)
  i = c(a,b)
  return(i)
}

n = dim(Z)[1]
r = dim(Z)[2]
z0 = c(1,0.5)
S2 = (t(Eps)%*%Eps)/(n-r-1)
z0%*%Bg[,1] # centro intervalo de confianza para Y01
alfa = 0.05
t = qt((1-alfa)/2, df=n-r-1, lower.tail = FALSE)
a = t(z0)%*%Bg[,1] - t*sqrt(t(z0)%*%solve(t(Z)%*%Z)%*%z0*S2[1,1])
b = t(z0)%*%Bg[,1] + t*sqrt(t(z0)%*%solve(t(Z)%*%Z)%*%z0*S2[1,1])
int_EY01 = c(a,b)
int_EY01
