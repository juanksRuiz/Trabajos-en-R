#Ejercicio 7.1
z1 <- c(10,5,7,19,11,8)
Z <- cbind(rep(1,length(z1)), z1)

Y <- c(15,9,3,25,7,13)
Bg <- solve(t(Z)%*%Z)%*%t(Z)%*%Y


Yg <- Z%*%Bg
Eps <- Y-Yg
SSE <- t(Eps)%*%Eps

#Ejercicio 7.2
Z <- cbind(rep(1,6),c(10,5,7,19,11,18),c(2,3,3,6,7,9))
Y <- c(15,9,3,25,7,13)

Zn1 <- (Z[,1]-mean(Z[,1]))/sqrt(5*sd(Z[,1]))
Zn2 <- (Z[,2]-mean(Z[,2]))/sqrt(5*sd(Z[,2]))
Zn <- cbind(Zn1,Zn2)
#Se normaliza también las salidas con LA MISMA FORMULA
Yn <- (Y-mean(Y))/sqrt(5*sd(Y))
Yn
#Beta gorro con Z normalizada
Bg_n <- solve(t(Zn)%*%Zn)%*%t(Zn)%*%Yn
Hn <- Zn%*%solve(t(Zn)%*%Zn)%*%t(Zn)
Ygn <- Hn%*%Yn
Eps_n <- Yn-Ygn

#Verificando que el modelo ajustado coincide
t(Yn)
t(Zn%*%Bg_n + Eps_n)
Bg_n

#=================================================================
# Ejercicio 7.9
z1 <- c(-2,-1,0,1,2)
Y1 <- c(5,3,4,2,1)
Y2 <- c(-3,-1,-1,2,3)
Y <- cbind(Y1, Y2)

Bg <- solve(t(z1)%*%z1)%*%t(z1)%*%Y
Yg <- z1%*%Bg
Eps <- Y-Yg
Eps%*%t(Eps)
