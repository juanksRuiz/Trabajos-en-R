install.packages("mclust")
library("mclust")
# Ejercicio 11.1
X1 <- cbind(c(3,2,4),c(7,4,7))
X2 <- cbind(c(6,5,4),c(9,7,8))

xbar1 <- c(3,6)
xbar2 <- c(5,8)

Spooled <- cbind(c(1,1),c(1,2))

# discriminante: A*x+B
A <- t(xbar1 - xbar2)%*%solve(Spooled)
B <- -0.5*t(xbar1-xbar2)%*%solve(Spooled)%*%(xbar1+xbar2)

# b)
x0 <- c(2,7)
#Discriminante con x0
d1 <- A%*%x0+B #  = 4 > 0

#luego x0 pertenece a pi1

