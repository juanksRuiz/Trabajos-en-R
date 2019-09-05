#ejercicio 1.1 pg. 37
x1 <- c(3,4,2,6,8,2,5)
x2 <- c(5,5.5,4,7,10,5,7.5)
xb1 <- mean(x1)
xb2 <- mean(x2)
s11 <- var(x1)
s22 <- var(x2)
s12 <- cov(x1,x2)

#Ejercicio 1.2
# a)
b1 <- c(1,2,3,3,4,5,6,8,9,11)
b2 <- c(18.95,19,17.95,15.54,14,12.95,8.94,7.49,6,3.99)

plot(b1,b2)

# b)
# s12 es negativo

#c) 
b1bar <- mean(b1)
b2bar <- mean(b2)
cov(b1,b2)
cor(b1,b2)

#d) 
bbar <- c(mean(b1),mean(b2))
b <- cbind(b1,b2)
cov(b)
cor(b)

#Ejercicio 1.3
x1 <- c(9,2,6,5,8)
x2 <- c(12,8,6,4,10)
x3 <- c(3,4,0,2,1)
x <- cbind(x1,x2,x3)
cov(x)
cor(x)

#Ejercicio 1.9
# a)
a1 <- c(-6,-3,-2,1,2,5,6,8)
a2 <- c(-2,-3,1,-1,2,1,5,3)


plot(a1,a2,col='red')

# b)
Rotar <- function(a){
  # Funcion que rota vector de un angulo a en radianes 
  m <- matrix(c(cos(a),sin(a),-sin(a),cos(a)), nrow = 2, byrow = TRUE)
  return(m)
}
a <- cbind(a1,a2)
ar <- a %*%Rotar(26*pi/180)
am1 <- ar[,1]
am2 <- ar[,2]
# c)
s11m <- var(am1)
s22m <- var(am2)

# d)
y <- c(4,-2)
ym <- y%*%Rotar(26*pi/180)

# distancia al origen de 
A <- (ym[1]^2)/s11m
B <- (ym[2]^2)/s22m

