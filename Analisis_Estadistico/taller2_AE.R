# Ejercicio 3.5
#matriz b

x <- c(3,4,6,-2,3,1)
x <- matrix(x,nrow = 3,byrow = TRUE)
s1 <- matrix(c(32/3,-4/3,-4/3,2/3),nrow = 2,byrow = TRUE)
s2 <- cov(x)

#Ejercicio 3.7
s3 <- matrix(c(5,4,4,5),nrow = 2,byrow = TRUE)
eigval <- eigen(s3)$values
eigvec <- eigen(s3)$vectors

#Ejercicio 3.20
x1 <- c(12.5,14.5,8,9,19.5,8,9,7,7,9,6.5,10.5,10,4.5,7,8.5,6.5,8,3.5,8,17.5,10.5,12,6,13)
x2 <- c(13.7,16.5,17.4,11,23.6,13.2,32.1,12.3,11.8,24.4,18.2,22,32.5,18.7,15.8,15.6,12,12.8,26.1,14.5,42.3,17.5,21.8,10.4,25.6)
m <- cbind(x1,x2)

#a)
summary(x1)
summary(x2)
sumfinal <- summary(x2)-summary(x1)
ma <- mean(x2)-mean(x1)
va <- var(x2)+var(x1)-2*cov(x2,x1)
#b)
d <- x2-x1
mdif <- mean(d)
vdif <- var(d)
