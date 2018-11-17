rm(list=ls())

x <- rnorm(100000,10,16)
y <- rnorm(100000,5,9)
t <- -50000:49999
z <- x+y

#Datos teoricos
#Ejercicio 1 (Ver hoja teórica)
p <- seq(from=0.01,by=0.01, length.out = 99)
teo <- rnorm(100000,15,25)
qTeo <- quantile(teo,p)
qz <- quantile(z,p)

dif <- abs(qTeo-qz)
Error <- 0
for(i in dif){
  Error <- Error + i
}
ErrorP <- Error/length(dif)
minimo <- min(dif)
maximo <- max(dif)

#Ejercicio 2
p3 <- seq(from=0.01,by=0.01, length.out = 99)
x3 <- rgamma(100000,1,2)
y3 <- rgamma(100000,5,2)
z3 <- x3+y3


teo3 <- rgamma(100000,6,2)
qTeo3 <- quantile(teo3,p3)
qz3 <- quantile(z3,p3)


dif3 <- abs(qTeo3-qz3)
Error3 <- 0
for(i in dif3){
  Error3 <- Error3 + i
}
ErrorP3 <- Error3/length(dif3)
minimo3 <- min(dif3)
maximo3 <- max(dif3)



#Ejercicio 3
pX <- 0.2
qN <- 0.1
N <- rgeom(100000,qN)
z2 <- c()
s <- 0
for (i in N){
  v <- rgeom(i,pX)
  s <- sum(v)
  z2 <- c(z2,s)
  
}
p <- seq(from=0.01,by=0.01, length.out = 99)
teo2 <- rgeom(100000,pX*qN)
qTeo2 <- quantile(teo2,p)
qz2 <- quantile(z2,p)

dif2 <- abs(qTeo2-qz2)
Error <- 0
for(i in dif2){
  Error <- Error + i
}
ErrorP2 <- Error/length(dif2)
minimo2 <- min(dif2)
maximo2 <- max(dif2)
