euc_len <- function(x){
  x2 = x*x
  s <- sum(x2)
  return(sqrt(s))
}

m <- matrix(c(1,2,3,2,5,6,3,6,9),byrow = TRUE,ncol = 3)
eigen(m)


cov_z <- matrix(c(1,0.4,0.5,0.6,0.4,1,0.3,0.4,0.5,0.3,1,0.2,0.6,0.4,0.2,1),byrow = TRUE,ncol = 4)
r11 <- cov_z[1:2,1:2]
r12 <- cov_z[1:2,3:4]
r21 <- t(r12)
r22 <- cov_z[3:4,3:4]

mat_m12 <- function(m){
  inv_m <- solve(m)
  m_m12 <- (eigen(inv_m)$vectors)%*%diag(sqrt(eigen(inv_m)$values))%*%t(eigen(inv_m)$vectors)
  return(m_m12)
}

r11_m12 <- mat_m12(r11)
r22_m1 <- solve(r22)

B <- r11_m12%*%r12%*%r22_m1%*%r21%*%r11_m12
eigen(B)$values
e1 <- eigen(B)$vectors[,1]

a1 <- mat_m12(r11)%*%e1

b1 <- mat_m12(r22)%*%r21%*%mat_m12(r11)%*%e1
b1
#----------------------------------------------------------
# Ejercicio 10.2
mu <- matrix(c(-3,2,0,1), ncol = 1, byrow=TRUE)
#a) Calcular las relaciones canonicas ro*_1, ro*_2

sig <- matrix(c(8,2,3,1,2,5,-1,3,3,-1,6,-2,1,3,-2,7), ncol = 4,byrow = TRUE)
sig11 <- sig[1:2,1:2]
sig12 <- sig[1:2,3:4]
sig21 <- t(sig12)
sig22 <- sig[3:4,3:4]

# ei: vectores propios de matriz grande del grupo 1
# fi: vectores propios e matriz grande del grupo 2

mat_m12(sig11)
# MAtriz del grupo 1
A <- mat_m12(sig11)%*%sig12%*%solve(sig22)%*%sig21%*%mat_m12(sig11)
e <- eigen(A)$vectors

# ro_1^*2
ro_1 <- sqrt(eigen(A)$values[1])
ro_2 <- sqrt(eigen(A)$values[2])

# Pag 542: doemula de matriz grande para segundo grupo 
B <- mat_m12(sig22)%*%sig21%*%solve(sig11)%*%sig12%*%mat_m12(sig22)
f <- eigen(B)$vectors
 # MATRICES GRANDES DEBEN ESTAR ENTRE -1 Y 1: SON CORRELACIONES

# b) Ui y Vi es el i-esmo par de variables canonicas,
# i: indice del i-esimo vector propio
# Para U1: a1'*X(1)
a1t <-t(e[,1])%*%mat_m12(sig11)
# PAra V1: b1'X(2)
b1t <- t(f[,1])%*%mat_m12(sig22)
