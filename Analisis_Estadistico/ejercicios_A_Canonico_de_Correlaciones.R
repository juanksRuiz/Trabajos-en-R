euc_len <- function(x){
  x2 = x*x
  s <- sum(x2)
  return(sqrt(s))
}



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

a2t <- t(e[,2])%*%mat_m12(sig11)
b2t <- t(f[,2])%*%mat_m12(sig22)

# c
U <- t(c())
#--------------------------------------------------
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

#Matriz grande
# a) 
mat_peq <- solve(r11)%*%r12%*%solve(r22)%*%r21
eigen(mat_peq)$values

B1 <- r11_m12%*%r12%*%r22_m1%*%r21%*%r11_m12
eigen(B1)$values
# Los valores propios de la matriz pequeña y la grande son iguales

# b)
e <- eigen(B1)$vectors
# U2
a2 <- t(e[,2])%*%mat_m12(r11)
B2 <- mat_m12(r22)%*%r21%*%solve(r11)%*%r12%*%mat_m12(r22)
f <- eigen(B2)$vectors
# V2
b2 <- t(f[,2])%*%mat_m12(r22)

# correlaciones entre (U2,V2)
# Se toma la raiz cuadrada ya que los valores propios son
# las correlaciones al cuadrado

#Confirmamos que la corr(U2,V2) = 0.03
sqrt(eigen(B1)$values)[2]

#---------------------------------------------------
# Ejercicio 10.9
r11 <- matrix(c(1,0.6328,0.6328,1),ncol = 2,byrow = TRUE)
r12 <- matrix(c(0.2412,0.0586,-0.0553,0.0655),ncol=2,byrow = TRUE)
r21 <- t(r12)
r22 <- matrix(c(1,0.4248,0.4248,1),ncol = 2,byrow = TRUE)
R <- rbind(cbind(r11,r12),cbind(r21,r22))

A <- mat_m12(r11)%*%r12%*%solve(r22)%*%r21%*%mat_m12(r11)
# Correlaciones
sqrt(eigen(A)$values)
e <- eigen(A)$vectors

B <- mat_m12(r22)%*%r21%*%solve(r11)%*%r12%*%mat_m12(r22)
f <- eigen(B)$vectors

a1 <- t(e[,1])%*%mat_m12(r11)
b1 <- t(f[,1])%*%mat_m12(r22)

a2 <- t(e[,2])%*%mat_m12(r11)
b2 <- t(f[,2])%*%mat_m12(r22)

# b)
product_test <- function(ro_sq){
  # funcion de productoria para el test
  # ro_sq: correlaciones al cuadrado, vals prop. de una matriz grande 
  p = 1
  for (i in 1:length(ro_sq)) {
    p <- p*(1-ro_sq[i])  
  }
  return(p)
  
}
n <- 140
ch <- qchisq(0.05,df=4,lower.tail = FALSE)
medidor2 <- (-n - 1 - 0.5*(5))*log(product_test(eigen(A)$values)) 
medidor2 > ch
# LA hipótesis nula es rechazada
# Ahora la primera correlación es distinta de 0 y las demás != 0
a <- eigen(A)$values[2]

medidor3 <- (-n - 1 - 0.5*(5))*log(product_test(a))
medidor3 > ch
                                   
