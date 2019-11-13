euc_len <- function(x){
  x2 = x*x
  s <- sum(x2)
  return(sqrt(s))
}

m <- matrix(c(1,2,3,2,5,6,3,6,9),byrow = TRUE,ncol = 3)
eigen(m)

euc_len(eigen(m)$vectors[,3])

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
eigen(B)$vectors
B%*%eigen(B)$vectors[,1]
eigen(B)$values[1]*eigen(B)$vectors[,1]
