#a)
MediaMuestral <- function(n){
  X <-  rexp(n,0.1)
  s <- 0
  for (i in 1:n) {
    s <- s + X[i]
  }
  Mn <- s*(1.0/n)
  return(Mn)
  
}
VectorErrores <- function(k,n,promTeo){
  #Retorna lista con k elementos de los errores relativos
  Err <- vector('numeric',k)
  for(i in 1:k){
    Mn <- MediaMuestral(n)
    Err[i] = abs(Mn-promTeo)
  }
  return(Err)
}

Mn <- MediaMuestral(10)



#Promedio de la exponencial
Ex <- 1/0.1
derr <- abs(Mn-Ex)/Ex

Errores <- vector('numeric',100)
for (i in 1:length(Errores)) {
  Mn <- MediaMuestral(10)
  Errores[i] <- abs(Mn-Ex)/Ex
}

qa <- quantile(Errores,seq(0.9,1,0.01))

#Percentil 95
p95 <- qa[6]
hist(Errores)

#b.1) 
Errores2 <- VectorErrores(100,1000,10)
qb1 <- quantile(Errores2,seq(0.95,0.96,0.01))
pb1 <- qb1[1]
hist(Errores2)

#b.2)
Errores3 <- VectorErrores(100,10000,10)
qb2 <- quantile(Errores3,seq(0.95,0.96,0.01))
pb2 <- qb2[1]
hist(Errores3)

#Respuesta comparando histogramas:
#Observe que entre más grande sea n (la cantidad de muestras) menor es el error relativo

#c)
Zn <- function(n,promTeo,sd){
  #promTeo y sd son de X
  Mn = MediaMuestral(n)
  Zn <- (Mn - promTeo)/(sd/(n**0.5))
  return(Zn)
}

Comparar <- function(n,I){
  #Retorna vector de diferencias entre percentil 60 y el percentil teorico 95 
  dif <- vector('numeric',I)
  for( i in 1:I){
    Errores <- VectorErrores(1000,n,0)
    qz <- quantile(ErroresZ,seq(0.6,0.7,0.1))
    p60 = qz[1]
    N <- rnorm(n,0,1)
    qN <- quantile(N,seq(0.95,0.96,0.001))
    p95 <- qN[1]
    dif[i] = abs(p60-p95)/p95
    
  }
  return(dif)
}

# I= 1
#a)
dif1 <- Comparar(10,1)

#b)
#n = 10
dif2 <- Comparar(10,100)
hist(dif2)
#n = 1000
dif3 <- Comparar(1000,100)
hist(dif3)
#n = 10000
dif4 <- Comparar(10000,100)
hist(dif4)

# Se estandarizó la variable X para luego comparar su percentil 60 con el 
# percentil 95(teorico) de una distribucion normal estandar
# Se observa que con n = 10, la aproximación de Zn al percentil teorico es muy malo. 
# Sin embargo, con n = 1000,10000 el los errores teóricos parecen tener una distribucion normal
# con promedio al rededor de 5,35.



