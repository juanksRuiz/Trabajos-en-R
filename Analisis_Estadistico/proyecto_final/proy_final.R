install.packages("ggplot2")
install.packages("dplyr")
library("ggplot2")
library("dplyr")
df18_1 <- read.csv(file = "C:\\Users\\juank\\Downloads\\BasesDeDatos_proyecto_AED\\Saber_11__2018_1.csv", encoding="UTF-8",header = TRUE)
df18_2 <- read.csv(file = "C:\\Users\\juank\\Downloads\\BasesDeDatos_proyecto_AED\\Saber_11__2018_2.csv", encoding="UTF-8", header = TRUE)
# 67, 70 73, 76,79
View(df18_1)
# 62, 65, 68, 71,74
View(df18_2)

puntaje18_1 <- df18_1[,c(67,70,73,76,79,82)]
puntaje18_2 <- df18_2[,c(62,65,68,71,74,77)]
colnames(puntaje18_2) <- colnames(puntaje18_1)

puntajes <- rbind(puntaje18_1,puntaje18_2)
Xpunt <- data.matrix(puntajes)

View(Xpunt)
#==============================================================================
# Análisis descriptivo de los datos
summary(Xpunt)

# Histogramas de cada una de las competencias evaluadas y del puntaje total

# para el global
ggplot(puntajes, aes(x=puntajes$punt_global)) + geom_histogram(color="black", fill="gray",binwidth = 5)

# para lectura critica
ggplot(puntajes, aes(x=puntajes$punt_lectura_critica)) + geom_histogram(color="black", fill="coral3",binwidth = 2)

# para matemáticas
ggplot(puntajes, aes(x=puntajes$punt_matematicas)) + geom_histogram(color="black", fill="chartreuse4",binwidth = 2)

# para ciencias naturales
ggplot(puntajes, aes(x=puntajes$punt_c_naturales)) + geom_histogram(color="black", fill="dodgerblue3",binwidth = 2)

# para sociales y ciudadanas
ggplot(puntajes, aes(x=puntajes$punt_sociales_ciudadanas)) + geom_histogram(color="black", fill="darkorchid4", binwidth = 3)

#para ingles
ggplot(puntajes, aes(x=puntajes$punt_ingles)) + geom_histogram(color="black", fill="gold", binwidth = 2)

#==============================================================================
# 3) Relacion entre competencias evaluadas: análisis canónico de correlaciones
# Grupo 1: ciencias naturales y matemáticas
# Grupo 2: lectura y escritura, sociales y ciudadanas, inglés
X1 <- Xpunt[,c(2,3)]
X2 <- Xpunt[,c(1,4,5)]

# Hay Na en ingles
na_ingles <- which(is.na(Xpunt[,5]))
XNA <- Xpunt[na_ingles,]
# encontrando los puntajes de ingles faltantes
XNA[,5] <- round((13/5)*XNA[,6] - 3*(XNA[,1] + XNA[,2] + XNA[,3] + XNA[,4]))

count = 1
for (i in which(is.na(Xpunt[,5]))) {
  Xpunt[i,5] <- XNA[count,5]
  count = count + 1
}

X1 <- Xpunt[,c(2,3)]
X2 <- Xpunt[,c(1,4,5)]


sum(is.na(Xpunt[,5]))
# Verificar si se ajusta al puntaje

# Ahora si los datos están completos
s11 <- cov(X1)
s12 <- cov(X1,X2)
s21 <- t(s12)
s22 <- cov(X2)

mat_m12 <- function(m){
  inv_m <- solve(m)
  m_m12 <- (eigen(inv_m)$vectors)%*%diag(sqrt(eigen(inv_m)$values))%*%t(eigen(inv_m)$vectors)
  return(m_m12)
}

M1 <- mat_m12(s11)%*%s12%*%solve(s22)%*%s21%*%mat_m12(s11)
# correlaciones
sqrt(eigen(M1)$values)
e <- eigen(M1)$vectors

M2 <- mat_m12(s22)%*%s21%*%solve(s11)%*%s12%*%mat_m12(s22)
sqrt(eigen(M2)$values) # no dan los mismos valores que las correlaciones de A ?
f <- eigen(M2)$vectors

# los a y B están transpuestos
a1t <- t(e[,1])%*%mat_m12(s11) # *X1 = U1 
b1t <- t(f[,1])%*%mat_m12(s22) # *X2 = V1

a2t <- t(e[,2])%*%mat_m12(s11) # *X1 = U2
b2t <- t(f[,2])%*%mat_m12(s22) # *X2 = V2

# a3 <-  ? 
b3t <- t(f[,3])%*% mat_m12(s22)# *X2 = V3 -----> NO SIRVE

A <- t(cbind(t(a1t),t(a2t)))
B <- t(cbind(t(b1t),t(b2t)))

# Calculamos las correlaciones muestrales entre U o V y X1 o X2
D11_m12 <- mat_m12(diag(diag(s11)))
D22_m12 <- mat_m12(diag(diag(s22)))
# Con U
# U relacionado con grupo 1
R_ux1 <- A%*%s11%*%D11_m12
rownames(R_ux1) <- c("U1","U2")
colnames(R_ux1) <- c("matematicas","c_naturales")

# U relacionado con grupo 2
R_ux2 <- A%*%s12%*%D22_m12
rownames(R_ux2) <- c("U1","U2")
colnames(R_ux2) <- c("lectura_critica","sociales_ciudadanas","ingles")

R_ux2

# Con V
# V relacionado con el grupo 1
R_vx1 <- B%*%s21%*%D11_m12
rownames(R_vx1) <- c("V1","V2")
colnames(R_vx1) <- c("matematicas","c_naturales")

# V relacionado con el grupo 2
R_vx2 <- B%*%s22%*%D22_m12
rownames(R_vx2) <- c("V1","V2")
colnames(R_vx2) <- c("lectura_critica","sociales_ciudadanas","ingles")

R_vx2

#Hacer 2 tablas: una por cada par, y por tabla:
# Par i con grupo 1,  par i con grupo 2
