install.packages("ggplot2")
install.packages("dplyr")
library("ggplot2")
library("dplyr")
df18_1 <- read.csv(file = "C:\\Users\\juank\\Downloads\\BasesDeDatos_proyecto_AED\\Saber_11__2018_1.csv", encoding="UTF-8",header = TRUE)
col1 = c(3,17:21,28:35,50,56,60,61,67, 70,73, 76,79,82)
df18_1 <- df18_1[,col1]
colnames(df18_1)
df18_2 <- read.csv(file = "C:\\Users\\juank\\Downloads\\BasesDeDatos_proyecto_AED\\Saber_11__2018_2.csv", encoding="UTF-8", header = TRUE)
colnames(df18_2) <- tolower(colnames(df18_2))
col2 <- c(3,12:16,23:30,45,51,55:56,62, 65, 68, 71,74,77)
df18_2 <- df18_2[,col2]

df18 <- rbind(df18_1,df18_2)

arreglarNAIngles <- function(df){
  print("elementos NA en la columna de ingles:")
  print(sum(is.na(df[,23])))
  # indices de los registros que son NA en ingles
  na_ingles <- which(is.na(df[,23]))
  # obtengamos los puntajes en las otras competencias de los registros con NA en ingles
  dfNA <- df[na_ingles,]
  # encontrando los puntajes de ingles faltantes
  dfNA[,23] <- round((13/5)*dfNA[,24] - 3*(dfNA[,19] + dfNA[,20] + dfNA[,21] + dfNA[,22]))
  
  count = 1
  for (i in which(is.na(df_puntajes[,5]))) {
    df[i,23] <- dfNA[count,23]
    count = count + 1
  }
  print("elementos NA despues:")
  print(sum(is.na(df[,23])))
  return(df)
}
#=================================

df18 <- arreglarNAIngles(df18)


#________________________________________________________________________________
# Separacion de datos por departamento
install.packages("hash")
library(hash)
dic_dept <- hash()

for (dep in levels(df18$cole_depto_ubicacion)) {
  dic_dept[[dep]] = subset(df18,df18$cole_depto_ubicacion == dep)
}

# numero de datos por departamento
for (dep in levels(df18$cole_depto_ubicacion)) {
  print(dep)
  print(nrow(dic_dept[[dep]]))
  print("---------------------------------------")
}

#______________________________________________________________________________________
# organizacion por estrato
dic_estrato <- hash()
for (e in levels(df18$fami_estratovivienda)) {
  dic_estrato[[e]] = subset(df18,df18$fami_estratovivienda == e)
}

#______________________________________________________________________________________
# organizacion (si es  colegio en  zona rural o urbana)
dic_zona <- hash()
for (z in levels(df18$cole_area_ubicacion)) {
  dic_zona[[z]] = subset(df18,df18$cole_area_ubicacion == z)
}

#______________________________________________________________________________________
# organizacion por regiones
regiones <- c("Amazonia","Andina","Caribe","Insular","Orinoquia","Pacifico")
dic_region <- hash()

for (k in keys(dic_dept)) {
    if ((k == "ANTIOQUIA") || (k == "BOYACA") || (k == "CUNDINAMARCA") || (k == "BOGOTA") || (k == "HUILA") || (k == "NORTE SANTANDER") || (k == "QUINDIO") || (k == "RISARALDA") || (k == "SANTANDER") || (k == "TOLIMA")){
      dic_region[["Andina"]] = rbind(dic_region[["Andina"]], dic_dept[[k]])
        #dic_dept[["ANTIOQUIA"]], dic_dept[["BOYACA"]], dic_dept[["CUNDINAMARCA"]],
        #dic_dept[["BOGOTA"]], dic_dept[["HUILA"]],dic_dept[["NORTE SANTANDER"]],
        #dic_dept[["QUINDIO"]],dic_dept[["RISARALDA"]], dic_dept[["SANTANDER"]],
        #dic_dept[["TOLIMA"]])
      
    }else if ((k == "ATLANTICO") || (k == "BOLIVAR") || (k == "CESAR") || (k == "CORDOBA") || (k == "LA GUAJIRA") || (k == "MAGDALENA") || (k == "SAN ANDRES")){
      dic_region[["Caribe"]] = rbind(dic_region[["Caribe"]], dic_dept[[k]])
        # dic_dept[["ATLANTICO"]], dic_dept[["BOLIVAR"]], dic_dept[["CESAR"]],
        # dic_dept[["CORDOBA"]], dic_dept[["LA GUAJIRA"]], dic_dept[["MAGDALENA"]],
        # dic_dept[["SAN ANDRES"]])
    
    }else if((k == "CHOCO") || (k == "VALLE") || (k == "CAUCA") || (k == "NARIÑO")){
      dic_region[["Pacifico"]] = rbind(dic_region[["Pacifico"]], dic_dept[[k]])
        #dic_dept[["CHOCO"]], dic_dept[["VALLE"]], dic_dept[["CAUCA"]], dic_dept[["NARIÑO"]])
    
    }else if ((k == "AMAZONAS") || (k == "CAQUETA") || (k == "GUAINIA") || (k == "GUAVIARE") || (k == "PUTUMAYO") || (k == "VAUPES")){
      dic_region[["Amazonia"]] = rbind(dic_region[["Amazonia"]], dic_dept[[k]])
        #dic_dept[["AMAZONAS"]], dic_dept[["CAQUETA"]], dic_dept[["GUAINIA"]],
        #dic_dept[["GUAVIARE"]], dic_dept[["PUTUMAYO"]], dic_dept[["VAUPES"]] ) 
    
    }else if((k == "META") || (k == "VICHADA") || (k == "CASANARE") || (k == "ARAUCA")){
      dic_region[["Orinoquia"]] = rbind(dic_region[["Orinoquia"]], dic_dept[[k]])
        # dic_dept[["META"]], dic_dept[["VICHADA"]], dic_dept[["CASANARE"]], dic_dept[["ARAUCA"]])
    }
  
}




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


X1 <- Xpunt[,c(2,3)]
X2 <- Xpunt[,c(1,4,5)]


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
cor(Xpunt)
#========================================================================================
#========================================================================================
#========================================================================================

# Regresiones lineales, como responde variable Y con respecto a las otras cuatro variables
#View(Xpunt)
#which(is.na(Xpunt[,5]))
#pairs(Xpunt)
#Xpunt[1:as.integer(nrow(Xpunt)/3),] # reduciendo al primer tercio de datos
#df_Xpunt <- data.frame(Xpunt[,1:5])

# Caso 1: Y: lectura critica,
Ya <- Xpunt[,1]
Za <- Xpunt[,2:5]
Za <- cbind(rep(1,length(Ya)), Za)

Bga <- solve(t(Za)%*%Za)%*%t(Za)%*%Ya
Yga <- Za%*%Bga
Ea <- Ya - Yga


ggplot(df_Xpunt, aes(x=df_Xpunt$punt_lectura_critica, y=df_Xpunt$punt_matematicas)) + geom_point()


# Caso 2: Y: matemáticas
Yb <- Xpunt[,2]
Zb <- Xpunt[,c(1,3,4,5)]
Zb <- cbind(rep(1,length(Yb)), Zb)

Bgb <- solve(t(Zb)%*%Zb)%*%t(Zb)%*%Yb
Ygb <- Zb%*%Bgb
Eb <- Yb - Ygb

# Caso 3: c_naturales
Yc <- Xpunt[,3]
Zc <- Xpunt[,c(1,2,4,5)]
Zc <- cbind(rep(1,length(Yc)), Zc)

Bgc <- solve(t(Zc)%*%Zc)%*%t(Zc)%*%Yc
Ygc <- Zc%*%Bgc
Ec <- Yc - Ygc

# Caso 4: sociales y ciudadanas
Yd <- Xpunt[,4]
Zd <- Xpunt[,c(1,2,3,5)]
Zd <- cbind(rep(1,length(Yd)), Zd)

Bgd <- solve(t(Zd)%*%Zd)%*%t(Zd)%*%Yd
Ygd <- Zd%*%Bgd
Ed <- Yd - Ygd

# Caso 5: ingles
Ye <- Xpunt[,5]
Ze <- Xpunt[,c(1,2,3,4)]
Ze <- cbind(rep(1,length(Ye)), Ze)

Bge <- solve(t(Ze)%*%Ze)%*%t(Ze)%*%Ye
Yge <- Ze%*%Bge
Ee <- Ye - Yge
# _______________________________________________________________________________________
# Para cada region
# puntajes en las competencias son de columna 19 a 23
getPuntajes <- function(df){
  # funcion que retorna matriz con los 5 componentes evaluados
  mat = data.matrix(df[,19:23])
  
  return(mat)
}

arreglarNAIngles(dic_region[["Caribe"]])



for (k in keys(dic_region)) {
  print(k)
  print("Correlacion entre competencias")
  corr <-cor(dic_region[[k]][,19:23])
  colnames(corr) <- c("LC","Math","CN","SC","I")
  rownames(corr) <- c("LC","Math","CN","SC","I")
  print(corr)
  print("============================================================")
  
}




#========================================================================================
#========================================================================================
#========================================================================================
# Análisis de componentes principales
Xcp <- Xpunt[,1:5]
sds <- diag(cov(Xcp))
S <- cov(Xcp)
lam <- eigen(S)$values
e <- eigen(S)$vectors

# proporciones de varianza capturada por las componentes principales
prop <- lam/sum(lam)
corYi_Xk <- function(i,k){
  # correlacion entre la i-esima componente principal y la k-esima variable
  eik = e[k,i]
  #print("eik:")
  #print(e[k,i])
  
  li = lam[i]
  #print("sqrt(li)")
  #print(sqrt(li))
  
  #print("sqrt(Skk)")
  #print(sqrt(S[k,k]))
  return((eik*sqrt(li))/sqrt(S[k,k])) 
}

matCor_Yi_Xk <- c()
for (i in 1:5) {
  # i: indices de PC filas
  for (k in 1:5) {
    # k: indices de las X COLUMNAS
    matCor_Yi_Xk <- c(matCor_Yi_Xk, corYi_Xk(i,k))
  }
}




matCor_Yi_Xk <- matrix(matCor_Yi_Xk,nrow = 5, byrow = TRUE)
colnames(matCor_Yi_Xk) <- colnames(Xcp)
rownames(matCor_Yi_Xk) <- c("Y1","Y2","Y3","Y4","Y5")

#----------------------------------------------------------------------------------------
# PCA guiandose de datacamp
# se hizo PCA con los datos estandarizados
df_Xpunt.pca <- prcomp(df_Xpunt, center = TRUE, scale. = TRUE)
summary(df_Xpunt.pca)

# Tenemos 5 componentes principales
# la 1era componente acumula práctimaente el 80% de la varianza de los datos
# la 2da componente acumula 7% de la varianza de los datos
# la 3ra componente acumula 5,7% de la varianza de los datos
# la 4ta componente acumula 4.1% de la varianza de los datos
# la 5ta componente acumula 3.2% de la varianza de los datos

str(df_Xpunt.pca)
df_Xpunt.pca$rotation

# plotting PCA
install.packages("remotes")
remotes::install_github("vqv/ggbiplot")
library(ggbiplot)

grid::current.viewport()
pca_Plot <- ggbiplot(df_Xpunt.pca)
pca_Plot
