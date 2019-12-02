install.packages("ggplot2")
install.packages("dplyr")
library("ggplot2")
library("dplyr")
#df18_1 <- read.csv(file = "C:\\Users\\juank\\Downloads\\BasesDeDatos_proyecto_AED\\Saber_11__2018_1.csv", encoding="UTF-8",header = TRUE)
# en Hipathia
df18_1 <- read.csv(file = "C:\\Users\\prestamour\\Downloads\\BasesDeDatos_proyecto_AED\\BasesDeDatos_proyecto_AED\\Saber_11__2018_1.csv", encoding="UTF-8", header=TRUE)
col1 = c(3,17:21,28:35,50,56,60,61,67, 70,73, 76,79,82,58,59)
df18_1 <- df18_1[,col1]
colnames(df18_1)
#df18_2 <- read.csv(file = "C:\\Users\\juank\\Downloads\\BasesDeDatos_proyecto_AED\\Saber_11__2018_2.csv", encoding="UTF-8", header = TRUE)
# en Hipathia
df18_2 <- read.csv(file = "C:\\Users\\prestamour\\Downloads\\BasesDeDatos_proyecto_AED\\BasesDeDatos_proyecto_AED\\Saber_11__2018_2.csv", encoding="UTF-8", header=TRUE)
colnames(df18_2) <- tolower(colnames(df18_2))
col2 <- c(3,12:16,23:30,45,51,55:56,62, 65, 68, 71,74,77,53,54)
df18_2 <- df18_2[,col2]

df18 <- rbind(df18_1,df18_2)

arreglarNAIngles <- function(df){
  print("elementos NA en la columna de ingles:")
  print(sum(is.na(df[,23])))
  # indices de los registros que son NA en ingles
  na_ingles <- which(is.na(df[,23]))
  # obtengamos los df18 en las otras competencias de los registros con NA en ingles
  dfNA <- df[na_ingles,]
  # encontrando los df18 de ingles faltantes
  dfNA[,23] <- round((13/5)*dfNA[,24] - 3*(dfNA[,19] + dfNA[,20] + dfNA[,21] + dfNA[,22]))
  
  count = 1
  for (i in na_ingles) {
    df[i,23] <- dfNA[count,23]
    count = count + 1
  }
  print("elementos NA despues:")
  print(sum(is.na(df[,23])))
  return(df)
}
#=================================

df18 <- arreglarNAIngles(df18)


#=================================
# pegando la columna de las regiones
cole_region <- c()
for (i in 1:nrow(df18)) {
  if(df18$cole_depto_ubicacion[i] %in% c("ANTIOQUIA","BOYACA","CUNDINAMARCA","HUILA","NORTE SANTANDER","QUINDIO","RISARALDA","SANTANDER","TOLIMA")) {
    cole_region <- c(cole_region, "Andina")
    
  }else if (df18$cole_depto_ubicacion[i] %in% c("ATLANTICO","BOLIVAR","CESAR","CORDOBA","LA GUAJIRA","MAGDALENA","SAN ANDRES")){
    cole_region <- c(cole_region, "Caribe")
    
  }else if(df18$cole_depto_ubicacion[i] %in% c("CHOCO","VALLE","CAUCA","NARIÃ‘O")){
    cole_region <- c(cole_region, "Pacifico")
    
  }else if (df18$cole_depto_ubicacion[i] %in% c("AMAZONAS","CAQUETA","GUAINIA","GUAVIARE","PUTUMAYO","VAUPES")){
    cole_region <- c(cole_region, "Amazonia")
    
  }else if(df18$cole_depto_ubicacion[i] %in% c("META","VICHADA","CASANARE","ARAUCA")){
    cole_region <- c(cole_region, "Orinoquia")
  }
}
cbind(df18,cole_region)


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

for (k in (keys(dic_dept))) {
    if ((k == "ANTIOQUIA") || (k == "BOYACA") || (k == "CUNDINAMARCA") 
        || (k == "BOGOTA") || (k == "HUILA") || (k == "NORTE SANTANDER") 
        || (k == "QUINDIO") || (k == "RISARALDA") || (k == "SANTANDER") 
        || (k == "TOLIMA")){
      dic_region[["Andina"]] = rbind(dic_region[["Andina"]], dic_dept[[k]])
        
    }else if ((k == "ATLANTICO") || (k == "BOLIVAR") || (k == "CESAR") 
              || (k == "CORDOBA") || (k == "LA GUAJIRA") || (k == "MAGDALENA") 
              || (k == "SAN ANDRES")){
      dic_region[["Caribe"]] = rbind(dic_region[["Caribe"]], dic_dept[[k]])
        
    }else if((k == "CHOCO") || (k == "VALLE") || (k == "CAUCA") || (k == "NARIÃÑO")){
      dic_region[["Pacifico"]] = rbind(dic_region[["Pacifico"]], dic_dept[[k]])
        
    }else if ((k == "AMAZONAS") || (k == "CAQUETA") || (k == "GUAINIA") 
              || (k == "GUAVIARE") || (k == "PUTUMAYO") || (k == "VAUPES")){
      dic_region[["Amazonia"]] = rbind(dic_region[["Amazonia"]], dic_dept[[k]])
        
    }else if((k == "META") || (k == "VICHADA") || (k == "CASANARE") || (k == "ARAUCA")){
      dic_region[["Orinoquia"]] = rbind(dic_region[["Orinoquia"]], dic_dept[[k]])
    }    
    
}





#==============================================================================
# AnÃ¡lisis descriptivo de los datos

# Histogramas de cada una de las competencias evaluadas y del puntaje total

# para el global
ggplot(df18, aes(x=df18$punt_global)) + geom_histogram(color="black", fill="gray",binwidth = 5) + ggtitle("Histograma del puntaje global")

# para lectura critica
ggplot(df18, aes(x=df18$punt_lectura_critica)) + geom_histogram(color="black", fill="coral3",binwidth = 2)+ ggtitle("Histograma del puntaje  en lectura crítica")

# para matemÃ¡ticas
ggplot(df18, aes(x=df18$punt_matematicas)) + geom_histogram(color="black", fill="chartreuse4",binwidth = 2) + ggtitle("Histograma del puntaje en Matemáticas")

# para ciencias naturales
ggplot(df18, aes(x=df18$punt_c_naturales)) + geom_histogram(color="black", fill="dodgerblue3",binwidth = 2)+ ggtitle("Histograma del puntaje  en Ciencias Naturales")

# para sociales y ciudadanas
ggplot(df18, aes(x=df18$punt_sociales_ciudadanas)) + geom_histogram(color="black", fill="darkorchid4", binwidth = 1) + ggtitle("Histograma del puntaje en Sociales y Ciudadanas")

#para ingles
ggplot(df18, aes(x=df18$punt_ingles)) + geom_histogram(color="black", fill="gold", binwidth = 2) + ggtitle("Histograma del puntaje en Ingles")


# Sociales y ciudadanas e Ingles no parecen tener distribucion ormal

#==============================================================================
# 3) Relacion entre competencias evaluadas: anÃ¡lisis canÃ³nico de correlaciones
# Grupo 1: ciencias naturales y matemÃ¡ticas
# Grupo 2: lectura y escritura, sociales y ciudadanas, inglÃ©s
X1 <- Xpunt[,c(2,3)]
X2 <- Xpunt[,c(1,4,5)]



# Verificar si se ajusta al puntaje

# Ahora si los datos estÃ¡n completos
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

# los a y B estÃ¡n transpuestos
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

# Regresiones lineales
# Para cada region

# df18 en las competencias son de columna 19 a 23
getdf18 <- function(df){
  # funcion que retorna matriz con los 5 componentes evaluados
  mat = data.matrix(df[,19:23])
  
  return(mat)
}

arreglarNAIngles(dic_region[["Caribe"]])



for (k in (keys(dic_region))) {
  print(k)
  print("Correlacion entre competencias")
  corr <-cor(dic_region[[k]][,19:23])
  colnames(corr) <- c("LC","Math","CN","SC","I")
  rownames(corr) <- c("LC","Math","CN","SC","I")
  print(corr)
  print("============================================================")
  
}


# 1 grafica de regresion por region: correlacion entre ciencias naturales y matemÃ¡ticas
# Amazonia
linmod1 <- lm(punt_lectura_critica ~ punt_c_naturales, data = dic_region[["Amazonia"]])
summary(linmod1)

ggplot(dic_region[["Amazonia"]], aes(x=punt_lectura_critica, y=punt_c_naturales)) + geom_point() + geom_abline(intercept = linmod1$coefficients[1], slope = linmod1$coefficients[2],col="red" )+ ggtitle("Relacion entre Lectura crÃ­tica y Ciencias naturales en la regiÃ³n Amazonia ")


# Andina
linmod2 <- lm(punt_lectura_critica ~ punt_c_naturales, data = dic_region[["Andina"]])
summary(linmod2)

ggplot(dic_region[["Andina"]], aes(x=punt_lectura_critica, y=punt_c_naturales)) + geom_point() + geom_abline(intercept = linmod2$coefficients[1],
                                                                                                             slope = linmod2$coefficients[2],col="red" )+ ggtitle("Relacion entre Lectura crÃ­tica y Ciencias naturales en la regiÃ³n Andina")

# Caribe
linmod3 <- lm(punt_lectura_critica ~ punt_c_naturales, data = dic_region[["Caribe"]])
summary(linmod3)

ggplot(dic_region[["Caribe"]], aes(x=punt_lectura_critica, y=punt_c_naturales)) + geom_point() + geom_abline(intercept = linmod3$coefficients[1],
                                                                                                             slope = linmod3$coefficients[2],col="red" )+ ggtitle("Relacion entre Lectura crÃ­tica y Ciencias naturales en la regiÃ³n Caribe ")

# Orinoquia
linmod4 <- lm(punt_lectura_critica ~ punt_c_naturales, data = dic_region[["Orinoquia"]])
summary(linmod4)

ggplot(dic_region[["Orinoquia"]], aes(x=punt_lectura_critica, y=punt_c_naturales)) + geom_point() + geom_abline(intercept = linmod4$coefficients[1],
                                                                                                                slope = linmod4$coefficients[2],col="red" )+ ggtitle("Relacion entre Lectura crÃ­tica y Ciencias naturales en la regiÃ³n Orinoquia ")

# Pacifico
linmod5 <- lm(punt_lectura_critica ~ punt_c_naturales, data = dic_region[["Pacifico"]])
summary(linmod5)

ggplot(dic_region[["Pacifico"]], aes(x=punt_lectura_critica, y=punt_c_naturales)) + geom_point() + geom_abline(intercept = linmod5$coefficients[1],
                                                                                                               slope = linmod5$coefficients[2],col="red" )+ ggtitle("Relacion entre Lectura crÃ­tica y Ciencias naturales en la regiÃ³n PacÃ­fico ")



#========================================================================================
#========================================================================================
#========================================================================================
# AnÃ¡lisis de componentes principales

#----------------------------------------------------------------------------------------
# PCA guiandose de datacamp
pca_todo <- prcomp(df18[,19:23], center = TRUE, scale. = TRUE)
pca_todo$rotation
# HACER pca PARA CADA REGIÃ“N
# se hace PCA con los datos estandarizados
pca_Amazonia <- prcomp(dic_region[["Amazonia"]][,19:23], center = TRUE, scale. = TRUE)
summary(pca_Amazonia)

pca_Andina <- prcomp(dic_region[["Andina"]][,19:23], center = TRUE, scale. = TRUE)
summary(pca_Andina)

pca_Caribe <- prcomp(dic_region[["Caribe"]][,19:23], center = TRUE, scale. = TRUE)
summary(pca_Caribe)

pca_Orinoquia <- prcomp(dic_region[["Orinoquia"]][,19:23], center = TRUE, scale. = TRUE)
summary(pca_Orinoquia)

pca_Pacifico <- prcomp(dic_region[["Pacifico"]][,19:23], center = TRUE, scale. = TRUE)
summary(pca_Pacifico)

pca_Amazonia$rotation
pca_Andina$rotation
pca_Caribe$rotation
pca_Orinoquia$rotation
pca_Pacifico$rotation
# plotting PCA
install.packages("remotes")
remotes::install_github("vqv/ggbiplot")
library(ggbiplot)
library(ggfortify)

df2 <- rbind(df18_1,df18_2)


getPercentageofDF <- function(p,df){
  idx <- sample.int(1:nrow(df),as.integer(nrow(df)*p))
  return(df[idx,])
}


# con paquete ggfortify
autoplot(pca_Amazonia, data = dic_region[["Amazonia"]],
         loadings = TRUE,loadings.label = TRUE) + ggtitle("Datos en funcion de PC1 y PC2 en Amazonia")

autoplot(pca_Andina, data = dic_region[["Andina"]],
         loadings = TRUE,loadings.label = TRUE) + ggtitle("Datos en funcion de PC1 y PC2 en region Andina")

autoplot(pca_Caribe, data = dic_region[["Caribe"]],
         loadings = TRUE,loadings.label = TRUE) + ggtitle("Datos en funcion de PC1 y PC2 en Caribe")

autoplot(pca_Orinoquia, data = dic_region[["Orinoquia"]],
         loadings = TRUE,loadings.label = TRUE) + ggtitle("Datos en funcion de PC1 y PC2 en Orinoquia")

autoplot(pca_Pacifico, data = dic_region[["Pacifico"]],
         loadings = TRUE,loadings.label = TRUE) + ggtitle("Datos en funcion de PC1 y PC2 en Pacifico")



#===========================================================================
#===========================================================================
#===========================================================================
# comparacion de medias (ANOVA Y MANOVA)

#primero verificamos la distribucion de los datos de ingles y sociales y ciudadanas
require(graphics)
install.packages("fitdistrplus")
library(fitdistrplus)

# Sólo para la región Andina que cubre la mayoría de la población
# el fit no cuadra para inglés
fit.normAndina <- fitdist(dic_region[["Andina"]]$punt_ingles, distr = "norm", method = "mle")
plot(fit.normAndina)

# el fit no cuadra para sociales y ciuadanas
fit.normAndinaSC <- fitdist(dic_region[["Andina"]]$punt_sociales_ciudadanas, distr = "norm", method = "mle")
plot(fit.normAndinaSC)

# las distribuciones no se ajustan luego no siguen una distribución normal


#__________________________________
# Manova test
# entre departamentos
# Sólo nos quedamos con Lectura crítica, Ciencias Naturales y Matemáticas

# H0: todos los efectos de tratamiento (tao_1, ... ,tao_g) son iguales a 0 Para las 3 variables/competencias
# Las variables independientes son las competencias evaluadas, la dependiente es el departmento donde se ubica el colegio

pLC <- df18$punt_lectura_critica
pM <- df18$punt_matematicas
pCN <- df18$punt_c_naturales
res_manova <- manova(cbind(pLC,pM,pCN) ~ cole_depto_ubicacion, data = df18)

summary(res_manova,test = "Wilks")
#summary.aov(res_manova)

Wilks <- 0.91739
n <- nrow(df18)
# p: número de variables
p <- 3
g <- 33 # numero de pobalciones/grupos

izq <- -(n-1-(p+g)/2)*log(Wilks)
alfa <- 0.05
qc <- qchisq(alfa,df = p*(g-1), lower.tail = FALSE)

izq >qc # TRUE
# por lo tanto los efectos de tratamiento son distintos de 0 es decir si hay una variabilidad de las medias 
# entre cada uno de los departamentows para las 3 competencias evaluadas

# Hagamos la misma prueba para cada region del pais

isTratamientosIguala_0 <- function(w,n,p,g,a){
  iz <- -(n-1-(p+g)/2)*log(w)
  print("iz = ")
  print(iz)
  q <- qchisq(a,df = p*(g-1), lower.tail = FALSE)
  print("qchisq = ")
  print(q)
  return(iz > q)
}

# Para la region Andina

pLC <- dic_region[["Andina"]]$punt_lectura_critica
pM <- dic_region[["Andina"]]$punt_matematicas
pCN <- dic_region[["Andina"]]$punt_c_naturales
res_manova <- manova(cbind(pLC,pM,pCN) ~ dic_region[["Andina"]]$cole_depto_ubicacion, data = dic_region[["Andina"]])
summary(res_manova,test = "Wilks")

w1 <- 0.94414

isTratamientosIguala_0(w1, nrow(dic_region[["Andina"]]), 3,10,alfa) # = TRUE
# los tratamientos son distintos en la region Andina

# Para la region Amazonia
pLC <- dic_region[["Amazonia"]]$punt_lectura_critica
pM <- dic_region[["Amazonia"]]$punt_matematicas
pCN <- dic_region[["Amazonia"]]$punt_c_naturales
res_manova <- manova(cbind(pLC,pM,pCN) ~ dic_region[["Amazonia"]]$cole_depto_ubicacion, data = dic_region[["Amazonia"]])
summary(res_manova,test = "Wilks")

w2 <- 0.94942

isTratamientosIguala_0(w2, nrow(dic_region[["Amazonia"]]),3,6,alfa) # TRUE
# los tratamientos son distintos en la región Amazonia


# Para la region Caribe
pLC <- dic_region[["Caribe"]]$punt_lectura_critica
pM <- dic_region[["Caribe"]]$punt_matematicas
pCN <- dic_region[["Caribe"]]$punt_c_naturales
res_manova <- manova(cbind(pLC,pM,pCN) ~ dic_region[["Caribe"]]$cole_depto_ubicacion, data = dic_region[["Caribe"]])
summary(res_manova,test = "Wilks")

w3 <- 0.96368

isTratamientosIguala_0(w3, nrow(dic_region[["Caribe"]]),3,7,alfa) # = TRUE
# los tratamientos son distintoas ára la región Caribe


# pARA LA REGIÓN PACPIFICO
pLC <- dic_region[["Pacifico"]]$punt_lectura_critica
pM <- dic_region[["Pacifico"]]$punt_matematicas
pCN <- dic_region[["Pacifico"]]$punt_c_naturales
res_manova <- manova(cbind(pLC,pM,pCN) ~ dic_region[["Pacifico"]]$cole_depto_ubicacion, data = dic_region[["Pacifico"]])
summary(res_manova,test = "Wilks")

w4 <- 0.93114

isTratamientosIguala_0(w4, nrow(dic_region[["Pacifico"]]), 3, 4, alfa) # TRUE
# los tratamientos son distintos para la region Pacifico

# Para la region Orinoquia
pLC <- dic_region[["Orinoquia"]]$punt_lectura_critica
pM <- dic_region[["Orinoquia"]]$punt_matematicas
pCN <- dic_region[["Orinoquia"]]$punt_c_naturales
res_manova <- manova(cbind(pLC,pM,pCN) ~ dic_region[["Orinoquia"]]$cole_depto_ubicacion, data = dic_region[["Orinoquia"]])
summary(res_manova,test = "Wilks")

w5 <- 0.98641

isTratamientosIguala_0(w5, nrow(dic_region[["Orinoquia"]]), 3, 4, alfa) # TRUE
# los tratamientos son distintos de 0 para la regioin Orinoquia

# En consecuencia, independientemente de la region, hay diferencias en los tratamientos es decir que  para
# las competencias lectura crítica, matemáticas y Ciencias Naturales se comportan distintamente en cada departamento,
# por sus condiciones económicas, su contexto social 


