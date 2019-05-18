
# Leer archivo 
#bd <- read.csv("C:/Users/user/Desktop/Universidad/4to semestre/Estad?stica/Proyecto/heart.csv", 
#               header= TRUE,sep = "," , quote = "\"", dec = "." , fill = TRUE)

#Leer archivo en Hipathia: solo comente esta linea si estÃ¡ en otro pc
#bd <- read.csv("/home/hipatia/Desktop/Trabajos-en-R/Estadistica/LLANOS_RUIZ_PROYECTO_FINAL/heart.csv",
#               header= TRUE,sep = "," , quote = "\"", dec = "." , fill = TRUE)

#Leer archivo pc Ruiz
setwd("Desktop/Trabajos-en-R/Estadistica/LLANOS_RUIZ_PROYECTO_FINAL")
bd <- read.csv("heart.csv",header= TRUE, sep = ",")
# Ver la tabla 
View(bd)
attach(bd)

bd <- subset(bd,bd$chol < 450)
# plots enfermos
bdEnfermos <- subset(bd,bd$target == 1)
View(bdEnfermos)
pairs(bdEnfermos)

#plots no enfermos0
bdNoEnfermos <- subset(bd,bd$target == 0) 
View(bdNoEnfermos)
pairs(bdNoEnfermos)


names(bd)[names(bd) == "ï..age"] <- "age"
names(bdEnfermos)[names(bdEnfermos) == "ï..age"] <- "age"
names(bdNoEnfermos)[names(bdNoEnfermos) == "ï..age"] <- "age"

# Resumen de los atributos de la tabla 
summary(bd)

# Tambien sacamos desviacion y varianza para cada atributo
apply(bd,2,sd)
apply(bd,2,var)

# Histogramas 
hist(bd$age,main = "Edad",xlab = "Edad",ylab = "Frecuencia") # si
hist(bd$sex , main = "Sexo")  # si
hist(bd$cp,main = "Dolor de pecho") # quizas 
hist(bd$trestbps, xlab = "Presion reposo",ylab = "Frecuencia",main = "Presi?n en reposo") # si
hist(bd$chol,main = "Colesterol") # si
hist(bd$thalach,main = "Maxima frecuencia cardiaca",xlab = "Max frecuancia cardiaca",ylab = "Frecuencia") #si
hist(bd$oldpeak, main = "Depresion ST descanso") # quizas
hist(bd$thal,main = "Tipo de defecto") # no se
hist(bd$ca,main = "ca") # no se


# Diagramas de caja 
boxplot(bd$age,main = "Edad") # si
boxplot(bd$trestbps, main = "Presion reposo") # si
boxplot(bd$chol,main = "Colesterol") # si
boxplot(bd$thalach,main = "Max frecuencia cardiaca") #si
boxplot(bd$oldpeak, main = "Depresion ST descanso") # quizas
boxplot(bd$thal,main = "Tipo de defecto") # no se


#con str(bd) se mira la estructura de los datos
# Para datos categoricos con factor

#----------------------------------------------------
# HISTOGRAMAS ENFERMOS

#Histograma de edad de no enfermos
hist(bdEnfermos$age,main = "Edad enfermos", prob=TRUE) # si
# linea de densidad
lines(density(bdEnfermos$age), col="red",lwd=2)
# linea de densidad suavizada
lines(density(bdEnfermos$age, adjust=2), lty="dotted")



#Histograma de presion en reposo
hist(bdEnfermos$trestbps, main = "Presion reposo enfermos", prob=TRUE) # si
# linea de densidad
lines(density(bdEnfermos$trestbps), col="red",lwd=2)
# linea de densidad suavizada
lines(density(bdEnfermos$trestbps, adjust=2), lty="dotted")

#Histograma de colesterol en enfermos
hist(bdEnfermos$chol,main = "Colesterol enfermos", prob= TRUE) # si - CHI - CUADRADA
# linea de densidad
lines(density(bdEnfermos$chol), col="blue",lwd=2)
# linea de densidad suavizada
lines(density(bdEnfermos$chol, adjust=2), lty="dotted")
# Parece normal


#hist(bdEnfermos$fbs,main = "Niveles de azucar en la sangre enfermos") #booleano  # NO

#Histograma maxima frecuencia cardiaca
hist(bdEnfermos$thalach,main = "Max frecuencia cardiaca enfermos", prob = TRUE) #si
# linea de densidad
#lines(density(bdEnfermos$thalach), col="blue",lwd=2)
# linea de densidad suavizada
lines(density(bdEnfermos$thalach, adjust=2),col = "red")

#Histograma  de depresion ST en descanso de enfermos
hist(bdEnfermos$oldpeak, main = "Depresion ST descanso enfermos", prob = TRUE) # si - CHI - CUADRADA
#linea de densidad
lines(density(bdEnfermos$oldpeak), col="blue",lwd=2)
# linea de densidad suavizada
lines(density(bdEnfermos$oldpeak, adjust=2), lty="dotted")



#HISTOGRAMAS NO ENFERMOS
hist(bdEnfermos$age,main = "Edad  NO enfermos") # si
hist(bdEnfermos$trestbps, main = "Presion reposo NO enfermos") # si
hist(bdEnfermos$chol,main = "Colesterol  NO enfermos") # si - CHI - CUADRADA
hist(bdEnfermos$fbs,main = "Niveles de azucar en la sangre NO enfermos") #booleano  # NO
hist(bdEnfermos$thalach,main = "Max frecuencia cardiaca NO enfermos") #si
hist(bdEnfermos$oldpeak, main = "Depresion ST descanso NO enfermos") # si - CHI - CUADRADA



# cp: tipo de dolor
#0: angina tipica
#1: angina atipica
#2: dolor no anginal
#3: asintomatico
# falta
#ST depression induced by exercise relative to rest
hist(bdEnfermos$oldpeak)
hist(bdNoEnfermos$oldpeak)

#slope: slope of the peak exercise ST segment
# AYUDA: https://ecgwaves.com/ecg-normal-p-wave-qrs-complex-st-segment-t-wave-j-point/
#La pendiente (slope) del segmento ST  de los enfermos  en su mayoria es PLANA (2).
#La pendiente (slope) del segmento ST  de los  NO enfermos  en su mayoria es plana es CRECIENTE (1)
hist(bdEnfermos$slope)
hist(bdNoEnfermos$slope)

#ca: numero de vasos sanguineos comprometidos en fluoroscopia
hist(bdEnfermos$ca)
hist(bdNoEnfermos$ca)

#-----------------------------------------



#==========================================================================================================================

#7) Diagramas de dispersion

# trestbps: presion arterial en reposo
# thalach: maxima frecuencia cardiaca

# 1: colesterol en funcion de presion arterial
# Para hombres/Mujeres, angina inducida/no inducida, azucar alto/bajo

#ENFERMOS
#SANOS

#Enfermos:
cor(bdEnfermos$trestbps,bdEnfermos$chol)
plot(bdEnfermos$trestbps,bdEnfermos$chol,
     col = "brown3",
     pch = 20,
     main = "ENFERMOS: Colesterol respecto a la presion arterial en reposo",
     xlab = "Presion arterial en reposo (mm Hg)",
     ylab = "Colesterol mg/dl")         

modelo11 <- lm(bdEnfermos$chol~bdEnfermos$trestbps)
summary(modelo11)
# abline(modelo11) : no pone la linea donde es

# Sanos:
cor(bdNoEnfermos$trestbps,bdNoEnfermos$chol)
plot(bdNoEnfermos$trestbps,bdNoEnfermos$chol,
     col = "brown3",
     pch = 20,
     main = "NO ENFERMOS: Colesterol respecto a la presion arterial en reposo",
     xlab = "Presion arterial en reposo (mm Hg)",
     ylab = "Colesterol mg/dl")         
modelo12 <- lm(bdNoEnfermos$chol~bdNoEnfermos$trestbps)
summary(modelo12)
# abline(modelo12) : no funciona 

# Hombres:
bdHombres <- subset(bd,bd$sex == 1)
cor(bdHombres$trestbps, bdHombres$chol)
modelo13 <- lm(bdHombres$chol~bdHombres$trestbps)
summary(modelo13)
#Mujeres:
bdMujeres <- subset(bd,bd$sex == 0)
cor(bdMujeres$trestbps, bdMujeres$chol)
modelo14 <- lm(bdMujeres$chol~bdMujeres$trestbps)
summary(modelo14)


# a las personas con angina inducida por ejercicio
bdIndAngina <- subset(bd,bd$exang == 1)
cor(bdIndAngina$trestbps, bdIndAngina$chol)
modelo15 <- lm(bdIndAngina$chol~bdIndAngina$trestbps)
summary(modelo15)
# a las personas SIN angina inducida por ejercicio
bdNoIndAngina <- subset(bd,bd$exang == 0)
cor(bdNoIndAngina$trestbps, bdNoIndAngina$chol)
modelo16 <- lm(bdNoIndAngina$chol~bdNoIndAngina$trestbps)
summary(modelo16)


# Con  azucar alto :fbs: (nivel de azucar > 120mg/dl)
bdAltoAzucar <- subset(bd,bd$fbs == 1)
cor(bdAltoAzucar$trestbps, bdAltoAzucar$chol)
modelo17 <- lm(bdAltoAzucar$chol~bdAltoAzucar$trestbps)
summary(modelo17)

# Con  azucar bajo:fbs: (nivel de azucar < 120mg/dl)
bdAzucarNormal <- subset(bd,bd$fbs == 0)
cor(bdAzucarNormal$trestbps, bdAzucarNormal$chol)
modelo18 <- lm(bdAzucarNormal$chol~bdAzucarNormal$trestbps)
summary(modelo18)

 
#3) Frecuencia cardiaca maxima en funcion de la edad
# Enfermos
cor(bdEnfermos$age,bdEnfermos$thalach)
plot(bdEnfermos$age,bdEnfermos$thalach,
     col = "brown3",
     pch = 20,
     main = "ENFERMOS: frecuencia cardiaca máxima respecto a la edad",
     xlab = "Edad",
     ylab = "Frecuencia cardiaca maxima")         

modelo31 <- lm(bdEnfermos$thalach~bdEnfermos$age)
summary(modelo31)
abline(modelo31)

# Sanos:
cor(bdNoEnfermos$age,bdNoEnfermos$thalach)
plot(bdNoEnfermos$age,bdNoEnfermos$thalach,
     col = "brown3",
     pch = 20,
     main = "NO ENFERMOS: frecuencia cardiaca máxima respecto a la edad",
     xlab = "Edad",
     ylab = "Frecuencia cardiaca maxima")         

modelo32 <- lm(bdEnfermos$thalach~bdEnfermos$age)
summary(modelo32)
abline(modelo31)

# (2) frecuencia cardiaca en funcion de la presion arterial
# ENFERMOS
cor(bdEnfermos$thalach,bdEnfermos$trestbps)
plot(bdEnfermos$trestbps,bdEnfermos$thalach,
     col = "brown3",
     pch = 20,
     main = "ENFERMOS: frecuencia cardiaca en funcion de la presion arterial",
     xlab = "Presion Arterial",
     ylab = "Frecuencia cardiaca maxima")         

fit51 <- lm(bdEnfermos$trestbps~bdEnfermos$thalach)
summary(fit51)
abline(fit51)

# SANOS
cor(bdNoEnfermos$thalach,bdNoEnfermos$trestbps)
fit52 <- lm(bdNoEnfermos$trestbps~bdNoEnfermos$thalach)
summary(fit52)

# (3) frecuencia cardiaca en funcion del colesterol
# ENFERMOS
cor(bdEnfermos$chol, bdEnfermos$thalach)
fit61 <- lm(bdEnfermos$chol~bdEnfermos$thalach)
summary(fit61)
# SANOS
cor(bdNoEnfermos$chol, bdNoEnfermos$thalach)
fit62 <- lm(bdNoEnfermos$chol~bdNoEnfermos$thalach)
summary(fit62)

# (4) depresion ST respecto a la presion arterial
# ENFERMOS
cor(bdEnfermos$oldpeak, bdEnfermos$trestbps)
fit71 <- lm(bdEnfermos$trestbps~bdEnfermos$oldpeak)
plot(bdEnfermos$trestbps, bdEnfermos$oldpeak)
summary(fit71)
#SANOS
fit72 <- lm(bdNoEnfermos$trestbps~bdNoEnfermos$oldpeak)
summary(fit72)
plot(bdNoEnfermos$trestbps, bdNoEnfermos$oldpeak)
# (5) depresion ST respecto al  colesterol              (oldpeak)
# ENFERMOS
fit81 <- lm(bdEnfermos$oldpeak~bdEnfermos$chol)
summary(fit81)
plot(bdEnfermos$oldpeak, bdEnfermos$chol)
# SANOS
fit82 <- lm(bdNoEnfermos$oldpeak~bdNoEnfermos$chol)
summary(fit82)
plot(bdNoEnfermos$oldpeak, bdNoEnfermos$chol) # ESTEEE
# (6) presion arterial respecto al colesterol
# ENFERMOS:
fit91 <- lm(bdEnfermos$chol~bdEnfermos$trestbps)
summary(fit91)

#SANOS
fit92 <- lm(bdNoEnfermos$chol~bdNoEnfermos$trestbps)
summary(fit92)



cor(bdEnfermos$thalach,bdEnfermos$chol) # interesante
cor(bdNoEnfermos$thalach,bdNoEnfermos$chol) # interesante




plot(bdEnfermos$thalach,bdEnfermos$chol,
     col = "brown3",
     pch = 20,
     main = "Diagrama de dispersion")  


########################################################################################

# ESTIMACION PARAMETROS RELEVANTES

    # Enfermos 
      enf_age <- sum(enfermos$age) * (1/nrow(enfermos))
      print(enf_age)
      t.test(enfermos$age)
      
      enf_trestbps <- sum(enfermos$trestbps) * (1/nrow(enfermos))
      print(enf_trestbps)
      t.test(enfermos$trestbps)
      
      enf_chol <- sum(enfermos$chol) * (1/nrow(enfermos))
      print(enf_chol)
      t.test(enfermos$chol)
      
      enf_thalach <- sum(enfermos$thalach) * (1/nrow(enfermos))
      print(enf_thalach)
      t.test(enfermos$thalach)
      
    
    # No enfermos
      nenf_age <- sum(noenfermos$age) * (1/nrow(noenfermos))
      print(nenf_age)
      t.test(noenfermos$age)
      
      nenf_trestbps <- sum(noenfermos$trestbps) * (1/nrow(noenfermos))
      print(nenf_trestbps)
      t.test(noenfermos$trestbps)
      
      nenf_chol <- sum(noenfermos$chol) * (1/nrow(noenfermos))
      print(nenf_chol)
      t.test(noenfermos$chol)
      
      nenf_thalach <- sum(noenfermos$thalach) * (1/nrow(noenfermos))
      print(nenf_thalach)
      t.test(noenfermos$thalach)

# PRUEBAS DE HIPOTESIS
 #(1)
      #H0 : enf_chol - nenf_chol  = 0       Est. P : xb - yb
      #Ha : enf_chol - nenf_chol != 0       RR = {} U {}
      
  
      
      
      
    
                                                                  # Entering the data
t.test(enf_chol - nenf_chol,alternative="two.sided",conf.level=0.95)     
      

# MODELOS LINEALES
# DESCARTADAS
# thal
# restecg

#UTILES
# ca: numero de vasos en fluoroscopia comprometidos: a mayor # de vasos peor.
# cp: dolor de pecho:
#0: angina tipica
#1: angina atipica
#2: dolor no anginal
#3: asintomatico

# oldpeak: ST depresion induced by exercise relative to rest






# HIPOTESIS RESPECTO A LAS DISTRIBUCIONES



# CONCLUSIONES




