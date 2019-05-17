
# Leer archivo 
#bd <- read.csv("C:/Users/user/Desktop/Universidad/4to semestre/Estad?stica/Proyecto/heart.csv", 
#               header= TRUE,sep = "," , quote = "\"", dec = "." , fill = TRUE)

#Leer archivo en Hipathia: solo comente esta linea si estÃ¡ en otro pc
#bd <- read.csv("/home/hipatia/Desktop/Trabajos-en-R/Estadistica/LLANOS_RUIZ_PROYECTO_FINAL/heart.csv",
#               header= TRUE,sep = "," , quote = "\"", dec = "." , fill = TRUE)

#Leer archivo pc Ruiz
#setwd("\\Desktop\\Trabajos-en-R\\Estadistica\\LLANOS_RUIZ_PROYECTO_FINAL\\")
bd <- read.csv("heart.csv",header= TRUE, sep = ",")
# Ver la tabla 
View(bd)
attach(bd)
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
hist(bd$sex , main = "Sexo")  # no se
hist(bd$cp,main = "Dolor de pecho") # quizas 
hist(bd$trestbps, xlab = "Presion reposo",ylab = "Frecuencia",main = "Presi?n en reposo") # si
hist(bd$chol,main = "Colesterol") # si
hist(bd$fbs,main = "FBS")  # no creo
hist(bd$thalach,main = "Maxima frecuencia cardiaca",xlab = "Max frecuancia cardiaca",ylab = "Frecuencia") #si
hist(bd$target,main = "Enfermos") # quizas
hist(bd$oldpeak, main = "Depresion ST descanso") # quizas
hist(bd$thal,main = "Tipo de defecto") # no se
hist(bd$ca,main = "ca") # no se


# Diagramas de caja 
boxplot(bd$age,main = "Edad") # si
boxplot(bd$sex , main = "Sexo")  # no
boxplot(bd$cp,main = "Dolor de pecho") # si
boxplot(bd$trestbps, main = "Presion reposo") # si
boxplot(bd$chol,main = "Colesterol") # si
boxplot(bd$fbs,main = "FBS")  # NO
boxplot(bd$thalach,main = "Max frecuencia cardiaca") #si
boxplot(bd$target,main = "Enfermos") # no
boxplot(bd$oldpeak, main = "Depresion ST descanso") # quizas
boxplot(bd$thal,main = "Tipo de defecto") # no se

# LIMPIAR LA BASE DE DATOS
#sexo
# sexVector <- vector(mode = "logical", length= length(bd$sex))
# for (i in c(1:length(sexVector))) {
#   if(bd$sex[i] == 1){
#     # hombre
#     sexVector[i] = 'M'
#   }else{
#     sexVector[i] = 'F'
#   }
# }

#cp: tipos de dolores de pecho
# chestPainVector <- vector(mode ="character",length = length(bd$cp))
# for (i in c(1:length(chestPainVector))) {
#   chestPainVector[i] = bd$cp[i]
# }
# factorChestPainVector <- factor(chestPainVector)


#con str(bd) se mira la estructura de los datos
# Para datos categoricos con factor

# plots enfermos
enfermos <- bd$target == 1
bdEnfermos <- subset(bd,enfermos)
View(bdEnfermos)
pairs(bdEnfermos)

#plots no enfermos
noEnfermos <- bd$target == 0;
bdNoEnfermos <- subset(bd,noEnfermos) 
View(bdNoEnfermos)
pairs(bdNoEnfermos)
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
lines(density(bdEnfermos$thalach, adjust=2),,col = "red")

#Histograma  de depresion ST en descanso de enfermos
hist(bdEnfermos$oldpeak, main = "Depresion ST descanso enfermos", prob = TRUE) # si - CHI - CUADRADA
#linea de densidad
lines(density(bdEnfermos$oldpeak), col="blue",lwd=2)
# linea de densidad suavizada
lines(density(bdEnfermos$oldpeak, adjust=2), lty="dotted")



#HISTOGRAMAS NO ENFERMOS
hist(bdEnfermos$age,main = "Edad  NO enfermos") # si
hist(bdEnfermos$sex , main = "Sexo  NO enfermos")  # NO
hist(bdEnfermos$cp,main = "Dolor de pecho NO enfermos") # NO - CATEGORICA 
hist(bdEnfermos$trestbps, main = "Presion reposo NO enfermos") # si
hist(bdEnfermos$chol,main = "Colesterol  NO enfermos") # si - CHI - CUADRADA
hist(bdEnfermos$fbs,main = "Niveles de azucar en la sangre NO enfermos") #booleano  # NO
hist(bdEnfermos$thalach,main = "Max frecuencia cardiaca NO enfermos") #si
hist(bdEnfermos$oldpeak, main = "Depresion ST descanso NO enfermos") # si - CHI - CUADRADA
hist(bdEnfermos$thal,main = "Tipo de defecto NO enfermos") # NO



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

#1: colesterol en funcion de presion arterial
cor(trestbps,chol)
plot(trestbps,chol,
     col = "brown3",
     pch = 20,
     main = "Colesterol respecto a la presion arterial en reposo",
     xlab = "Presion arterial en reposo (mm Hg)",
     ylab = "Colesterol mg/dl")         

modelo <- lm(chol~trestbps)
summary(modelo)
abline(modelo)

#2 : presion arterial en reposo en funcion de maxima frecuencia cardiaca
cor(thalach,trestbps)
plot(thalach,trestbps,
     col = "brown3",
     pch = 20,
     main = "Presion arterial en reposo respcto a maxima frecuencia cardiaca",
     xlab = "Maxima frecuencia cardiaca (pul)/min",
     ylab = "Presion arterial en reposo (mm Hg)"
     )         

modelo1 <- lm(trestbps~thalach)
summary(modelo1)
abline(modelo1)

#3 
cor(?..age,thalach)
plot(?..age,thalach,
     col = "brown3",
     pch = 20,
     main = "Diagrama de dispersi?n",
     xlab = "Edad",
     ylab = "Frecuencia cardiaca maxima")         

modelo2 <- lm(thalach~bd$age)
summary(modelo2)
abline(modelo2)


cor(restecg,thalach)
cor(exang,thalach)
cor(thal,thalach)
cor(thal,chol)
cor(thalach,age)
cor(age,cp)
cor(thalach,chol)

pairs(bd)

sum(target)

enfermos <- subset(bd,target == 1)
View(enfermos)
summary(enfermos)

noenfermos <- subset(bd,target == 0)
View(noenfermos)
summary(noenfermos)


apply(enfermos,2,sd)

apply(enfermos,2,var)

apply(noenfermos,2,sd)
apply(noenfermos,2,var)

hist(enfermos$cp,main = "Dolor de pecho")
hist(noenfermos$cp,main = "Dolor de pecho")

plot(enfermos$thalach,enfermos$chol,
     col = "brown3",
     pch = 20,
     main = "Diagrama de dispersion")  
modelo <- l

pairs(enfermos, pch = 18)
pairs(noenfermos, pch = 18)

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
#3: asintomÃ¡tico

# oldpeak: ST depresion induced by exercise relative to rest


# (1) colesterol en funcion de presion arterial
#   En general para enfermos y sanos

#ENFERMOS
fit11 <- lm(bdEnfermos$trestbps~bdEnfermos$chol, data = bdEnfermos)
plot(bd$trestbps,bd$chol)
#abline(fit11)
summary(fit11)
#SANOS
fit12 <- lm(bdNoEnfermos$trestbps~bdNoEnfermos$chol, data = bdNoEnfermos)

#   para hombres
bdHombres <- subset(bd,bd$sex==1)
fit21 <- lm(bdHombres$trestbps~bdHombres$chol)
summary(fit21)
#   para mujeres
bdMujeres <- subset(bd,bd$sex==0)
fit22 <- lm(bdMujeres$trestbps~bdMujeres$chol)
summary(fit22)


#   a los que tuvieron angina inducida por ejercicio
bdIndAngina <- subset(bd, bd$exang == 1)
fit31 <- lm(bdIndAngina$trestbps~bdIndAngina$chol)
summary(fit31)

#los que no tuvieron angina
bdNoAngina <- subset(bd, bd$exang == 0)
fit32 <- lm(bdNoAngina$trestbps~bdNoAngina$chol)
summary(fit32)


#   Con  azucar alto :fbs: (nivel de azucar > 120mg/dl)
bdAzucarAlto <- subset(bd,bd$fbs == 1)
fit41 <- lm(bdAzucarAlto$trestbps~bdAzucarAlto$chol)
summary(fit41)

# Con  azucar bajo:fbs: (nivel de azucar > 120mg/dl)
bdAzucarBajo <- subset(bd,bd$fbs == 0)
fit42 <- lm(bdAzucarBajo$trestbps~bdAzucarBajo$chol)
summary(fit41)

# (2) frecuencia cardiaca en funcion de la presion arterial (sobra  ?)
# ENFERMOS
fit51 <- lm(bdEnfermos$trestbps~bdEnfermos$thalach)
summary(fit51)

# SANOS
fit52 <- lm(bdNoEnfermos$trestbps~bdNoEnfermos$thalach)
summary(fit52)

# (3) frecuencia cardiaca en funcion del colesterol
# ENFERMOS
fit61 <- lm(bdEnfermos$chol~bdEnfermos$thalach)
summary(fit61)
# SANOS
fit62 <- lm(bdNoEnfermos$chol~bdNoEnfermos$thalach)
summary(fit62)

# (4) depresion ST respecto a la presion arterial
# ENFERMOS
fit71 <- lm(bdEnfermos$trestbps~bdEnfermos$oldpeak)
summary(fit71)
#SANOS
fit72 <- lm(bdNoEnfermos$trestbps~bdNoEnfermos$oldpeak)
summary(fit72)

# (5) depresion ST respecto al colesterol               (oldpeak)
# ENFERMOS
fit81 <- lm(bdEnfermos$chol~bdEnfermos$oldpeak)
summary(fit81)

# SANOS
fit82 <- lm(bdNoEnfermos$chol~bdNoEnfermos$oldpeak)
summary(fit82)

# (6) presion arterial respecto al colesterol
# ENFERMOS:
fit91 <- lm(bdEnfermos$chol~bdEnfermos$trestbps)
summary(fit91)

#SANOS


# HIPOTESIS RESPECTO A LAS DISTRIBUCIONES



# CONCLUSIONES




