
# Leer archivo 
bd <- read.csv("C:/Users/user/Desktop/Universidad/4to semestre/Estad?stica/Proyecto/heart.csv", 
               header= TRUE,sep = "," , quote = "\"", dec = "." , fill = TRUE)
#Leer archivo en Hipathia
bd <- read.csv("/home/hipatia/Desktop/Trabajos-en-R/Estadistica/LLANOS_RUIZ_PROYECTO_FINAL/heart.csv", 
               header= TRUE,sep = "," , quote = "\"", dec = "." , fill = TRUE)

# Ver la tabla 
View(bd)

# Resumen de los atributos de la tabla 
summary(bd)


pairs(bd)

# Tambien sacamos desviacion y varianza para cada atributo
apply(bd,2,sd)
apply(bd,2,var)

# Histogramas 
hist(bd$age,main = "Edad") # si
hist(bd$sex , main = "Sexo")  # no se
hist(bd$cp,main = "Dolor de pecho") # quizas 
hist(bd$trestbps, main = "Presion reposo") # si
hist(bd$chol,main = "Colesterol") # si
hist(bd$fbs,main = "FBS")  # no creo
hist(bd$thalach,main = "Max frecuencia cardiaca") #si
hist(bd$target,main = "Enfermos") # quizas
hist(bd$oldpeak, main = "Depresion ST descanso") # quizas
hist(bd$thal,main = "Tipo de defecto") # no se



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

#---------------------------------------------------------------------------


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
plot(bdEnfermos$oldpeak,bdEnfermos$chol)

#plots no enfermos
noEnfermos <- bd$target == 0;
bdNoEnfermos <- subset(bd,noEnfermos) 
View(bdNoEnfermos)
pairs(bdNoEnfermos)
plot(bdNoEnfermos$oldpeak,bdNoEnfermos$chol)
#----------------------------------------------------
# HISTOGRAMAS ENFERMOS

#Histograma de edad de no enfermos
hist(bdEnfermos$age,main = "Edad enfermos", prob=TRUE) # si
# linea de densidad
lines(density(bdEnfermos$age), col="red",lwd=2)
# linea de densidad suavizada
lines(density(bdEnfermos$age, adjust=2), lty="dotted")

hist(bdEnfermos$sex , main = "Sexo enfermos")  # NO

#hist(bdEnfermos$cp,main = "Dolor de pecho enfermos") # NO - CATEGORICA 

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
lines(density(bdEnfermos$thalach), col="blue",lwd=2)
# linea de densidad suavizada
lines(density(bdEnfermos$thalach, adjust=2), lty="dotted")

#Histograma  de depresion ST en descanso de enfermos
hist(bdEnfermos$oldpeak, main = "Depresion ST descanso enfermos", prob = TRUE) # si - CHI - CUADRADA
#linea de densidad
lines(density(bdEnfermos$oldpeak), col="blue",lwd=2)
# linea de densidad suavizada
lines(density(bdEnfermos$oldpeak, adjust=2), lty="dotted")


hist(bdEnfermos$thal,main = "Tipo de defecto enfermos") # NO



X <- c(rep(65, times=5), rep(25, times=5), rep(35, times=10), rep(45, times=4))
hist(X, prob=TRUE, col="grey")# prob=TRUE for probabilities not counts
lines(density(X), col="blue", lwd=2) # add a density estimate with defaults
lines(density(X, adjust=2), lty="dotted", col="darkgreen", lwd=2) 


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
#0: angina típica
#1: angina atípica
#2: dolor no anginal
#3: asintomático
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
