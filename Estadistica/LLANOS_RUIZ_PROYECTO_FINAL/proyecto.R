
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


# plots enfermos
enfermos <- bd$target == 1
bdEnfermos <- subset(bd,enfermos)
View(bdEnfermos)
pairs(bdEnfermos)
plot(bdEnfermos$oldpeak,bdEnfermos$chol)
#ST depression induced by exercise relative to rest
hist(bdEnfermos$oldpeak)

#slope: slope of the peak exercise ST segment
hist(bdEnfermos$slope)

#ca: numero de vsos sanguineos comprometidos en fluoroscopia
hist(bdEnfermos$ca)

#-----------------------------------------
#plots no enfermos
noEnfermos <- bd$target == 0;
bdNoEnfermos <- subset(bd,noEnfermos) 
View(bdNoEnfermos)
pairs(bdNoEnfermos)
#ST depression induced by exercise relative to rest
hist(bdNoEnfermos$oldpeak)

#slope: slope of the peak exercise ST segment
hist(bdNoEnfermos$slope)

#La pendiente (slope) del segmento ST  de los enfermos  en su mayoria es PLANA (2).
#La pendiente (slope) del segmento ST  de los  NO enfermos  en su mayoria es plana es CRECIENTE (1)

#ca: numero de vsos sanguineos comprometidos en fluoroscopia
hist(bdNoEnfermos$ca)


plot(bdNoEnfermos$oldpeak,bdNoEnfermos$chol)
