
# Leer archivo 
bd <- read.csv("C:/Users/user/Desktop/Universidad/4to semestre/Estadística/Proyecto/heart.csv", 
               header= TRUE,sep = "," , quote = "\"", dec = "." , fill = TRUE)

# Ver la tabla 
View(bd)

# Resumen de los atributos de la tabla 
summary(bd)

# Tambien sacamos desviacion y varianza para cada atributo
apply(bd,2,sd)
apply(bd,2,var)

# Histogramas 
hist(bd$ï..age,main = "Edad") # si
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
boxplot(bd$ï..age,main = "Edad") # si
boxplot(bd$sex , main = "Sexo")  # no
boxplot(bd$cp,main = "Dolor de pecho") # si
boxplot(bd$trestbps, main = "Presion reposo") # si
boxplot(bd$chol,main = "Colesterol") # si
boxplot(bd$fbs,main = "FBS")  # NO
boxplot(bd$thalach,main = "Max frecuencia cardiaca") #si
boxplot(bd$target,main = "Enfermos") # no
boxplot(bd$oldpeak, main = "Depresion ST descanso") # quizas
boxplot(bd$thal,main = "Tipo de defecto") # no se






