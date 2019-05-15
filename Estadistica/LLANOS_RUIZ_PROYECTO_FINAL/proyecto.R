
# Leer archivo 
bd <- read.csv("C:/Users/user/Desktop/Universidad/4to semestre/Estadística/Proyecto/heart.csv", 
               header= TRUE,sep = "," , quote = "\"", dec = "." , fill = TRUE)

# Ver la tabla 
View(bd)
attach(bd)

# Resumen de los atributos de la tabla 
summary(bd)

# Tambien sacamos desviacion y varianza para cada atributo
apply(bd,2,sd)
apply(bd,2,var)

# Histogramas 
hist(bd$ï..age,main = "Edad",xlab = "Edad",ylab = "Frecuencia") # si
hist(bd$sex , main = "Sexo")  # no se
hist(bd$cp,main = "Dolor de pecho") # quizas 
hist(bd$trestbps, xlab = "Presion reposo",ylab = "Frecuencia",main = "Presión en reposo") # si
hist(bd$chol,main = "Colesterol") # si
hist(bd$fbs,main = "FBS")  # no creo
hist(bd$thalach,main = "Maxima frecuencia cardiaca",xlab = "Max frecuancia cardiaca",ylab = "Frecuencia") #si
hist(bd$target,main = "Enfermos") # quizas
hist(bd$oldpeak, main = "Depresion ST descanso") # quizas
hist(bd$thal,main = "Tipo de defecto") # no se
hist(bd$ca,main = "ca") # no se


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

#7) Diagramas de dispersion
#1
cor(trestbps,chol)
plot(trestbps,chol,
     col = "brown3",
     pch = 20,
     main = "Diagrama de dispersiÃ³n")         

modelo <- lm(chol~trestbps)
summary(modelo)
abline(modelo)

#2 
cor(thalach,trestbps)
plot(thalach,trestbps,
     col = "brown3",
     pch = 20,
     main = "Diagrama de dispersion"
     #xlab = "",
     #ylab = ""
     )         

modelo1 <- lm(trestbps~thalach)
summary(modelo1)
abline(modelo1)

#3 
cor(ï..age,thalach)
plot(ï..age,thalach,
     col = "brown3",
     pch = 20,
     main = "Diagrama de dispersión",
     xlab = "Edad",
     ylab = "Frecuencia cardiaca maxima")         

modelo2 <- lm(thalach~ï..age)
summary(modelo2)
abline(modelo2)


cor(restecg,thalach)
cor(exang,thalach)
cor(thal,thalach)
cor(thal,chol)
cor(thalach,age)
cor(age,cp)
cor(thalach,chol)
cor(thalach,chol)
cor(thalach,chol)
cor(thalach,chol)
cor(thalach,chol)
cor(thalach,chol)
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
      enf_age <- sum(enfermos$ï..age) * (1/nrow(enfermos))
      print(enf_age)
      t.test(enfermos$ï..age)
      
      enf_trestbps <- sum(enfermos$trestbps) * (1/nrow(enfermos))
      print(enf_trestbps)
      t.test(enfermos$trestbps)
      
      enf_chol <- sum(enfermos$chol) * (1/nrow(enfermos))
      print(enf_chol)
      t.test(enfermos$chol)
      
      enf_thalach <- sum(enfermos$thalach) * (1/nrow(enfermos))
      print(enf_thalach)
      t.test(enfermos$ï..thalach)
      
    
    # No enfermos
      nenf_age <- sum(noenfermos$ï..age) * (1/nrow(noenfermos))
      print(nenf_age)
      t.test(noenfermos$ï..age)
      
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
      
    
      
      
      


# MODELOS LINEALES

# HIPOTESIS RESPECTO A LAS DISTRIBUCIONES

# CONCLUSIONES




