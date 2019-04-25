library(dplyr)
View(mtcars)
mtcars %>% View()

summary(mtcars)
sd(mtcars$mpg)
apply(mtcars,2 ,sd)

cor(mtcars$mpg,mtcars$hp)

attach(mtcars)
cor(mpg, hp)

#Para sacar el parametro Beta1

numB1 = sum((hp-mean(hp))*(mpg-mean(mpg)))
den <- sum((hp-mean(hp))^2)
beta1 <- numB1/den
beta0 <- mean(mpg) - beta1*mean(hp)
plot(hp,mpg, xlab="caballos de fuerza", ylab="Millas por galón", col = "deepskyblue",pch = 20, main="Diagrama de dispersión")
abline(a = beta0, b=beta1, col="red")


modelo <- lm(mpg~hp)
#estadisticos de los errores
summary(modelo)
#siempre comprobar si el promedio de los errores tiende a 0
mean(modelo$residuals)

#--------------------------------------------------------
cor(mtcars$qsec, mtcars$hp)
numB1_2 = sum((mtcars$qsec-mean(mtcars$qsec))*(hp-mean(hp)))
den2 <- sum((-mean(mtcars$qsec))^2)
beta1_2 <- numB1_2/den2
beta0_2 <- mean(hp) - beta1*mean(mtcars$qsec)
abline(a = beta1_2,  b = beta1_2,col="red")
modelo <- lm(qsec~hp)


#qsec = tiempo que tarda en recorrer un carto de milla
plot(mtcars$qsec, mtcars$hp, xlab ="tiempo para cuarto de milla",
     ylab = "caballos de potencia",col = "deepskyblue",pch = 20, main="Diagrama de dispersión" )
