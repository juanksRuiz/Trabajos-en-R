View(mtcars)
mtcars
summary(mtcars)
pairs(mtcars)

plot(mtcars$hp,mtcars$wt)

#Que es la variable dependiente e independientew (cual va en eje x y y)

#Modelo linear
#abajo del intercept dice el nombre de la variable que aumenta (en X) y el valor es el de la pendiente
#Entender los niveles de significancia
fit <- lm(hp~wt, data = mtcars)
summary(fit)
qt(0.95,nrow(mtcars)-1)

#intervalo de confianza
#bota int. de confianza de Beta0 y Beta1
#por defecto el nivel de confianza es de 0.95
confint(fit, level = 0.9)


#Parametro 200: representa el nuevo valor de la variable elegida, en este caso wt

#Estimar intervalo de confianza para el valor  de la varable dependiente
nuevos <-  data.frame(wt = seq(200,300, by = 5))

p_new <- predict(fit, nuevos, interval="predict") #nuevos datos

#Estimar intervalo de confianza para el valor esperado de la varable dependiente
p_mean <- predict(fit, nuevos, interval="confidence") #Media
plot(mtcars$hp, mtcars$wt, pch = 20, col = "blue")
plot(nuevos$wt , p_new[,2], col = "red", type="l", ylim = c(5000,20000))
par(new = T)
plot(nuevos$wt , p_new[,3], col = "red", type="l", ylim = c(5000,20000))
par(new = T)
