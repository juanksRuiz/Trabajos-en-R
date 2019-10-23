install.packages("ggplot2")
library("ggplot2")

#Ejercicio 4
Year <- c(2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016)
Month <- c(12, 11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1)
Interest_Rate <- c(2.75,2.5,2.5,2.5,2.5,2.5,2.5,2.25,2.25,2.25,2,2,2,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75)
Unemployment_Rate <- c(5.3,5.3,5.3,5.3,5.4,5.6,5.5,5.5,5.5,5.6,5.7,5.9,6,5.9,5.8,6.1,6.2,6.1,6.1,6.1,5.9,6.2,6.2,6.1)
Stock_Index_Price <- c(1464,1394,1357,1293,1256,1254,1234,1195,1159,1167,1130,1075,1047,965,943,958,971,949,884,866,876,822,704,719)

mat <- cbind(Year,Month,Interest_Rate, Unemployment_Rate, Stock_Index_Price)

#Variable dependiente: Precio del índice de acciones (la ultima)
# variables independientes: tasa de interés y tasa de desempleo

#i)
Z <- cbind(rep(1,length(Year)),Interest_Rate, Unemployment_Rate)
Y <-  Stock_Index_Price

# Beta gorro
Bg <- solve(t(Z)%*%Z)%*%t(Z)%*%Y

# Y gorro
Yg <- Z%*%Bg

# Epsilon gorro
Eps_g <- (Y-Yg)

# ii) Intervalo de confianza de 95% del valor Esperado de precio de índice de acciones (E(Y_01))
# con tasa de interés 1.5 y tasa de desempleo 6.3

z0 <- c(1,1.5,6.3)
n <- length(Year)
r <- 2
alfa = 0.05
t <- qt((1-alfa)/2,df=n-r-1, lower.tail = FALSE)
s2 = (t(Eps_g)%*%Eps_g)/(n-r-1)
D <- t(z0)%*%solve(t(Z)%*%Z)%*%z0*s2

a <- t(z0)%*%Bg - t*sqrt(D)
b <- t(z0)%*%Bg + t*sqrt(D)

# Intervalo de confianza de 95% de E(Y_01)
i = c(a,b)

#iii)
# Intervalo de confianza para Y_01
G <- s2*(1-t(z0)%*%solve(t(Z)%*%Z)%*%z0)
a2 <- t(z0)%*%Bg - t*sqrt(G)
b2 <- t(z0)%*%Bg + t*sqrt(G)

#intervalo de confianza para Y_01
i2 <- c(a2,b2)

#iv)
t(Y)%*%Y # = 28503296
(t(Yg)%*%Yg + t(Eps_g)%*%Eps_g)  # = 28503296

#v)
# Grafica 1
mat = data.frame(mat)
ggplot(data= mat, aes(x = Interest_Rate, y = Stock_Index_Price)) + geom_point(color="blue") 


#Grafica 2
ggplot(data= mat, aes(x = Unemployment_Rate, y = Stock_Index_Price)) + geom_point(color="blue") 


#=============================================================
#ejercicio 5 (base de datos: Parcial2Ej5)
#i) 
R = data.frame(Parcial2Ej5)
cor(R)

