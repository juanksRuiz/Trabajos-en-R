library(datasets)
data("iris")
View(iris)
summary(iris)

AD <- function(col){
  #Retorna vector con estadistica descriptiva de la columna de datos
  res <- c(summary(col), var(col), sd(col))
  return(res)
}
IntervalConfNorm <-function(col,estim,alfa,sdEstimador){
  lowIC <- estim - qnorm(1-alfa/2)*sdEstimador/(sqrt(length(col)))
  upIC <-  estim + qnorm(1-alfa/2)*sdEstimador/(sqrt(length(col)))
  return(c(lowIC,upIC))
} 
#Ejercicio 1
# a) Estadistico de prueba para mu: xBar
#mu longitud de los sepalos

# H0: mu = 6
# Ha: mu > 6
# Estadistico de prueba: xBar
# Region de rechazo
# Nivel de confianza de 0.95 : alfa = 0.05
rm(list = ls())
data <- c(AD(iris$Sepal.Length),AD(iris$Sepal.Width),AD(iris$Petal.Length),
          AD(iris$Petal.Width))
matData <- matrix(data,nrow = 4,byrow = TRUE)
colnames(matData) <- c("Min","1st qu","Median","Mean","3rd qu,","Max","Var","sd")
rownames(matData) <- c("Sepal length","Sepal width","Petal length", "Petal width")


xBar <- sum(iris$Sepal.Length)/nrow(iris)
sdXbar <- sqrt(var(iris$Sepal.Length)/nrow(iris))
#con la forma estandarizada
xRR <- (xBar - 6)/sdXbar
qnorm(0.05)
xRR > qnorm(0.95) 

#con la forma normal
6+qnorm(0.95)*sdXbar # = 6.11
# H0 no pertenece a la region de rechazo --> no se rechaza H0

#calculando el intervalo de confianza
IC_sepalLength <- IntervalConfNorm(iris$Sepal.Length, xBar, 0.05,sdXbar)
# En realidad H0 no pertenece al IC --> rechazo H0

#valor p: P(theta > theta observado) CALCULAR
