install.packages("zoo")
install.packages("fitdistrplus")
library("fitdistrplus")
library("zoo")
# Ejercicio 1
num_muestras = 1000
# forma: alfa , escala: beta
x <- rgamma(num_muestras, shape = 10, scale = 3)
x <- x + rnorm(length(x), mean = 0, sd = .1)

bj <- hist(x, breaks=20, include.lowest = FALSE, right = FALSE)
# puntos de quiebre
bj$breaks

#valor de la funcion acumulada teorica (Gamma(10,3) en los puntos de quiebre del histograma)
b_cdf <- pgamma(bj$breaks, shape = 10, scale = 3)

#area acumulada bajo el rectangulo
#proporcion
pj <- rollapply(b_cdf, 2, function(x) x[2] - x[1])


Nj <- bj$counts


# encontrando el estimador
a = ((Nj - num_muestras*pj)^2)/(num_muestras*pj)
estimador <- sum(a)

# cuantos se observaron y cuanto se esperaba
# se Reescala las probabilidades
chisq.test(bj$counts,p=pj, rescale.p = TRUE)

# !!! LIMITE DE LA REGION DE RECHAZO
qchisq(0.95,df = length(bj$breaks)-1)


# 3
fit.gamma <- fitdist(x,distr = "exp", method = "mle")
summary(fit.gamma)
plot(fit.gamma)
