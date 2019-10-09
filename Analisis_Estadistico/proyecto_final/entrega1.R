#librerias
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("htmlwidgets")
library("htmlwidgets")
library("ggplot2")
library("dplyr")
library("tidyverse")
library("stringr")
# Leyendo la base de datos de wifi
data_wifi <- read.csv(file="C:\\Users\\juank\\Desktop\\Trabajos-en-R\\Analisis_Estadistico\\proyecto_final\\acceso_internet.csv", header=TRUE, sep=",")
View(data_wifi)

# Limpieza de datos 

#con grepl(patron, s) se confirma si el patron esta en s
#dep_Boy = grepl("BOYA",data_wifi[,2])
#data_wifi["DEPARTAMENTO",dep_Boy,]
#a <- subset(data_wifi["DEPARTAMENTO"],dep_Boy)


#EJ: en 3er fila hay Boyaca
err_dep <- c("BOYACï¿½")
idx_Boy <- data_wifi[,2] %in% err_dep
data_wifi[idx_Boy,"DEPARTAMENTO"] <- "BOYACA"


# levels(data_wifi[,2])
levels(data_wifi[,2])[4] <- "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA"
levels(data_wifi[,2])[5] <- "ATLANTICO"
levels(data_wifi[,2])[6] <- "BOGOTA D.C."
levels(data_wifi[,2])[7] <- "BOLIVAR"
levels(data_wifi[,2])[8] <- "BOYACA"
levels(data_wifi[,2])[14] <- "CHOCO"
levels(data_wifi[,2])[15] <- "CORDOBA"
levels(data_wifi[,2])[17] <- "GUAINIA"
levels(data_wifi[,2])[23] <- "NARIÑO"
levels(data_wifi[,2])[26] <- "QUINDIO"
levels(data_wifi[,2])[32] <- "VAUPES"

