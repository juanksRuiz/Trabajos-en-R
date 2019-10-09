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

# columnas 6 y 7: velocidad bajada y subida
data_wifi[,6] <- as.numeric(data_wifi[,6])
data_wifi[,7] <- as.numeric(data_wifi[,7])
#===========================================================================
summary(data_wifi)
class(mpg)

#Datos subscripciones
data_subs <- subset(data_wifi[1:3,8:21])


#Scatter plots entre periodos (entre año y año) ???????????

#subscripciones por departamento
df_amazonas <- subset(data_wifi, data_wifi$DEPARTAMENTO == "AMAZONAS")
df_antioquia <- subset(data_wifi, data_wifi$DEPARTAMENTO == "ANTIOQUIA")
df_arauca <- subset(data_wifi, data_wifi$DEPARTAMENTO == "ARAUCA")
df_SAI <- subset(data_wifi, data_wifi$DEPARTAMENTO == "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA")
df_atlantico <- subset(data_wifi, data_wifi$DEPARTAMENTO == "ATLANTICO")
df_bogota <-subset(data_wifi, data_wifi$DEPARTAMENTO == "BOGOTA D.C.")
df_bolivar <- subset(data_wifi, data_wifi$DEPARTAMENTO == "BOLIVAR")
df_boyaca <- subset(data_wifi, data_wifi$DEPARTAMENTO == "BOYACA")
df_caldas <- subset(data_wifi, data_wifi$DEPARTAMENTO == "CALDAS")
df_caqueta <- subset(data_wifi, data_wifi$DEPARTAMENTO == "CAQUETA")
df_casanare <- subset(data_wifi, data_wifi$DEPARTAMENTO == "CASANARE")
df_cauca <- subset(data_wifi, data_wifi$DEPARTAMENTO == "CAUCA")
df_cesar <- subset(data_wifi, data_wifi$DEPARTAMENTO == "CESAR")
df_choco <- subset(data_wifi, data_wifi$DEPARTAMENTO == "CHOCO")
df_cordoba <- subset(data_wifi, data_wifi$DEPARTAMENTO == "CORDOBA")
df_cundinamarca <- subset(data_wifi, data_wifi$DEPARTAMENTO == "CUNDINAMARCA")
df_guainia <- subset(data_wifi, data_wifi$DEPARTAMENTO == "GUIANIA")
df_guaviare <- subset(data_wifi, data_wifi$DEPARTAMENTO == "GUAVIARE") 
df_huila <- subset(data_wifi, data_wifi$DEPARTAMENTO == "HUILA")
df_guajira <- subset(data_wifi, data_wifi$DEPARTAMENTO == "LA GUAJIRA")
df_magdalena <- subset(data_wifi, data_wifi$DEPARTAMENTO == "MAGDALENA")
df_meta <- subset(data_wifi, data_wifi$DEPARTAMENTO == "META")
df_narino <-subset(data_wifi, data_wifi$DEPARTAMENTO == "NARIÑO")
df_norSantander <- subset(data_wifi, data_wifi$DEPARTAMENTO == "NORTE DE SANTANDER")
df_putumayo <- subset(data_wifi, data_wifi$DEPARTAMENTO == "PUTUMAYO")
df_quindio <- subset(data_wifi, data_wifi$DEPARTAMENTO == "QUINDIO")
df_risaralda <- subset(data_wifi, data_wifi$DEPARTAMENTO == "RISARALDA")
df_santander <- subset(data_wifi, data_wifi$DEPARTAMENTO == "SANTANDER")
df_sucre <- subset(data_wifi, data_wifi$DEPARTAMENTO == "SUCRE")
df_tolima <- subset(data_wifi, data_wifi$DEPARTAMENTO == "TOLIMA")
df_vdCauca <- subset(data_wifi, data_wifi$DEPARTAMENTO == "VALLE DE CAUCA")
df_vaupes <- subset(data_wifi, data_wifi$DEPARTAMENTO == "VAUPES")
df_vichada <- subset(data_wifi, data_wifi$DEPARTAMENTO == "VIVHADA")

ggplot(data=data_subs, aes(x = 1:14, y=))
View(df_amazonas)


# desde columna 8 son subscripciones
#num de columnas : 21

medias <- function(df){
  m <- c()
  for (i in 8:21) {
    m <- c(m,mean(df[,i]))
  }
  return(m)
  
}

med_Bog <- medias(df_bogota)
med_Antioquia <- medias(df_antioquia)
med_atlantico <- medias(df_atlantico)
med_santander <- medias(df_santander)
med_narino <- medias(df_narino)
trimestre <- 1:14
med_total <- cbind(med_Bog,med_Antioquia,med_atlantico,med_santander,med_narino)
med_total <- data.frame(med_total)

ggplot(data=med_total, aes(x=trimestre,y=med_total[,1], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,2], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,3], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,4], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,5], group=1)) + geom_line()+geom_point()
