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
# En windows 10, con computador personal
#data_wifi <- read.csv(file="C:\\Users\\juank\\Desktop\\Trabajos-en-R\\Analisis_Estadistico\\proyecto_final\\acceso_internet.csv", header=TRUE, sep=",", encoding = "UTF-8")

#en linux, sala lovelace
data_wifi <- read.csv(file = "/home/lovelace/Desktop/Trabajos-en-R/Analisis_Estadistico/proyecto_final/acceso_internet.csv", header=TRUE, encoding = "UTF-8")
head(data_wifi)
View(data_wifi)

for(col in colnames(data_wifi)){
  Encoding(data_wifi[[col]])
}

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

#dfs <- c(df_amazonas,df_antioquia, df_arauca, df_atlantico,
#         df_bogota, df_bolivar,df_boyaca, df_caldas, df_caqueta, df_casanare,df_cauca, df_cesar, df_choco, df_cordoba, df_cundinamarca,
#         df_guainia, df_guajira, df_guaviare, df_huila, df_magdalena, df_meta, df_narino, df_norSantander, df_putumayo, df_quindio,
#         df_risaralda, df_SAI, df_santander, df_sucre, df_tolima, df_vaupes, df_vdCauca, df_vichada)

med_amaz <- medias(df_amazonas)
med_Antioquia <- medias(df_antioquia)
med_arauca <- medias(df_arauca)
med_atlantico <- medias(df_atlantico)
med_Bog <- medias(df_bogota)
med_bolivar <- medias(df_bolivar)
med_boyaca <- medias(df_boyaca) #
med_caldas <- medias(df_caldas)
med_caq <- medias(df_caqueta)
med_casanare <- medias(df_casanare)
med_cauca <- medias(df_cauca)
med_cesar <- medias(df_cesar)
med_choco <- medias(df_choco)
med_cord <- medias(df_cordoba)
med_cund <- medias(df_cundinamarca) #

med_total <- cbind(med_amaz, med_Antioquia, med_arauca, med_atlantico, med_Bog, med_bolivar, med_caldas, med_caq, med_casanare, med_cauca, med_cesar,
                   med_choco, med_cord, med_cund)
med_total <- data.frame(med_total)

# desde columna 8 son subscripciones
#num de columnas : 21

medias <- function(df){
  m <- c()
  for (i in 8:21) {
    m <- c(m,mean(df[,i]))
  }
  return(m)
  
}



trimestre <- 1:14

ggplot(data=med_total, aes(x=trimestre,y=med_total[,1], group=1,)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,2], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,3], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,4], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,5], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,6], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,7], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,8], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,9], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,10], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,11], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,12], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,13], group=1)) + geom_line()+geom_point()
ggplot(data=med_total, aes(x=trimestre,y=med_total[,14], group=1)) + geom_line()+geom_point() # cundinamarca - NaN
