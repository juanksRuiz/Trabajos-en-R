#librerias
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("htmlwidgets")
install.packages("hash")
library("htmlwidgets")
library("ggplot2")
library("dplyr")
library("tidyverse")
library("stringr")
library("hash")
# Leyendo la base de datos de wifi

# En windows 10, con computador personal
#data_wifi <- read.csv(file="C:\\Users\\juank\\Desktop\\Trabajos-en-R\\Analisis_Estadistico\\proyecto_final\\acceso_internet.csv", header=TRUE, sep=",", encoding = "UTF-8")

#en linux, sala lovelace
# data_wifi <- read.csv(file = "/home/lovelace/Desktop/Trabajos-en-R/Analisis_Estadistico/proyecto_final/acceso_internet.csv",
#header=TRUE,
#encoding = "UTF-8")

# en sala Hipathia
data_wifi <- read.csv(file="C:\\Users\\prestamour\\Desktop\\Trabajos-en-R\\Analisis_Estadistico\\proyecto_final\\acceso_internet.csv",
                      encoding = 'UTF8',header = TRUE,sep = ',')
head(data_wifi)
View(data_wifi)




# funcion para cambiar las tildes
# x es el nombre a cambiar
foo <- function(x){
  x <- strsplit(x, '')[[1]]
  y <- ifelse(x == "á", 'a',
              ifelse(x == "é", 'e',
                     ifelse(x == "í", 'i',
                            ifelse(x == "ó", 'o',
                                   ifelse(x == "ú", 'u', x)))))
  paste(y, sep = "", collapse = "")
}

foo("adiós")

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
levels(data_wifi[,2])[23] <- "NARINO"
levels(data_wifi[,2])[26] <- "QUINDIO"
levels(data_wifi[,2])[32] <- "VAUPES"

#====================================================================================
#data frames por departamento
dfs_Depto <-  hash()
for (d in levels(data_wifi[,2])) {
  dfs_Depto[[d]] <- subset(data_wifi, data_wifi$DEPARTAMENTO == d)
}

View(dfs_Depto[["AMAZONAS"]])
length(dfs_Depto)

# data frames por proveedor
dfs_prov <- hash()
for(p in levels(data_wifi[,1])){
  dfs_prov[[p]] <- subset(data_wifi, data_wifi$PROVEEDOR == p)
}

View(dfs_prov[["AVANTEL S.A.S."]])
#data frames por segmento
dfs_seg <- hash()
for (s in levels(data_wifi[,4])) {
  dfs_seg[[s]] <- subset(data_wifi, data_wifi$SEGMENTO == s)
}
View(dfs_seg[["Corporativo"]])
#===========================================================================

# columnas 6 y 7: velocidad bajada y subida
data_wifi[,6] <- as.numeric(data_wifi[,6])
data_wifi[,7] <- as.numeric(data_wifi[,7])
summary(data_wifi)

#Datos subscripciones
data_subs <- subset(data_wifi[1:3,8:21])


#Scatter plots entre periodos (entre aÃ±o y aÃ±o) ???????????


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
