#Confugurando el espacio de trabajo
rm(list = ls())
setwd("C:/Users/juank/Desktop/Trabajos-en-R/Estadistica/LLANOS_RUIZ_TAREA1")
projects_df <- read.csv(file="ks_projects_201801.csv",header = TRUE,sep = ',')
#-----------------------------------
#Funciones
AD <- function(col){
  #Retorna vector con estadistica descriptiva de la columna de datos
  res <- c(summary(col), var(col), sd(col))
  return(res)
}

IC <- function(datos,alfa){
  #retorna el intervalo de confianza con nivel de confianza 1 - alfa
  #alfa: error de confianza
  xbar <- sum(datos)/length(datos)
  lowerLimit <- xbar + qnorm(alfa/2)*(sd(datos)/(length(datos)**0.5))
  upperLimit <- xbar - qnorm(alfa/2)*(sd(datos)/(length(datos)**0.5))
  return(c(lowerLimit,upperLimit))
}

#----------------------------------------
# a)
usd_PR_all <- projects_df$usd_pledged_real
succesful_proj <- subset(projects_df,state == "successful")$usd_pledged_real
failed_proj <- subset(projects_df,state == "failed")$usd_pledged_real

software_proj <- subset(projects_df, category == "Software")$usd_pledged_real
soft_succes_proj <- subset(subset(projects_df, category == "Software"), state == "successful")$usd_pledged_real
soft_fail_proj <- subset(subset(projects_df, category == "Software"),state == "failed")$usd_pledged_real

data_vector <- c(AD(usd_PR_all), AD(succesful_proj), AD(failed_proj),
                 AD(software_proj), AD(soft_succes_proj), AD(soft_fail_proj))

matrizAD <- matrix(data_vector,byrow = TRUE, nrow = 6)
colnames(matrizAD) <- c("Min", "1st Qu", "Median", "Mean", "3rd Qu.", "Max","Var", "sd")
rownames(matrizAD) <- c("Todas propuestas","Prop. exitosas", "Prop fallidas",
                        "Prop Software", "P_Soft_exitosas", "P_Soft_fallidas")

#Graficas
#histogramas
ha <- hist(usd_PR_all,main = "Histograma de dinero prometido en proyectos",
           xlab = "Pledged amount in USD",
           ylab = "Frequency")
hs <- hist(succesful_proj, main ="Histograma de dinero prometido en proyectos exitosos",
           xlab = "Pledged amount in USD",
           ylab = "Frequency")
hf <- hist(failed_proj, main ="Histograma de dinero prometido en proyectos fallidos",
           xlab = "Pledged amount in USD",
           ylab = "Frequency")


hsa <- hist(software_proj, main = "Histograma de dinero prometido en proyectos de software",
           xlab = "Pledged amount in USD",
           ylab = "Frequency")
hss <- hist(soft_succes_proj, main = "Histograma de dinero prometido en proyectos de software exitosos",
            xlab = "Pledged amount in USD",
            ylab = "Frequency")
hsf <- hist(soft_fail_proj, main = "Histograma de dinero prometido en proyectos de software fallidos",
            xlab = "Pledged amount in USD",
            ylab = "Frequency")

#diagramas de cajas y bigotes
b1 <- boxplot(usd_PR_all, succesful_proj, failed_proj,names = c("USD pledged real all projects",
                                                                "USD pledged real in succeeded projects",
                                                                "USD pledged real in failed projects"))
b2 <- boxplot(software_proj, soft_succes_proj, soft_fail_proj,names= c("USD pledged real in software projects",
                                                                "USD pledged real in succeeded software projects",
                                                                "USD pledged real in failed software projects"))

#b)
#Revisar funcion
IC_all <- IC(software_proj,0.05)
IC_suc <- IC(soft_succes_proj,0.05)
IC_fail <- IC(soft_fail_proj,0.05)
