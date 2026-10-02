####Sección de tablas y parámetros
library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(openxlsx)
setwd("C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Reservas/ENTORNO_DEFINITIVO")
source("Modulo_2_Valuacion_de_Reservas.R")
##Fecha de corte
##Modificar según convenga
corte = as.Date("2025-10-31")
##Tabla que será usada en reservas
tabla_CNSF_M_2013 <- read_csv("C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Reservas/ENTORNO_DEFINITIVO/Plantillas/Tabla_CNSF_M_2013.csv")
names(tabla_CNSF_M_2013)[2] = "q"
tabla_CNSF_M_2013$p = 1-tabla_CNSF_M_2013$q
library(readxl)
ruta = "C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Reservas/ENTORNO_DEFINITIVO/Plantillas/CA_plantilla.xlsx"
CA_plantilla <- read_excel(ruta)
######################3
##Tablas que serán empleadas para la desviación de siniestralidad RRC
library(readxl)
ruta = "C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Reservas/ENTORNO_DEFINITIVO/Plantillas/Percentil.xlsx"
Percentil <- read_excel(ruta)
names(Percentil)[2] = "q"
Percentil$p = 1-Percentil$q
library(readxl)
ruta = "C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Reservas/ENTORNO_DEFINITIVO/Plantillas/AMIS.xlsx"
AMIS <- read_excel(ruta)
AMIS$p = 1-AMIS$q
###############################3
###############################
#######Módulo 2
###Valuacion de reservas
#########El presente módulo
##tiene por objeto calcular el BEL y los importes
#recuperables de reaseguro
#Se asume que se cuenta con un data frame proveniente del 
###"Modulo_1_Tarificacion.R"
library(readxl)
library(dplyr)
library(lubridate)
library(openxlsx)
ruta = "C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Reservas/ENTORNO_DEFINITIVO/Plantillas/Plantilla_Gastos.xlsx"
source("003_Calculo_BEL.R")
tabla = tabla_CNSF_M_2013
corte = as.Date("2025-10-31")
###########Se desea calcular el BEL para cada polisario
###sin importes recuperables de reaseguro
ruta = "C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Reservas/ENTORNO_DEFINITIVO/Plantillas/mensual_TL.csv"
BEL_V = Calcula_BEL(Polisario1V, tabla, corte, anual=0, tasa = ruta)
BEL_T = Calcula_BEL(Polisario1T, tabla, corte,anual  =0,tasa = ruta)
BEL_D = Calcula_BEL(Polisario1D, tabla, corte,anual  =0,tasa = ruta)
###
IRR_V = BEL_IRR(Polisario1V, tabla,Percentil, corte,anual  =0,tasa = ruta)
IRR_T = BEL_IRR(Polisario1T, tabla,Percentil, corte,anual  =0,tasa = ruta)
IRR_D = BEL_IRR(Polisario1D, tabla,Percentil, corte,anual  =0,tasa = ruta)
############Generar_Reporte
IRR_D = BEL_IRR(Polisario1D, tabla,Percentil, corte,anual  =0,tasa = ruta)
############Generar_Reporte
cols_comunes <- Reduce(intersect, list(names(IRR_V), names(IRR_T), names(IRR_D)))
resultado1IRR <- bind_rows(
  select(IRR_V, all_of(cols_comunes)),
  select(IRR_T, all_of(cols_comunes)),
  select(IRR_D, all_of(cols_comunes))
)
cols_comunes <- Reduce(intersect, list(names(BEL_V), names(BEL_T), names(BEL_D)))
resultado1SOLOBEL <- bind_rows(
  select(BEL_V, all_of(cols_comunes)),
  select(BEL_T, all_of(cols_comunes)),
  select(BEL_D, all_of(cols_comunes))
)
########Exportacion_resultados
#Cambiar la ruta en caso necesario
#write.xlsx(resultado1SOLOBEL,"C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Reservas/ENTORNO_DEFINITIVO/BEL.xlsx")
#write.xlsx(resultado1IRR,"C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Reservas/ENTORNO_DEFINITIVO/BEL_IRR.xlsx")
