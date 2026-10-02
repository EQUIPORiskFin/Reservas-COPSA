####Sección de tablas y parámetros
library(readxl)
library(dplyr)
library(readr)
library(lubridate)
setwd("C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Reservas/ENTORNO_DEFINITIVO")
source("001_Tarificacion_temporales.R")
source("002_Tarificacion_dotalesMixtos_Vitalicios.R")
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
#######Módulo 1
###Tarificación
#########El presente módulo
##tiene por objeto calcular la prima de tarifa
##para las pólizas presentadas de acuerdo
##a las plantillas de acuerdo al manual
library(readxl)
library(dplyr)
##Cambiar ruta según localización de la plantilla
ruta = "C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Reservas/ENTORNO_DEFINITIVO/Plantillas/polisario.xlsx"
Polisario <- read_excel(ruta, col_types = c("text", "text", "text", "numeric", "date", "numeric", "text", "date", "date"))
###########################
##Filtrado por plan
library(readxl)
library(dplyr)
tabla = AMIS
ruta = "C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Reservas/ENTORNO_DEFINITIVO/Plantillas/Plantilla_Gastos.xlsx"
Gastos <- read_excel(ruta)
Polisario2 = Polisario%>%filter(Polisario$Producto == "Temporal")
Polisario3 = Polisario%>%filter(Polisario$Producto == "Vitalicio")
Polisario4 = Polisario%>%filter(Polisario$Producto == "Dotal")
####################################
tasa = .06
######
######Seguros Dotales Mixtos
CA = CA_plantilla$Dotal
tabla = AMIS
Polisario1D = Prepara_Polisario_d(Polisario4,corte, Gastos)
Polisario1D = Calcula_PT_d(Polisario1D,tabla, tasa,CA)
##########################################
####
####Seguros Vitalicios
CA = CA_plantilla$Vitalicio
Polisario1V = Prepara_Polisario_d(Polisario3,corte, Gastos)
Polisario1V = Calcula_PT_d(Polisario1V,tabla, tasa,CA)
####
####Seguros Temporales
CA = CA_plantilla$Temporal
Polisario1T = Prepara_Polisario(Polisario2,corte, Gastos)
Polisario1T = Calcula_PT(Polisario1T,tabla, tasa,CA)
