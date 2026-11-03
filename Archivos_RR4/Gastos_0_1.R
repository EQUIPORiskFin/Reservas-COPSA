#####Author Act. Alejandro García de León
#####Archivo Gastos (0,1) del RR4 
#####Metodo principal Archivo_Gastos_LP(Base,corte)
library(readr)
library(dplyr)
library(lubridate)
library(openxlsx)
###Campo 1
Calcular_edad_1 = function(Base,corte){
  Base$Edad = floor(time_length(interval(Base$`Fecha de nacimiento`,corte), "years"))
  return(Base$Edad)
}
###Campo 2
Calcular_Plazo_2 = function(Base,corte){
  Base$temporalidad2 = floor(time_length(interval(Base$`Fecha inicio`,Base$`Fecha fin`), "years"))
  return(Base$temporalidad2)
}
###Campo 3
Calcular_Antigedad_3 = function(Base,corte){
  Base$antiguedad = floor(time_length(interval(Base$`Fecha inicio`,corte), "years"))
  return(Base$antiguedad)
}
###Campo 4
Calcular_Vigencia_rest_4 = function(Base,corte){
  Base$Vig_rest = floor(time_length(interval(corte,Base$`Fecha fin`), "years"))
  return(Base$Vig_rest)
}
###Campo 5
Calcular_Moneda_5 = function(Base,corte){
  Mon = sort(unique(Base$Moneda))
  Mon1 = Mon
  Base$MON = 10
  Base$MON = ifelse(Base$Moneda != "Pesos", 20, Base$MON)
  return(Base$MON)
}
###Campo 6
Calcular_Mod_Plan_6 = function(Base,corte){
  ruta = "D:/Carp/CSV/proyecto/RR4/Gastos/Plantillas/Plantilla_temporalidad.csv"
  Plantilla_temporalidad <- read_csv(ruta)
  #s = unique(Plantilla_temporalidad$Plan)
  #s = sort(s)
  #x = sort(unique(Base$Plan))
  plan = Plantilla_temporalidad$Plan
  llave = Plantilla_temporalidad$llave
  catalogo_plan = setNames(llave, plan)
  Base$Mod_Plan = catalogo_plan[Base$Plan]
  return(Base$Mod_Plan)
}
###Campo 7
#ruta3 = "C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Gastos/Plantillas/Plantilla_caducidades.csv"
Calcular_Tipo_Cad_7 = function(Base,corte){
  ruta3 = "D:/Carp/CSV/proyecto/RR4/Gastos/Plantillas/Plantilla_caducidades.csv"
  Base$Tip_Cad = 0
  k = is.na(Base$`Tipo de caducidad/ Hipótesis de caducidad`)|Base$`Tipo de caducidad/ Hipótesis de caducidad`=="NA"
  Base$Tip_Cad[k] = 1
  k = Base$Tip_Cad ==0
  if(any(k)){
    Ma <- read_csv(ruta3)
    A = as.character(Ma$...1)
    Ma = as.matrix(Ma[,-1]) 
    rownames(Ma) = A
    t1 = Calcular_Mod_Plan_6(Base,corte)
    t2 = Calcular_Moneda_5(Base,corte)
    t1 = t1[k]
    t2 = t2[k]
    Base$Tip_Cad[k] =  mapply(
      function(x, y) Ma[as.character(x), as.character(y)],
      x = t1,
      y = t2
    )}
  return(Base$Tip_Cad)
}
###Campo 8,9,10
#ruta 1 = "C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Gastos/Plantillas/Esquemas_reaseguro.csv"
Calcular_esquemasReas = function(Base,corte){
  ruta1 = "D:/Carp/CSV/proyecto/RR4/Gastos/Plantillas/Esquemas_reaseguro.csv"
  Esquemas_reaseguro <- read_csv(ruta1)
  Esquemas_reaseguro$cve = as.character(Esquemas_reaseguro$cve)
  Base= Base%>%left_join(Esquemas_reaseguro, by = c("cve_reas" = "cve", "anio" = "anio"))
  return(Base[,c("Clave_esquema1","Clave_esquema2","Clave_esquema3")])
}
###Campo 11
Calcular_llave_ben_11 = function(Base,corte){
  Falle = list()
  Falle[[1]] = ifelse(Base$`Suma asegurada Fallecimiento`>0,1,0) 
  Falle[[2]] = ifelse(Base$`Suma asegurada Pérdidas Orgánicas`>0,1,0)
  Falle[[3]] = ifelse(Base$`Suma asegurada Muerte accidental`>0,1,0)
  Falle[[4]] = ifelse(Base$`Suma asegurada Muerte Colectiva`>0,1,0)
  Falle[[5]] = ifelse(Base$`Suma asegurada Incapacidad o Invalidez`>0,1,0)
  Falle[[6]] = ifelse(Base$`Suma asegurada Otros beneficios`>0,1,0)
  Falle[[7]] = ifelse(Base$`Suma asegurada Sobrevivencia`>0,1,0)
  Falle[[8]] = ifelse((!is.na(Base$`Tipo de caducidad/ Hipótesis de caducidad`)&!trimws(Base$`Tipo de caducidad/ Hipótesis de caducidad`) %in% c("NA", "")),1,0)
  d <- apply(
    do.call(cbind, Falle),
    1,
    paste,
    collapse = ""
  )
  d = paste0("|",d,"|")
  Base$Cod_ben = d
  return(Base$Cod_ben)
}
##Campo 12,13,14,15,16,17,18
Calcular_SA = function(Base,corte){
  Base$SUMA_A_FALL = ifelse(is.na(as.numeric(Base$`Tasa garantizada del fondo (Componente de inversión)`)),
                          Base$`Suma asegurada Fallecimiento`,
                          (as.numeric(Base$`Valor del Fondo a la fecha de corte (Componente de inversión)`)*
                            (1+as.numeric(Base$`Tasa garantizada del fondo (Componente de inversión)`))))                                                                                                                          
  Base$SUMA_A_PERD_ORG = Base$`Suma asegurada Pérdidas Orgánicas`
  Base$SUMA_A_MUERTE_ACC = Base$`Suma asegurada Muerte accidental`
  Base$SUMA_A_MUERTE_COLECT = Base$`Suma asegurada Muerte Colectiva`
  Base$SUMA_A_INC_INV = Base$`Suma asegurada Incapacidad o Invalidez`
  Base$SUMA_A_OTROS_BEN = Base$`Suma asegurada Otros beneficios`
  Base$SUMA_A_SUPERV = Base$`Suma asegurada Sobrevivencia`
  return(Base[,c("SUMA_A_FALL","SUMA_A_PERD_ORG","SUMA_A_MUERTE_ACC","SUMA_A_MUERTE_COLECT",
                 "SUMA_A_INC_INV","SUMA_A_OTROS_BEN","SUMA_A_SUPERV")])
}
###Campo 19 
Calcular_Rescate_19 = function(Base,corte){
  Base$Rescate = ifelse(Base$Flexible == "SI",as.numeric(Base$`Valor del Fondo a la fecha de corte (Componente de inversión)`)*(1+
    as.numeric(Base$`Tasa garantizada del fondo (Componente de inversión)`)),0)
    Base$Rescate[which(is.na(Base$Rescate))] = Base$`Valor del Fondo a la fecha de corte (Componente de inversión)`[which(is.na(Base$Rescate))]
  return(Base$Rescate)
  }
###Campo 20,21,22
Calcular_Prima_tarifa_anual = function(Base,corte){
  Base$Prima_tarifa_anual = ifelse(Base$`Forma de pago de prima` =="Prima Nivelada cada año",Base$`Prima de riesgo/ Prima Neta Nivelada`,Base$`Prima de riesgo/ Prima Neta Nivelada`/Base$temporalidad2 )
  Base$Gastos_operacion = Base$`G. ADM.`*Base$Prima_tarifa_anual
  Base$Gastos_adquisicion = Base$`C. ADQ.`*Base$Prima_tarifa_anual
  return(Base[,c("Prima_tarifa_anual","Gastos_operacion","Gastos_adquisicion")])
}
###Campo 21 a 26 son rentas vitalicias
###Campo 27 y 28
Calcular_Acumulados = function(Base,corte){
  Base$Reserva_acum = ifelse(Base$`Aclaraciones reaseguro` == "Al esquema de reaseguro no le aplica una deducción de la reserva y el reaseguro se aplica solamente a las coberturas contratadas de Fallecimiento, Muerte accidental y Muerte Colectiva.",0,1)
  Base$Num_reg_Acum = 1
  return(Base[,c("Reserva_acum","Num_reg_Acum")])
}
###Campo 29
Calcular_mortalidad = function(Base,corte){
  Base$Tipo_Mort  =ifelse(Base$`Tipo de mortalidad/ Hipótesis de mortalidad` == "Estatutaria",0,1)
  return(Base$Tipo_Mort)
}
###Campo 30
#ruta2 = "C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Gastos/Plantillas/Plantilla_beneficios_reaseguro.csv"ruta2 = "C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Gastos/Plantillas/Plantilla_beneficios_reaseguro.csv")
Calcular_llave_ben_reaseguro = function(Base,corte){
  ruta2 ="D:/Carp/CSV/proyecto/RR4/Gastos/Plantillas/Plantilla_beneficios_reaseguro.csv"
  Rease = list()
  Plantilla_beneficios_reaseguro <- read_csv(ruta2)
  Rease = ifelse(Plantilla_beneficios_reaseguro$Aplica>0,1,0) 
  Rease = as.list(Rease)
  #unique(Base$`Aclaraciones reaseguro`)
  #d1 <- paste(Rease,collapse = "|")
  d1 <- apply(
    do.call(cbind, Rease),
    1,
    paste,
    collapse = ""
  )
  d1 = paste0("|",d1,"|")
  Base$llave_benef_Reas=d1
  return(Base$llave_benef_Reas)
}
Gastos_LP = function(Base,corte){
  Gasto_01_LP = data.frame(edad = Calcular_edad_1(Base,corte))
  Gasto_01_LP$Flexible = Base$Flexible  
  Gasto_01_LP$Plazo_Seguro = Calcular_Plazo_2(Base,corte)
  Gasto_01_LP$antiguedad = Calcular_Antigedad_3(Base,corte)
  Gasto_01_LP$Vigencia_Restante = Calcular_Vigencia_rest_4(Base,corte)
  Gasto_01_LP$MON = Calcular_Moneda_5(Base,corte)
  Gasto_01_LP$Mod_Plan = Calcular_Mod_Plan_6(Base,corte)
  Gasto_01_LP$Caducidad = Calcular_Tipo_Cad_7(Base,corte)
  Gasto_01_LP$ER_1 = unlist(Calcular_esquemasReas(Base,corte)[,1])
  Gasto_01_LP$ER_2 = unlist(Calcular_esquemasReas(Base,corte)[,2])
  Gasto_01_LP$ER_3 = unlist(Calcular_esquemasReas(Base,corte)[,3])
  Gasto_01_LP$llave_benef_Reas = Calcular_llave_ben_11(Base,corte)
  Gasto_01_LP$SUMA_A_FALL = unlist(Calcular_SA(Base,corte)[,1])
  Gasto_01_LP$SUMA_A_PERD_ORG = unlist(Calcular_SA(Base,corte)[,2])
  Gasto_01_LP$SUMA_A_MUERTE_ACC = unlist(Calcular_SA(Base,corte)[,3])
  Gasto_01_LP$SUMA_A_MUERTE_COLECT = unlist(Calcular_SA(Base,corte)[,4])
  Gasto_01_LP$SUMA_A_INC_INV = unlist(Calcular_SA(Base,corte)[,5])
  Gasto_01_LP$SUMA_A_OTROS_BEN = unlist(Calcular_SA(Base,corte)[,6])
  Gasto_01_LP$SUMA_A_SUPERV = unlist(Calcular_SA(Base,corte)[,7])
  a = Base$`Valor del Fondo a la fecha de corte (Componente de inversión)`[which(!is.na(as.numeric(Base$`Tasa garantizada del fondo (Componente de inversión)`)))]
  a = as.numeric(a)
  i = Base$`Tasa garantizada del fondo (Componente de inversión)`[which(!is.na(as.numeric(Base$`Tasa garantizada del fondo (Componente de inversión)`)))]
  i=as.numeric(i)
  n = Gasto_01_LP$Vigencia_Restante [which(!is.na(as.numeric(Base$`Tasa garantizada del fondo (Componente de inversión)`)))]
  Gasto_01_LP$SUMA_A_SUPERV[which(!is.na(as.numeric(Base$`Tasa garantizada del fondo (Componente de inversión)`)))] = a*((1+i)^n)
  Gasto_01_LP$rescate = Calcular_Rescate_19(Base,corte)
  Gasto_01_LP$Prima_tarifa_anual = unlist(Calcular_Prima_tarifa_anual(Base,corte)[,1])
  Gasto_01_LP$Gastos_operacion = unlist(Calcular_Prima_tarifa_anual(Base,corte)[,2])
  Gasto_01_LP$Gastos_adquisicion = unlist(Calcular_Prima_tarifa_anual(Base,corte)[,3])
  Gasto_01_LP$Reserva_acum = unlist(Calcular_Acumulados(Base,corte)[,1])
  Gasto_01_LP$Num_reg_Acum = unlist(Calcular_Acumulados(Base,corte)[,2])
  Gasto_01_LP$llave_benef_Reas = Calcular_llave_ben_reaseguro(Base,corte)
  return(Gasto_01_LP)
}
Archivo_Gastos_LP = function(Base,corte){
  df = Gastos_LP(Base,corte)
  df <- df %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  return(df)
}
#Crew = Archivo_Gastos_LP(Base,corte)
#write.xlsx(Crew,"D:/Carp/CSV/proyecto/RR4/Gastos/Plantillas/Gastos_preeliminar.xlsx")
