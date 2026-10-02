#######Los siguientes códigos permiten calcular el
###mejor estimador (BEL) para reserva de riesgos en curso.
###
library(lubridate)
library(dplyr)
library(readr)
library(readxl)
##Función 1
######Funcion que recibe como parámetro un elemento del output
###del "Modulo_1_Tarificacion.R" es decir, una póliza y su
#prima de tarifa y calcula el BEL para siniestros VPE_sin
#BEL para ingresos VPE_ingresos
#BEL para gastos VPE_gtos y
#BEL para parte dotal VPE_dotal
#En el caso de seguros temporales este output y sus variables
#involucradas obtenidas valdrán cero.
##Parametro corte incluye la fecha de valuacion corte
##La tabla a usar será la de la CNSF 2013 de acuerdo a la plantilla
##Se incorpora cancelación también a traves de la respectiva plantilla
##Se incluye la curva de rendimiento proporcionada por proveedor de precios
##de acuerdo a la plantilla
##Variable binaria anual para distinguir valuación temporalidad anual y mensual
##El código debe ser usado de forma mensual pues la plantilla de curva de rendimeinto
##Esta en forma mensual. En el caso anual = 1, se usa una tasa del 6%. Este valor
##Puede modificarse y solo sería una aproximación pues no se ha implementado una 
##Plantilla de tasa de interés anual. 
##para filtrar pólizas cuya vigencia incluya a esta
##emisión anterior al corte
#No se usa cancelación
library(readr)
#En caso de cambiar la ruta para cancelación
#de acuerdo al repositorio modificar la ruta interna manualmente
BEL = function(P1,tabla,corte, anual = 0, tasa = ruta){
  edad = P1$Edad
  if(P1$Producto == "Dotal"){
    cancelacion = read_excel("Cancelacion.xlsx", 
                             sheet = "Dotal")
  }else if(P1$Producto == "Vitalicio"){
    cancelacion = read_excel("Cancelacion.xlsx", 
                             sheet = "vitalicio")
  }else{
    cancelacion = read_excel("Cancelacion.xlsx", 
                             sheet = "temporal")
  }
  crt = P1$Temporalidad
  if(anual == 0){
    tabla  =Mensualizar(tabla)
    tasa1 = read_csv(tasa)
    #tasa1 = tasa1$Pesos
    tasa1 =  tasa1$mensual
    cancelacion =  Mensualizar_Canc(cancelacion)
    prima = P1$PT_Niv/12
    crt = crt*12
    crt1 = (100-edad)*12
  }else{
    tasa1 = rep(0.06,times = length(tabla$Edad))
    crt1 = 100-edad
    prima = P1$PT_Niv}
  ######
  Resultado = data.frame(Edad = tabla$Edad)
  Resultado$p = tabla$p
  Resultado$q = tabla$q
  Resultado$SA = P1$`Suma Asegurada`
  Resultado$Prima_original = P1$PT_Niv
  Resultado$PT = prima
  suppressMessages({Resultado = Resultado%>%filter(Edad>=edad)})
  Resultado$PT[1] = Resultado$PT[1] + 500
  if(!is.na(crt)){
    Resultado = Resultado[c(1:crt),]
  }else{
    Resultado = Resultado[c(1:crt1),]
  }
  Auxiliar2 = cancelacion
  Auxiliar2 = Auxiliar2[c(1:length(Resultado$Edad)),]
  Resultado$caducidad = Auxiliar2$Nacional
  Resultado$tasa  =tasa1[c(1:length(Resultado$Edad))]
  Resultado$v = 1/(1+Resultado$tasa)
  n = length(Resultado$Edad)
  Resultado$v_venc = mapply(function(x,y){y^(x+1)},x = seq(from = 0, to = (n-1)), y = Resultado$v)
  Resultado$v_ant = mapply(function(x,y){y^(x)},x = seq(from = 0, to = (n-1)), y = Resultado$v)
  Resultado$pcanc = Resultado$p * (1-Resultado$caducidad)
  n=length(Resultado$q)
  Resultado$p_t = 1
  Resultado$p_tau = 1
  for(k in 2:n){
    Resultado$p_t[k] = Resultado$p_t[k-1]*Resultado$p[k-1]
    Resultado$p_tau[k] =  Resultado$p_tau[k-1]*Resultado$pcanc[k-1]
  }
  Resultado$GA = P1$GA
  Resultado$CA = P1$CA
  Resultado$FE_sin = Resultado$SA * Resultado$q * Resultado$p_t
  Resultado$VPE_sin = 0
  
  for(k in 1:n){
    R <- Resultado$v_venc[1:(n-k+1)] *Resultado$FE_sin[k:n]
    Resultado$VPE_sin[k] <- sum(R)
  }
  if(P1$Producto == "Temporal"){
    Resultado$Dotal = 0
    Resultado$v_100 = 0
    Resultado$SADotal = 0
    Resultado$VPE_dotal = 0
  }else{
    Resultado$Dotal = 0
    Resultado$v_100 = 1
    for(k in 1:n){
      R <-prod(Resultado$p[k:n]) 
      Resultado$Dotal[k] <- R
      Resultado$SADotal[k] <- Resultado$Dotal[k] * Resultado$SA[k]
      Resultado$v_100[k] = (Resultado$v[k])^(n-k)
      #Resultado$VPE_dotal[k] <- R
    }
    Resultado$VPE_dotal = Resultado$SADotal*Resultado$v_100
  }
  for(k in 1:n){
    R <- Resultado$v_venc[1:(n-k+1)] *
      Resultado$FE_sin[k:n]
    Resultado$VPE_sin[k] <- sum(R)
  }
  Resultado$FE_gtos = (Resultado$GA + Resultado$CA)*Resultado$PT* Resultado$p_tau
  Resultado$VPE_gtos = 0
  for(k in 1:n){
    R <- Resultado$v_ant[1:(n-k+1)] *
      Resultado$FE_gtos[k:n]
    Resultado$VPE_gtos[k] <- sum(R)
  }
  Resultado$FE_ingresos = Resultado$PT* Resultado$p_tau
  Resultado$VPE_ingresos = 0
  for(k in 1:n){
    R <- Resultado$v_ant[1:(n-k+1)] *
      Resultado$FE_ingresos[k:n]
    Resultado$VPE_ingresos[k] <- sum(R)
  }
  return(Resultado)
}
##Función 2
######Funcion que aplica un corte a la reserva completa en la edad+t donde
#t es la valuación y se recupera el renglón específico correspondiente
#Requiere la función BEL anterior 
#Recibe los mismos parámetros
BEL_corte = function(P1,tabla,corte, anual = 0,tasa = ruta){
  valuacion = corte
  t = round(time_length(interval(P1$`Inicio de vigencia`,valuacion),"years"),2)
  edad = P1$Edad
  val = edad + t
  l = list()
  D = BEL(P1,tabla,corte, anual,tasa)
  suppressMessages({ D = D  %>%
    filter(Edad <= val) %>%
    slice_max(Edad, n = 1)})
  return(D)
}
##Función 3
######Funcion que recibe una tabla con un conjunto de pólizas, output
##del"Modulo_1_Tarificacion.R" y calcula a fecha de valuación el valor de cada
##uno de los BEL's para cada póliza. Recibe mismos parámetros que las funciones 
#anteriores a excepción de que recibe un Polisario completo.
Calcula_BEL <- function(Polisario, tabla, corte,anual =0,tasa = ruta){
  suppressMessages({ Resultado <- lapply(
    seq_len(nrow(Polisario)),
    function(i)
      BEL_corte(
        Polisario[i, , drop = FALSE],
        tabla,
        corte,
        anual,
        tasa
      )
  )})
  Resultado <- bind_rows(Resultado)
  Resultado = cbind(Polisario,Resultado)
  
  return(Resultado)
}
##Función 4
######Funcion que incorpora el cálculo de importes recuperables de reaseguro
###sobre prima de riesgo A_{x:n} no devengada.
BEL_IRR = function(Polisario, tabla,Percentil, corte,anual  =1,tasa = ruta){
  valuacion = corte
  Res = Calcula_BEL(Polisario, tabla, corte,anual,tasa)
  Res$BEL = Res$VPE_sin + Res$VPE_gtos + Res$VPE_dotal -Res$VPE_ingresos
  Res$Suma_cedida = ifelse(Res$`Suma Asegurada`>=2000000,Res$`Suma Asegurada`-2000000,Res$`Suma Asegurada`)
  Res$Porcentaje_cesion = Res$Suma_cedida/Res$`Suma Asegurada`
  Res$Reas_cal = 1-0.0005
  Dev = time_length(interval(valuacion,Res$fin_vigencia), "years")/time_length(interval(Res$`Inicio de vigencia`, Res$fin_vigencia), "years")
  Res$Fact_NDev = Dev
  Res$PRND = Res$VPE_sin
  Res$IRR =Res$PRND * Res$Reas_cal * Res$Porcentaje_cesion
  BEL_f =Res
  ####Desviacion
  tabla1 = Percentil
  Res1 = Calcula_BEL(Polisario, tabla1, corte,anual,tasa)
  Res1$BEL = Res1$VPE_sin + Res1$VPE_gtos + Res1$VPE_dotal -Res1$VPE_ingresos
  BEL_Final_Percentil = Res1
  BEL_f$BEL_PERCENTIL = Res1$BEL
  Desviacion_Final = BEL_Final_Percentil$BEL -BEL_f$BEL
  BEL_f$Desviacion = Desviacion_Final
  Desviacion_Final = sum(Desviacion_Final)
  BEL_f$Desviacion_Tot = Desviacion_Final
  return(BEL_f)
}
