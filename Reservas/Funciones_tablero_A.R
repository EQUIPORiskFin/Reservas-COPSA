library(lubridate)
library(dplyr)
library(readr)
library(readxl)
#######################################
###PRIMA DE TARIFA
###TABLA AMIS (edad 100)
######################################
###Método Preparar polisario (temporales)
####################
###################
Prepara_Polisario = function(Polisario,corte, Gastos,cancelacion){
  Polisario$fin_vigencia = 0
  for(i in 1:length(Polisario$Póliza)){
    P1 = Polisario[i,]
    if(!is.na(P1$Temporalidad)){
      P1$fin_vigencia = P1$Temporalidad
      P1$fin_vigencia = P1$`Inicio de vigencia` + years(P1$fin_vigencia)
    }else{
      m = max(tabla_CNSF_M_2013$Edad)
      P1$fin_vigencia = as.Date("9999-12-31")
    }
    Polisario$fin_vigencia[i]  =as.Date(P1$fin_vigencia, format = "%Y-%m-%d")
  }
  Polisario$fin_vigencia = as.Date(Polisario$fin_vigencia)
  Polisario$Edad = floor(time_length(interval(Polisario$`Fecha de nacimiento`,Polisario$`Inicio de vigencia`), "years"))
  Polisario = Polisario%>%filter(Polisario$`Fecha de emisión`<= corte & Polisario$`Inicio de vigencia`<= corte & corte<= Polisario$fin_vigencia)
  de = unique(Polisario$`Forma de pago`)
  da = c(1,4,2,12)
  Polisario$FPa = sapply(Polisario$`Forma de pago`,function(x){
    for(i in 1:length(de)){
      if(x == de[i]){
        return(da[i])
      }
    }})
  Polisario$prima_niv <- vapply(
    seq_len(nrow(Polisario)),
    function(i) Prima(Polisario[i, , drop = FALSE], tabla, tasa,cancelacion)[2],
    numeric(1)
  )
  Polisario$prima_riesgo <- vapply(
    seq_len(nrow(Polisario)),
    function(i) Prima(Polisario[i, , drop = FALSE], tabla, tasa,cancelacion)[1],
    numeric(1)
  )
  Polisario = Polisario%>%left_join(Gastos,by = c("Producto"="Tipo_seguro","Temporalidad"="Plazo"))
   return(Polisario)
}
Calcula_PT = function(Polisario,tabla, tasa,CA){
  Polisario$PT_U <- vapply(
    seq_len(nrow(Polisario)),
    function(i) Prima_tarifa(Polisario[i, , drop = FALSE], tabla, tasa,CA)[1],
    numeric(1)
  )
  Polisario$PT_Niv <- vapply(
    seq_len(nrow(Polisario)),
    function(i) Prima_tarifa(Polisario[i, , drop = FALSE],tabla, tasa,CA)[2],
    numeric(1)
  )
  Polisario$CA <- vapply(
    seq_len(nrow(Polisario)),
    function(i) Prima_tarifa(Polisario[i, , drop = FALSE],tabla, tasa,CA)[3],
    numeric(1)
  )
  Polisario$RF <- 500
  return(Polisario)
}
###########################
#####Método 2 Método Prima
##################
Prima = function(P1,tabla,tasa,cancelacion){
  edad = P1$Edad
  Auxiliar = tabla
  crt = P1$Temporalidad
  #Auxiliar2 = cancelacion$Nacional 
  desfase = edad + crt
  if(!is.na(crt)){
    Auxiliar = Auxiliar%>%filter(Edad<=desfase)
    #Auxiliar2 = Auxiliar2[1:(crt+1)] 
  }
  Auxiliar$tasa = tasa
  Auxiliar$v = 1/(1+Auxiliar$tasa)
  Auxiliar = Auxiliar%>%filter(Auxiliar$Edad>=edad)
  #Auxiliar$c  = Auxiliar2
  #Auxiliar$c = .1
  #Auxiliar2 = data.frame(c = Auxiliar2)
  #Auxiliar2$p = (1-Auxiliar$c) * Auxiliar$p
  #Auxiliar2$q = 1-Auxiliar2$p
  #Auxiliar$p = (1-Auxiliar$c) * Auxiliar$p
  #Auxiliar$q = 1-Auxiliar$p
  #Auxiliar2$tasa = tasa
  #Auxiliar2$v = 1/(1+Auxiliar2$tasa)
  n=length(Auxiliar$q)-1
  nn = n
  n = n-1
  Auxiliar$p_t =1
  for(k in 2:nn){
    Auxiliar$p_t[k] = Auxiliar$p_t[k-1]*Auxiliar$p[k-1]
  }
  K=Auxiliar
  Auxiliar = Auxiliar%>%filter(Edad<desfase)
  Auxiliar$tq_x = Auxiliar$p_t*Auxiliar$q
  #Auxiliar2$tq_x = Auxiliar2$p_t*Auxiliar2$q
  Auxiliar$v_ant = mapply(function(x,y){y^x},x = seq(from = 0, to = n), y = Auxiliar$v)
  m = P1$FPa
  Auxiliar$v_venc = mapply(function(x,y){y^(x+1)},x = seq(from = 0, to = n), y = Auxiliar$v)
  #Auxiliar2$v_venc = mapply(function(x,y){y^x},x = seq(from = 1, to = n), y = Auxiliar2$v)
  Auxiliar$a_ant = sum(Auxiliar$p_t*(Auxiliar$v_ant))
  #-(((m-1)/(2*m))*(1-Auxiliar$v_venc[nn]*Auxiliar$p_t[nn]))
  #Auxiliar$prima = sum(Auxiliar2$tq_x*Auxiliar2$v_venc)
  Auxiliar$prima = sum(Auxiliar$tq_x*Auxiliar$v_venc)
  Auxiliar$prima_Niv = Auxiliar$prima/Auxiliar$a_ant
  Auxiliar$Prima = Auxiliar$prima * P1$`Suma Asegurada`
  Auxiliar$PNIVELADA = Auxiliar$prima_Niv * P1$`Suma Asegurada`
  a = unique(Auxiliar$Prima)
  b = unique(Auxiliar$PNIVELADA)
  return(c(a,b))
}
###Métodos auxiliares
#####################
############################AQUI ME QUEDÉ
Prima_tarifa = function(P1,tabla,tasa,CA){
  tem = P1$Temporalidad-6
  Cost1 = c(CA, rep(CA[6],times = tem))
  Costo  = c(CA, rep(CA[6],times = tem))
  Costo = Costo + (P1$GA + P1$MU)
  Factor = 1-Costo
  Factor = 1/Factor
  Primas = P1$prima_niv*Factor 
  #Primas[1] = Primas[1] + 500
  CostoAdq = Cost1 * Primas
  Sum_Primas = sum(Primas)
  Sum_Costos = sum(CostoAdq)
  CA1 = Sum_Costos/Sum_Primas
  factor = 1/(1-(P1$GA + CA1 + P1$MU))
  P = (factor*P1$prima_niv) 
  P2 = factor*P1$prima_riesgo
  return(c(P2,P,CA1))
}
######################################################
#####################################################
####Método 3 Método Preparar polisario (vitalicios y dotales)
Prepara_Polisario_d = function(Polisario,corte, Gastos,cancelacion){
  Polisario$fin_vigencia = 0
  for(i in 1:length(Polisario$Póliza)){
    P1 = Polisario[i,]
    if(!is.na(P1$Temporalidad)){
      P1$fin_vigencia = P1$Temporalidad
      P1$fin_vigencia = P1$`Inicio de vigencia` + years(P1$fin_vigencia)
    }else{
      m = max(tabla_CNSF_M_2013$Edad)
      P1$fin_vigencia = as.Date("9999-12-31")
    }
    Polisario$fin_vigencia[i]  =as.Date(P1$fin_vigencia, format = "%Y-%m-%d")
  }
  Polisario$fin_vigencia = as.Date(Polisario$fin_vigencia)
  Polisario$Edad = floor(time_length(interval(Polisario$`Fecha de nacimiento`,Polisario$`Inicio de vigencia`), "years"))
  Polisario = Polisario%>%filter(Polisario$`Fecha de emisión`<= corte & Polisario$`Inicio de vigencia`<= corte & corte<= Polisario$fin_vigencia)
  de = unique(Polisario$`Forma de pago`)
  da = c(1,4,2,12)
  Polisario$FPa = sapply(Polisario$`Forma de pago`,function(x){
    for(i in 1:length(de)){
      if(x == de[i]){
        return(da[i])
      }
    }})
  Polisario$prima_niv <- vapply(
    seq_len(nrow(Polisario)),
    function(i) Prima_d(Polisario[i, , drop = FALSE], tabla, tasa,cancelacion)[2],
    numeric(1)
  )
  Polisario$prima_riesgo <- vapply(
    seq_len(nrow(Polisario)),
    function(i) Prima_d(Polisario[i, , drop = FALSE], tabla, tasa,cancelacion)[1],
    numeric(1)
  )
  
  Polisario$Dotal <- vapply(
    seq_len(nrow(Polisario)),
    function(i) Prima_d(Polisario[i, , drop = FALSE], tabla, tasa,cancelacion)[3],
    numeric(1)
  )
  
  Polisario = Polisario%>%left_join(Gastos,by = c("Producto"="Tipo_seguro","Temporalidad"="Plazo"))
  return(Polisario)
}
Calcula_PT_d = function(Polisario,tabla, tasa,CA){
  Polisario$PT_U <- vapply(
    seq_len(nrow(Polisario)),
    function(i) Prima_tarifa_d(Polisario[i, , drop = FALSE], tabla, tasa,CA)[1],
    numeric(1)
  )
  Polisario$PT_Niv <- vapply(
    seq_len(nrow(Polisario)),
    function(i) Prima_tarifa_d(Polisario[i, , drop = FALSE],tabla, tasa,CA)[2],
    numeric(1)
  )
  Polisario$CA <- vapply(
    seq_len(nrow(Polisario)),
    function(i) Prima_tarifa_d(Polisario[i, , drop = FALSE],tabla, tasa,CA)[3],
    numeric(1)
  )
  Polisario$RF <- 500
  return(Polisario)
}
###################################################
###################################################
####Método 4 Prima_d (dotales y vitalicio)
Prima_d = function(P1,tabla,tasa,cancelacion){
  edad = P1$Edad
  Auxiliar = tabla
  crt = P1$Temporalidad
  #Auxiliar2 = cancelacion$Nacional 
  desfase = edad + crt
  if(!is.na(crt)){
    Auxiliar = Auxiliar%>%filter(Edad<=desfase)
    #Auxiliar2 = Auxiliar2[1:(crt+1)] 
  }
  Auxiliar$tasa = tasa
  Auxiliar$v = 1/(1+Auxiliar$tasa)
  Auxiliar = Auxiliar%>%filter(Auxiliar$Edad>=edad)
  n=length(Auxiliar$q)
  nn = n-1
  Auxiliar$p_t =1
  for(k in 2:n){
    Auxiliar$p_t[k] = Auxiliar$p_t[k-1]*Auxiliar$p[k-1]
  }
  K=Auxiliar
  d = length(Auxiliar$v)
  if(!is.na(crt)){
  Auxiliar = Auxiliar%>%filter(Edad<desfase)
  nn = nn-1
  }else{Auxiliar = Auxiliar%>%filter(Edad<100)
  nn = nn-1
  }
  Auxiliar$tq_x = Auxiliar$p_t*Auxiliar$q
  #Auxiliar2$tq_x = Auxiliar2$p_t*Auxiliar2$q
  Auxiliar$v_ant = mapply(function(x,y){y^x},x = seq(from = 0, to = nn), y = Auxiliar$v)
  m = P1$FPa
  Auxiliar$v_venc = mapply(function(x,y){y^(x+1)},x = seq(from = 0, to = nn), y = Auxiliar$v)
  #Auxiliar2$v_venc = mapply(function(x,y){y^x},x = seq(from = 1, to = n), y = Auxiliar2$v)
  Auxiliar$a_ant = sum(Auxiliar$p_t*(Auxiliar$v_ant))
  #-(((m-1)/(2*m))*(1-Auxiliar$v_venc[nn]*Auxiliar$p_t[nn]))
  #Auxiliar$prima = sum(Auxiliar2$tq_x*Auxiliar2$v_venc)
  Auxiliar$prima = sum(Auxiliar$tq_x*Auxiliar$v_venc)
  VP = 0.06
  VP = 1/(1+VP)
  VP = ((VP)^(n-1))*(K$p_t[n-1])
  parte_dotal = VP
  Auxiliar$prima = parte_dotal + Auxiliar$prima
  Auxiliar$prima_Niv = Auxiliar$prima/Auxiliar$a_ant
  Auxiliar$Prima = Auxiliar$prima * P1$`Suma Asegurada`
  Auxiliar$PNIVELADA = Auxiliar$prima_Niv * P1$`Suma Asegurada`
  a = unique(Auxiliar$Prima)
  b = unique(Auxiliar$PNIVELADA)
  return(c(a,b,parte_dotal))
}
#####################################################
#####################################################
###Métodos auxiliares
#####################
Prima_tarifa_d = function(P1,tabla,tasa,CA){
  crt = P1$Temporalidad
  if(is.na(crt)){
    tabla2 = tabla%>%filter(tabla$Edad>=P1$Edad)
  tem = length(tabla2$q)-7
  }else{tem = crt-6}
  Cost1 = c(CA, rep(CA[6],times = tem))
  Costo  = c(CA, rep(CA[6],times = tem))
  Costo = Costo + (P1$GA + P1$MU)
  Factor = 1-Costo
  Factor = 1/Factor
  Primas = P1$prima_niv*Factor 
  #Primas[1] = Primas[1] + 500
  CostoAdq = Cost1 * Primas
  Sum_Primas = sum(Primas)
  Sum_Costos = sum(CostoAdq)
  CA1 = Sum_Costos/Sum_Primas
  factor = 1/(1-(P1$GA + CA1 + P1$MU))
  P = (factor*P1$prima_niv) 
  P2 = factor*P1$prima_riesgo
  return(c(P2,P,CA1))
}
