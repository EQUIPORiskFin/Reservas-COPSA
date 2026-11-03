########Archivo EREA RR4
#######Author Act Alejandro García de León
EREA = function(Base){
  #Plantilla_Contratos <- read_csv("C:/Users/agarciadeleon/R_Studio/CSV/proyecto/RR4/Gastos/Plantillas/Plantilla_Contratos.csv")
  Plantilla_Contratos <- read_csv(ruta)
  Esquemas_reaseguro <- read_csv(ruta1)
  Esquemas_reaseguro$cve = as.character(Esquemas_reaseguro$cve)
  Base = Base%>%left_join(Esquemas_reaseguro, by = c("cve_reas" = "cve", "anio" = "anio"))
  #Base$clave_esq1 = ifelse(Base$Clave_esquema3==0,ifelse(Base$Clave_esquema2==0,Base$Clave_esquema1,Base$Clave_esquema2),Base$Clave_esquema3)
  EREA = list()
  we12 = unique(Base$cve_reas)
  for(i in 1:length(we12)){
    esq = sort(unique(Base$Clave_esquema1.y[Base$cve_reas == we12[i]]))
    esq1 = sort(unique(Base$Clave_esquema2.y[Base$cve_reas == we12[i]]))
    esq2 = sort(unique(Base$Clave_esquema3.y[Base$cve_reas == we12[i]]))
    f = list()
    f1 = list()
    f2 = list()
    f3 = list()
    f4 = list()
    f5 = list()
    f6 = list()
    f7 = list()
    f8 = list()
    f9 = list()
    #Se filtra la base por cada tipo de reaseguro
    WEE = Base%>%filter(cve_reas ==we12[i])
    #Se añade el anio de emisión
    d = sort(unique(year(WEE$`Fecha de emisión`)))
    ####Se almacenan los años de emisión
    ####
    A1 = d
    Cap = Plantilla_Contratos$Capa[Plantilla_Contratos$cve_reas == we12[i]]
    num = Plantilla_Contratos$num[Plantilla_Contratos$cve_reas == we12[i]]
    if(Plantilla_Contratos$num[Plantilla_Contratos$cve_reas == we12[i]]>1){
      C1 = trimws(unlist(strsplit(as.character(Plantilla_Contratos$Sufijo[Plantilla_Contratos$cve_reas == we12[i]]), ";")))
      C1_org <- C1
      C1 = as.vector(outer(C1, d, paste0))
      d  <- sort(rep(d, times = length(C1_org)))
      if( Plantilla_Contratos$Esq_ind[Plantilla_Contratos$cve_reas == we12[i]]==0){
      esq = sort(rep(esq, times = length(C1_org)))}else{
        esq = c(esq,esq1)
        esq = sort(esq)
      }
      A1 = d
      ESQ =  esq
    }else{
      C1 = paste0(Plantilla_Contratos$Sufijo[Plantilla_Contratos$cve_reas == we12[i]],d)
      ESQ = esq
     }
    if(Plantilla_Contratos$Capa[Plantilla_Contratos$cve_reas == we12[i]]>1){
      A21 = list()
      for(j in 1:Plantilla_Contratos$Capa[Plantilla_Contratos$cve_reas == we12[i]]){
      A21[[j]] = paste0(C1,"_",j)
      A1 = rep(d, times = Cap)
      ESQ = rep(esq, times = Cap)
      }
      C1 = unlist(A21)
      C1 = sort(C1)
      A1 = sort(A1)
      ESQ = sort(ESQ)
      }else{
        A21 = list()
        for(j in 1:Plantilla_Contratos$Capa[Plantilla_Contratos$cve_reas == we12[i]]){
          A21[[j]] = C1
          C1 = unlist(A21)
          A1 = d
          ESQ = esq
        }
        C1 = unlist(A21)}
    #######Se concatenan
    D1 = as.numeric(trimws(unlist(strsplit(as.character(Plantilla_Contratos$Cob[Plantilla_Contratos$cve_reas == we12[i]]), ";"))))
    D1 = rep(D1, times = (length(C1)/(Cap*num)))
    RPrio1 = as.numeric(trimws(unlist(strsplit(as.character(Plantilla_Contratos$Prioridad[Plantilla_Contratos$cve_reas == we12[i]]), ";"))))
    RPrio1 = rep(RPrio1, times = (length(C1)/(Cap*num)))
    RPrio1 = sapply(RPrio1, millones_a_importe)
    Ret_adic1 = as.numeric(trimws(unlist(strsplit(as.character(Plantilla_Contratos$Ret_adic[Plantilla_Contratos$cve_reas == we12[i]]), ";"))))
    Ret_adic1  = rep(Ret_adic1 , times = (length(C1)/(Cap*num))) 
    Ret_adic1 = sapply(Ret_adic1, millones_a_importe)
    CapL_1 = as.numeric(trimws(unlist(strsplit(as.character(Plantilla_Contratos$CapL_1[Plantilla_Contratos$cve_reas == we12[i]]), ";"))))
    CapL_1  = rep(CapL_1 , times = (length(C1)/(Cap*num))) 
    CapL_1 = sapply(CapL_1, millones_a_importe)
    CadpDiR_1 = as.numeric(trimws(unlist(strsplit(as.character(Plantilla_Contratos$CadpDiR_1[Plantilla_Contratos$cve_reas == we12[i]]), ";"))))
    CadpDiR_1  = rep(CadpDiR_1 , times = (length(C1)/(Cap*num))) 
    CadpDiR_1 = sapply(CadpDiR_1, millones_a_importe)
    ########
    ##Trabajo de llaves
    num = Plantilla_Contratos$num[Plantilla_Contratos$cve_reas == we12[i]]
    if(num>1){
    cadena <- Plantilla_Contratos$llaveD_1[
      Plantilla_Contratos$cve_reas == we12[i]
    ]
    
    partes <- strsplit(cadena, "\\|")[[1]]
    
    vecs <- lapply(partes, function(z)
      as.numeric(strsplit(z, ";")[[1]])
    )
    
    llaveD_1 <- Map(c, vecs[[1]], vecs[[2]])
    ################3
    cadena <- Plantilla_Contratos$llaveR_1[
      Plantilla_Contratos$cve_reas == we12[i]
    ]
    
    partes <- strsplit(cadena, "\\|")[[1]]
    
    vecs <- lapply(partes, function(z)
      as.numeric(strsplit(z, ";")[[1]])
    )
    
    llaveR_1 <- Map(c, vecs[[1]], vecs[[2]])
    }else{
      partes = Plantilla_Contratos$llaveD_1[
        Plantilla_Contratos$cve_reas == we12[i]
      ]
      
      vecs <- lapply(partes, function(z)
        as.numeric(strsplit(z, ";")[[1]])
      )
      
      llaveD_1 <- Map(c, vecs[[1]])
      ################3
      
      
      partes <- Plantilla_Contratos$llaveR_1[
        Plantilla_Contratos$cve_reas == we12[i]
      ]
      
      vecs <- lapply(partes, function(z)
        as.numeric(strsplit(z, ";")[[1]])
      )
      
      llaveR_1 <- Map(c, vecs[[1]]) 
    }
    #llaveD_1 = rep(Plantilla_Contratos$llaveD_1[Plantilla_Contratos$cve_reas == we12[i]], times = length(A1))
    #llaveR_1 = rep(Plantilla_Contratos$llaveR_1[Plantilla_Contratos$cve_reas == we12[i]], times = length(A1))
    f[[i]] = C1
    f1[[i]] = A1
    f2[[i]] = D1
    f3[[i]] = RPrio1
    f4[[i]] = Ret_adic1
    f5[[i]] = CapL_1
    f6[[i]] = CadpDiR_1
    f7[[i]] = llaveD_1
    f8[[i]] = llaveR_1
    EREA1 = data.frame(`Identificador de Contrato` = unlist(f), 
                      Año  =unlist(f1), `Tipo de Cobertura` = unlist(f2),
                      `Retención/Prioridad` = unlist(f3), 
                      `Capacidad/Límite` = unlist(f5), 
                      `Retenciones Adicionales` = unlist(f4),
                      `Capacidad Disponible y Reinstalaciones` = unlist(f6),
                      `Llave Vinculación Contrato Directo` = unlist(f7), 
                      `Llave Vinculación Contrato Retenido` = unlist(f8),
                        Esquema  =ESQ)
    if(i==1){
      EREA[[i]] = EREA1 
    }else{
    EREA[[i]] = rbind(EREA[[i-1]],EREA1)}
    EREA2 = EREA[[i]]
  }
  return(EREA2)}
#Y = EREA(Base)
#write.xlsx(Y, "D:/Carp/CSV/proyecto/RR4/EREA.xlsx")
