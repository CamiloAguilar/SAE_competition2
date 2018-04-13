#**********************************************************************************************************************
#*****************************************      SAE Competition 2      ************************************************ 
#**********************************************************************************************************************
library(sae); library(survey); library(readxl);library(dplyr)
options(survey.lonely.psu = "adjust")
options(scipen=999)
rm(list = ls())

## parámetros de interés:
##  * Promedio del ingreso 
##  * Proporción de desempleados por municipio.
## POBLACION: Numero de personas por municipio año 2014.  

## setwd("C:/Users/Camilo/Documents/GitHub/SAE_competition2")

muestra_multi <- readRDS("./data/df.RDS")
muestra_multi <- muestra_multi[,c(1,10,9,2:8,11:length(names(muestra_multi)))]
muestra_multi <- muestra_multi %>% select(-(POBLACION2014))  
muestra_multi$densidad_pop <- muestra_multi$POBLACION / muestra_multi$AREA_KM2
muestra_multi$Biblio_per <- muestra_multi$POBLACION / muestra_multi$NUM_BIBLIOTECAS
muestra_multi$Biblio_per[is.infinite(muestra_multi$Biblio_per)]<-0
muestra_multi$Tasa_secuestros <- muestra_multi$PERSONAS_SECUESTRADAS / muestra_multi$POBLACION
muestra_multi$Tasa_recibidos <- muestra_multi$PERSONAS_RECIBIDOS / muestra_multi$POBLACION
muestra_multi$meansByMun <- muestra_multi$meansByMun/1000
muestra_multi$varmeansByMun <- muestra_multi$varmeansByMun/(1000^2)

# Datos poblacionales
Municipios <- as.data.frame(read_excel("./data/CONSOLIDADO_MPIOS_CUNDINAMARCA.xlsx"))
Municipios$densidad_pop <- Municipios$POBLACION / Municipios$AREA_KM2
Municipios$Biblio_per <- Municipios$POBLACION / Municipios$NUM_BIBLIOTECAS
Municipios$Biblio_per[is.infinite(Municipios$Biblio_per)]<-0
Municipios$Tasa_secuestros <- Municipios$PERSONAS_SECUESTRADAS / Municipios$POBLACION
Municipios$Tasa_recibidos <- Municipios$PERSONAS_RECIBIDOS / Municipios$POBLACION
Municipios$meansByMun <- Municipios$meansByMun/1000
Municipios$varmeansByMun <- Municipios$varmeansByMun/(1000^2)


names(muestra_multi)

#*****************************************************************************
## Estimador de Fay - Herriot ####
#*****************************************************************************

## Tenemos un prblema de 31 observaciones y 44 variables (+ 1 var respuesta)
dim(muestra_multi)

## Selección de variables
m_corr <- data.frame(meansByMun=t(cor(muestra_multi$meansByMun, muestra_multi[,11:55])))
m_corr$feature <- row.names(m_corr)
m_corr$meansByMun <- abs(round(m_corr$meansByMun, 1))

m_corr <- m_corr %>% 
          filter(feature %in% names(Municipios)) %>%
          arrange(desc(abs(meansByMun))) #%>% 
          #select(feature, meansByMun) %>%
          #group_by(meansByMun) %>%
          #summarise(feature=last(feature))
m_corr

var_candidates <- c("CONSUMO_ENERGIA_PER_HABIT", "COBER_MEDIA_TASA", "NBI_RURAL_2010", 
                    "DEPENDENCIA_REGALIAS_TRANSFERENCIAS",
                    "TASA_AFILIADOS_CONTRIBUTIVO", "COBER_SECUNDARIA_TASA", "TASA_HOMICIDIOS",
                    "Tasa_recibidos", "IND_CAPACIDAD_ADMINISTRATIVA", "IND_INTEGRAL_MCIPAL",
                    "TASA_VACUN_BCG", "densidad_pop", "IND_EFICIENCIA_MUNICIPAL","IND_EFICACIA_MUNICIPAL",
                    "TASA_VACUN_POLIO", "TASA_VACUN_DPT", "Tasa_secuestros") #m_corr$feature
#var_candidates <- m_corr$feature[1:29]

# Exploración 
modelocompleto <- paste0("meansByMun", " ~ ", paste(var_candidates, collapse = " + "))
modelocompleto <- as.formula(modelocompleto)

modelo_optimo <- step(lm(modelocompleto, data = muestra_multi))
modelo_optimo$call
modelo_optimo <- names(modelo_optimo$coefficients)[2:length(names(modelo_optimo$coefficients))]
formula_var <- paste0("meansByMun", " ~ ", paste(modelo_optimo, collapse = " + "))

modelo_reducido <- lm(formula_var, data = muestra_multi)
(p<-summary(modelo_reducido))
var_selec <- names(which(p$coefficients[,4]<=0.05))
formula_var <- paste0("meansByMun", " ~ ", paste(var_selec, collapse = " + "))

modelo_reducido2 <- lm(formula_var, data = muestra_multi)
summary(modelo_reducido2)
var_sel <- names(modelo_reducido2$coefficients)[2:length(names(modelo_reducido2$coefficients))]
#var_sel <- modelo_optimo

#################################################################
formula_var <- as.formula(paste0("meansByMun", " ~ ", paste(var_sel, collapse = " + ")))
FH_prommat <- mseFH(formula_var,  varmeansByMun, data = muestra_multi)
#FH_prommat <- mseFH(meansByMun ~ COBER_MEDIA_TASA + 
#                         NBI_2010 + NBI_URBANO_2010 + AREA_KM2 + ESFUERZO_FISCAL + 
#                         TASA_DELITOS_SEXUALES + TASA_HOMICIDIOS + Tasa_recibidos + 
#                         DESMINADO_MILITAR_OPERACIONES + IND_CUMPLIMIENTO_REQUI_LEGALES + 
#                         IND_INTEGRAL_MCIPAL + PERSONAS_RECIBIDOS,  varmeansByMun, data = muestra_multi)
FH_prommat
#var_sel <- c("COBER_MEDIA_TASA", "NBI_2010", "NBI_URBANO_2010", "AREA_KM2", "ESFUERZO_FISCAL", 
#             "TASA_DELITOS_SEXUALES", "TASA_HOMICIDIOS", "Tasa_recibidos", "DESMINADO_MILITAR_OPERACIONES",
#             "IND_CUMPLIMIENTO_REQUI_LEGALES", "IND_INTEGRAL_MCIPAL", "PERSONAS_RECIBIDOS")

datos_ingreso <- muestra_multi
datos_ingreso$Y_FH <- FH_prommat$est$eblup
datos_ingreso$mseY_FH <- FH_prommat$mse
datos_ingreso$cve_FH <- FH_prommat$mse / FH_prommat$est$eblup * 100
datos_ingreso$cve_dir <- sqrt(datos_ingreso$varmeansByMun) / datos_ingreso$meansByMun * 100
Resultados_sd <- datos_ingreso[c("ID_MUNIC","MCPIO","meansByMun","cve_dir", "Y_FH", "mseY_FH","cve_FH")]
names(Resultados_sd)[3] <- "Y_dir"

mpiossel <- unique(muestra_multi$ID_MUNIC)
mpiosnosel <- Municipios$ID_DANE[!(Municipios$ID_DANE %in% mpiossel)]
length(mpiossel) + length(mpiosnosel)

# Hacer las estimaciones del promedio para los dominios no observados en la muestra
X_rd <- Municipios %>% filter(ID_DANE %in% mpiosnosel) %>%
        select_(.dots = var_sel)
head(X_rd)

X_rd$Unos <- 1
X_rd <- X_rd %>% select_(.dots = c("Unos", var_sel))

X_rd <- as.matrix(X_rd)
row.names(X_rd) <- Municipios[Municipios$ID_DANE %in% mpiosnosel,]$ID_DANE
Beta <- as.matrix(FH_prommat$est$fit$estcoef$beta)
YFH_rd <- X_rd %*% Beta # Estimación para los dominios no observados
YFH_rd <- as.data.frame(YFH_rd)
YFH_rd$ID_DANE <- Municipios[Municipios$ID_DANE %in% mpiosnosel, ]$ID_DANE
colnames(YFH_rd)[1] <- "Y_FH_rd"                             


# Error cuadrático medio para los municipios no observados
# \sigma_u ^2 + 
sigma2_u <- FH_prommat$est$fit$refvar

# Información auxiliar en la muestra
X_sd <- Municipios %>% filter(ID_DANE %in% mpiossel) %>%
        select_(.dots = var_sel)
X_sd$Unos <- 1
X_sd <- X_sd %>% select_(.dots = c("Unos", var_sel))
X_sd <- as.matrix(X_sd)

# \hat{V}(\hat{\boldsymbol{\beta}}): varianza de los parámetros
V_beta_est <- t(X_sd) %*% diag(sigma2_u + datos_ingreso$varmeansByMun) %*% X_sd  # 24 X 1

# Error cuadrático para dominios no observados
MSE_Y.FH <- X_rd %*% solve(V_beta_est) %*% t(X_rd) + sigma2_u
MSE_Y.FH <- diag(MSE_Y.FH)
MSE_Y.FH <- as.data.frame(as.table(MSE_Y.FH))
colnames(MSE_Y.FH) <- c("ID_DANE", "MSE_Y_FH_rd")

Resultados_rd <- merge(YFH_rd, MSE_Y.FH, by = "ID_DANE")
# Pegarle el municipio a Resultados_rd
nombres <- Municipios %>% group_by(ID_DANE, MCPIO) %>% summarise(temp = n()) %>% select(-(temp))
Resultados_rd <- merge(Resultados_rd, nombres, by = "ID_DANE")

# Combinarla con las estimaciones de los municipios observados
names(Resultados_sd); names(Resultados_rd)
names(Resultados_sd)[c(1, 5, 6, 7)] <- c("ID_DANE","Y_FH_sd", "MSE_Y_FH_sd", "cve_FH_sd")

Resultados_sd$TipologiaDominio  <- "Observado"
Resultados_rd$TipologiaDominio  <- "No observado"
Resultados <- bind_rows(Resultados_sd, Resultados_rd )
Resultados$Y_FH <- ifelse(Resultados$TipologiaDominio == "Observado",
                          Resultados$Y_FH_sd, Resultados$Y_FH_rd)

Resultados$MSE_Y_FH <- ifelse(Resultados$TipologiaDominio == "Observado", 
                              Resultados$MSE_Y_FH_sd, Resultados$MSE_Y_FH_rd)
Resultados$cve_FH <- sqrt(Resultados$MSE_Y_FH) / Resultados$Y_FH * 100

ResultadosFinales <- Resultados[c("TipologiaDominio","ID_DANE", "MCPIO", "Y_dir", "cve_dir", "Y_FH",
                                  "MSE_Y_FH", "cve_FH")]
View(ResultadosFinales)
ResultadosFinales <- arrange(ResultadosFinales, -Y_dir)
ResultadosFinales$Ranking <- 1:nrow(ResultadosFinales)

mean(ResultadosFinales$cve_FH)


