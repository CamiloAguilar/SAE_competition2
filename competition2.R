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
muestra_multi$AVALUOS_CATASTRALES_RURALES <- muestra_multi$AVALUOS_CATASTRALES_RURALES/1000
muestra_multi$AVALUOS_CATASTRALES_URBANOS <- muestra_multi$AVALUOS_CATASTRALES_URBANOS/1000
 
# Datos poblacionales
Municipios <- as.data.frame(read_excel("./data/CONSOLIDADO_MPIOS_CUNDINAMARCA.xlsx"))
Municipios$densidad_pop <- Municipios$POBLACION / Municipios$AREA_KM2
Municipios$Biblio_per <- Municipios$POBLACION / Municipios$NUM_BIBLIOTECAS
Municipios$Biblio_per[is.infinite(Municipios$Biblio_per)]<-0
Municipios$Tasa_secuestros <- Municipios$PERSONAS_SECUESTRADAS / Municipios$POBLACION
Municipios$Tasa_recibidos <- Municipios$PERSONAS_RECIBIDOS / Municipios$POBLACION
Municipios$AVALUOS_CATASTRALES_RURALES <- Municipios$AVALUOS_CATASTRALES_RURALES/1000
Municipios$AVALUOS_CATASTRALES_URBANOS <- Municipios$AVALUOS_CATASTRALES_URBANOS/1000


names(muestra_multi)

#*****************************************************************************
## 1. Promedio del ingreso ####
#*****************************************************************************

#*********************************
## Estimador de Fay - Herriot 
#*********************************

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

#********************************************************************************************
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


#*****************************************************************************
## 2. Proporción de desempleados por municipio ####
#*****************************************************************************

## Tenemos un prblema, no hay correlación entre variable objetivo y las regresoras...
dim(muestra_multi)
pairs(unempByMun ~ AREA_KM2 + TASA_AFILIADOS_CONTRIBUTIVO + TASA_AFILIADOS_SUBSIDIADO + 
           CONSUMO_ENERGIA_PER_HABIT + DEPENDENCIA_REGALIAS_TRANSFERENCIAS + 
           TASA_DELITOS_SEXUALES + AVALUOS_CATASTRALES_RURALES + COBER_TRANSICION_TASA + 
           NBI_2010 + NBI_RURAL_2010 + COBER_MEDIA_TASA + COBER_PRIMARIA_TASA + 
           COBER_SECUNDARIA_TASA,
      data=muestra_multi)

## Selección de variables
m_corr <- data.frame(unempByMun=t(cor(muestra_multi$unempByMun, muestra_multi[,11:55])))
m_corr$feature <- row.names(m_corr)
m_corr$unempByMun <- abs(round(m_corr$unempByMun, 1))

m_corr <- m_corr %>% 
     filter(feature %in% names(Municipios)) %>%
     arrange(desc(abs(unempByMun))) #%>% 
#     select(feature, unempByMun) %>%
#     group_by(unempByMun) %>%
#     summarise(feature=last(feature))
m_corr

var_candidates <- c("AREA_KM2", "TASA_AFILIADOS_CONTRIBUTIVO", "CONSUMO_ENERGIA_PER_HABIT",
                    "DEPENDENCIA_REGALIAS_TRANSFERENCIAS", "TASA_DELITOS_SEXUALES", "AVALUOS_CATASTRALES_RURALES",
                    "COBER_TRANSICION_TASA", "NBI_RURAL_2010", "COBER_MEDIA_TASA", "COBER_PRIMARIA_TASA",
                    "COBER_SECUNDARIA_TASA", "TASA_DESERCION_ESCOLAR") #m_corr$feature
#var_candidates <- m_corr$feature

# Exploración 
modelocompleto <- paste0("unempByMun", " ~ ", paste(var_candidates, collapse = " + "))
modelocompleto <- as.formula(modelocompleto)

modelo_optimo <- step(lm(modelocompleto, data = muestra_multi))
modelo_optimo$call
modelo_optimo <- names(modelo_optimo$coefficients)[2:length(names(modelo_optimo$coefficients))]
formula_var <- paste0("unempByMun", " ~ ", paste(modelo_optimo, collapse = " + "))

modelo_reducido <- lm(formula_var, data = muestra_multi)
(p<-summary(modelo_reducido))
var_selec <- names(which(p$coefficients[,4]<=0.05))
formula_var <- paste0("unempByMun", " ~ ", ifelse(var_selec[1]=="(Intercept)", 
                                                  paste(c(var_selec[2:length(var_selec)]), collapse = " + "),
                                                  paste( c(-1, var_selec), collapse = " + ")))

modelo_reducido2 <- lm(formula_var, data = muestra_multi)
summary(modelo_reducido2)
(p<-summary(modelo_reducido2))
var_selec <- names(which(p$coefficients[,4]<=0.05))
formula_var <- paste0("unempByMun", " ~ ", ifelse(var_selec[1]=="(Intercept)", 
                                                  paste(c(var_selec[2:length(var_selec)]), collapse = " + "),
                                                  paste( c(-1, var_selec), collapse = " + ")))

var_sel <- names(modelo_reducido2$coefficients)[2:length(names(modelo_reducido2$coefficients))]
#(var_sel <- var_selec)

#***********************************************************************************************
formula_var <- as.formula(paste0("unempByMun", " ~ ", paste(c(1,var_sel), collapse = " + ")))
FH_prommat <- mseFH(formula_var,  varunempByMun, data = muestra_multi)
#FH_prommat <- mseFH(unempByMun ~ COBER_MEDIA_TASA + 
#                         NBI_2010 + NBI_URBANO_2010 + AREA_KM2 + ESFUERZO_FISCAL + 
#                         TASA_DELITOS_SEXUALES + TASA_HOMICIDIOS + Tasa_recibidos + 
#                         DESMINADO_MILITAR_OPERACIONES + IND_CUMPLIMIENTO_REQUI_LEGALES + 
#                         IND_INTEGRAL_MCIPAL + PERSONAS_RECIBIDOS,  varmeansByMun, data = muestra_multi)
FH_prommat
#var_sel <- c("COBER_MEDIA_TASA", "NBI_2010", "NBI_URBANO_2010", "AREA_KM2", "ESFUERZO_FISCAL", 
#             "TASA_DELITOS_SEXUALES", "TASA_HOMICIDIOS", "Tasa_recibidos", "DESMINADO_MILITAR_OPERACIONES",
#             "IND_CUMPLIMIENTO_REQUI_LEGALES", "IND_INTEGRAL_MCIPAL", "PERSONAS_RECIBIDOS")

datos_desempleo <- muestra_multi
datos_desempleo$Y_FH <- FH_prommat$est$eblup
datos_desempleo$mseY_FH <- FH_prommat$mse
datos_desempleo$cve_FH <- FH_prommat$mse / FH_prommat$est$eblup * 100
datos_desempleo$cve_dir <- sqrt(datos_desempleo$varunempByMun) / datos_desempleo$unempByMun * 100
Resultados_sd_des <- datos_desempleo[c("ID_MUNIC","MCPIO","unempByMun","cve_dir", "Y_FH", "mseY_FH","cve_FH")]
names(Resultados_sd_des)[3] <- "Y_dir"

mpiossel <- unique(muestra_multi$ID_MUNIC)
mpiosnosel <- Municipios$ID_DANE[!(Municipios$ID_DANE %in% mpiossel)]
length(mpiossel) + length(mpiosnosel)

#***********************
## Dominios observados
#***********************
# Hacer las estimaciones del promedio para los dominios no observados en la muestra
X_rd <- Municipios %>% filter(ID_DANE %in% mpiosnosel) %>%
     select_(.dots = var_sel)
head(X_rd)

X_rd$Unos <- 1
X_rd <- X_rd %>% select_(.dots = c("Unos", var_sel))

X_rd <- as.matrix(X_rd)
head(X_rd)
row.names(X_rd) <- Municipios[Municipios$ID_DANE %in% mpiosnosel,]$ID_DANE
Beta <- as.matrix(FH_prommat$est$fit$estcoef$beta)

# Estimación para los dominios no observados
YFH_rd <- X_rd %*% Beta 
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
V_beta_est <- t(X_sd) %*% diag(sigma2_u + datos_desempleo$varunempByMun) %*% X_sd  # 24 X 1

# Error cuadrático para dominios no observados
MSE_Y.FH <- X_rd %*% solve(V_beta_est) %*% t(X_rd) + sigma2_u
MSE_Y.FH <- diag(MSE_Y.FH)
MSE_Y.FH <- as.data.frame(as.table(MSE_Y.FH))
colnames(MSE_Y.FH) <- c("ID_DANE", "MSE_Y_FH_rd")

Resultados_rd_des <- merge(YFH_rd, MSE_Y.FH, by = "ID_DANE")
# Pegarle el municipio a Resultados_rd_des
nombres <- Municipios %>% group_by(ID_DANE, MCPIO) %>% summarise(temp = n()) %>% select(-(temp))
Resultados_rd_des <- merge(Resultados_rd_des, nombres, by = "ID_DANE")

# Combinarla con las estimaciones de los municipios observados
names(Resultados_sd_des); names(Resultados_rd_des)
names(Resultados_sd_des)[c(1, 5, 6, 7)] <- c("ID_DANE","Y_FH_sd", "MSE_Y_FH_sd", "cve_FH_sd")

Resultados_sd_des$TipologiaDominio  <- "Observado"
Resultados_rd_des$TipologiaDominio  <- "No observado"
Resultados_des <- bind_rows(Resultados_sd_des, Resultados_rd_des )
Resultados_des$Y_FH <- ifelse(Resultados_des$TipologiaDominio == "Observado",
                              Resultados_des$Y_FH_sd, Resultados_des$Y_FH_rd)

Resultados_des$MSE_Y_FH <- ifelse(Resultados_des$TipologiaDominio == "Observado", 
                              Resultados_des$MSE_Y_FH_sd, Resultados_des$MSE_Y_FH_rd)
Resultados_des$cve_FH <- sqrt(Resultados_des$MSE_Y_FH) / Resultados_des$Y_FH * 100

ResultadosFinales_des <- Resultados_des[c("TipologiaDominio","ID_DANE", "MCPIO", "Y_dir", "cve_dir", "Y_FH",
                                  "MSE_Y_FH", "cve_FH")]
View(ResultadosFinales_des)
ResultadosFinales_des <- arrange(ResultadosFinales_des, cve_FH)
ResultadosFinales_des$Ranking <- 1:nrow(ResultadosFinales_des)

mean(ResultadosFinales_des$cve_FH)


#*********************************************************
## 3. Tabla final de Resultados ####
#*********************************************************
# MCIPIO, est_ingreso, mse_ingreso, cve_ingreso, est_desempleo, mse_desempleo, cve_desempleo

head(ResultadosFinales)
head(ResultadosFinales_des)
Res_final <- merge(ResultadosFinales, ResultadosFinales_des, by="ID_DANE") %>%
             select(MCPIO = MCPIO.x, est_ingreso=Y_FH.x, mse_ingreso=MSE_Y_FH.x, cve_ingreso=cve_FH.x,
                    est_desempleo=Y_FH.y, mse_desempleo=MSE_Y_FH.y, cve_desempleo=cve_FH.y)

mean(c(Res_final$cve_desempleo, Res_final$cve_ingreso))
write.table(Res_final, "Res_final.csv", sep=";", row.names = F)


