#**********************************************************************************************************************
#*****************************************      SAE Competition 2      ************************************************ 
#**********************************************************************************************************************
library(sae); library(survey); library(readxl);library(dplyr)
options(survey.lonely.psu = "adjust")
options(scipen=999)
rm(list = ls())

## NI: Numero de municipios en la pob,  
## NII: Número de colegios, 
## N_i: Número estudiantes

##setwd("C:/Users/Camilo/Documents/GitHub/SAE_competition2")

muestra <- readRDS("./data/muestra3etapasACundinamarca.RDS")
length(unique(muestra$CODIGOMUNICIPIO))
InfoAux <- as.data.frame(read_excel("./data/CONSOLIDADO_MPIOS_CUNDINAMARCA.xlsx"))

# Mejor traerle los nombres  y códigos de los municipios DANE (los códigos mpio ICFES cambian)
# Ver si se puede unir por los nombres de los municipios 
mun <- InfoAux$MCPIO 
mun2 <- unique(muestra$NOMBREMUNICIPIO)
table(mun2%in% mun)

Nomb_mpios <- InfoAux %>% group_by(ID_DANE, MCPIO) %>% summarise (temp = n())
Nomb_mpios$temp <- NULL
muestra <- merge(Nomb_mpios, muestra, all.y = T,
                 by.x = "MCPIO", by.y = "NOMBREMUNICIPIO")

### Paso 1: primero calcular las estimaciones directas de los municipios
diseno_muestral <- svydesign(ids = ~ID_DANE + CODIGO_ICFES + ID_estud,
                             strata = ~estrato_mpio + EstratoColegio,
                             fpc = ~ NI + NII + N_i, data = muestra,
                             nest = T)

Estimadirectas <- svyby(~MATEMATICAS_PUNT, ~ID_DANE, diseno_muestral,FUN = svymean)
Estimadirectas$vest_dir <- Estimadirectas$se^2 # INSUMO FUNDAMENTAL
Estimadirectas$cve <- Estimadirectas$se/Estimadirectas$MATEMATICAS_PUNT * 100

cons <- muestra %>% group_by(ID_DANE, MCPIO)  %>% 
     summarise(temp = n())
cons$temp <- NULL
Estimadirectas <- merge(cons, Estimadirectas, by = "ID_DANE")
Estimadirectas <- arrange(Estimadirectas, -MATEMATICAS_PUNT)

#################################### Estimador de Fay - Herriot ####################
Estimadirectas$MCPIO <- NULL
# Incorporar la información auxiliar
Datos <- merge(InfoAux,Estimadirectas, by.x = "ID_DANE", by.y = "ID_DANE", all.y=T)
#edit(names(Datos))
# Exploración 
modelocompleto <- paste0("MATEMATICAS_PUNT", " ~ ", paste(varcandidas, collapse = " + "))
modelocompleto <- as.formula(modelocompleto)
varcandidas <- c("TASA_AFILIADOS_SUBSIDIADO", 
                 "AVALUOS_CATASTRALES_RURALES", "AVALUOS_CATASTRALES_URBANOS", 
                 "COBER_MEDIA_TASA", "COBER_PRIMARIA_TASA", "COBER_SECUNDARIA_TASA", 
                 "COBER_TRANSICION_TASA", "CONSUMO_ENERGIA_PER_HABIT", "IND_EFICIENCIA_MUNICIPAL", 
                 "IND_INTEGRAL_MCIPAL", "NBI_2010", 
                 "NBI_RURAL_2010", "NBI_URBANO_2010", 
                 "TASA_DELITOS_SEXUALES", "TASA_DESERCION_ESCOLAR", 
                 "TASA_VIOLENCIA_INTRAFAMILIAR", "TASA_HOMICIDIOS", "TASA_VACUN_BCG", 
                 "TASA_VACUN_DPT", "TASA_VACUN_POLIO", "TASA_VACUN_TRIPLEVIRAL")
modelo_optimo <- step(lm(modelocompleto, data = Datos))
modelo_reducido <- lm(MATEMATICAS_PUNT ~ TASA_AFILIADOS_SUBSIDIADO + CONSUMO_ENERGIA_PER_HABIT + 
                           IND_INTEGRAL_MCIPAL + NBI_RURAL_2010 + NBI_URBANO_2010 + 
                           TASA_VIOLENCIA_INTRAFAMILIAR + TASA_VACUN_DPT + TASA_VACUN_POLIO + 
                           TASA_VACUN_TRIPLEVIRAL, data = Datos)

cor(Datos[,c("MATEMATICAS_PUNT",names(modelo_reducido$coefficients)[2:10])])[1,]
formula_candidata <- as.formula("MATEMATICAS_PUNT ~ AVALUOS_CATASTRALES_RURALES + 
                                AVALUOS_CATASTRALES_URBANOS + CONSUMO_ENERGIA_PER_HABIT + NBI_2010")
modelo_reducido2 <- lm(MATEMATICAS_PUNT ~ TASA_AFILIADOS_SUBSIDIADO + CONSUMO_ENERGIA_PER_HABIT + 
                            IND_INTEGRAL_MCIPAL + NBI_RURAL_2010 + NBI_URBANO_2010 + 
                            TASA_VACUN_DPT + TASA_VACUN_POLIO + 
                            TASA_VACUN_TRIPLEVIRAL, data = Datos)

summary(modelo_reducido2)
#################################################################
FH_prommat <- mseFH(MATEMATICAS_PUNT ~ TASA_AFILIADOS_SUBSIDIADO + CONSUMO_ENERGIA_PER_HABIT + 
                         IND_INTEGRAL_MCIPAL + NBI_RURAL_2010 + NBI_URBANO_2010 + 
                         TASA_VACUN_DPT + TASA_VACUN_POLIO + 
                         TASA_VACUN_TRIPLEVIRAL, vest_dir,
                    data = Datos)
FH_prommat
Datos$Y_FH <- FH_prommat$est$eblup
Datos$mseY_FH <- FH_prommat$mse
Datos$cve_FH <- FH_prommat$mse / FH_prommat$est$eblup * 100
Datos$cve_dir <- sqrt(Datos$vest_dir) / Datos$MATEMATICAS_PUNT * 100
Resultados_sd <- Datos[c("ID_DANE","MCPIO","MATEMATICAS_PUNT","cve_dir", "Y_FH", "mseY_FH","cve_FH")]
names(Resultados_sd)[3] <- "Y_dir"
mpiossel <- unique(muestra$ID_DANE)
mpiosnosel <- InfoAux$ID_DANE[!(InfoAux$ID_DANE %in% mpiossel)]

# Hacer las estimaciones del promedio para los dominios no observados en la muestra
X_rd <- InfoAux[InfoAux$ID_DANE %in% mpiosnosel, 
                c("TASA_AFILIADOS_SUBSIDIADO", "CONSUMO_ENERGIA_PER_HABIT", 
                  "IND_INTEGRAL_MCIPAL", "NBI_RURAL_2010", "NBI_URBANO_2010", 
                  "TASA_VACUN_DPT", "TASA_VACUN_POLIO", "TASA_VACUN_TRIPLEVIRAL")]

X_rd$Unos <- 1
X_rd <- X_rd[c("Unos", c("TASA_AFILIADOS_SUBSIDIADO", "CONSUMO_ENERGIA_PER_HABIT", 
                         "IND_INTEGRAL_MCIPAL", "NBI_RURAL_2010", "NBI_URBANO_2010", 
                         "TASA_VACUN_DPT", "TASA_VACUN_POLIO", "TASA_VACUN_TRIPLEVIRAL"))]
X_rd <- as.matrix(X_rd)
row.names(X_rd) <- InfoAux[InfoAux$ID_DANE %in% mpiosnosel,]$ID_DANE
Beta <- as.matrix(FH_prommat$est$fit$estcoef$beta)
YFH_rd <- X_rd %*% Beta # Estimación para los dominios no observados
YFH_rd <- as.data.frame(YFH_rd)
YFH_rd$ID_DANE <- InfoAux[InfoAux$ID_DANE %in% mpiosnosel, ]$ID_DANE
colnames(YFH_rd)[1] <- "Y_FH_rd"                             


# Error cuadrático medio para los municipios no observados
# \sigma_u ^2 + 
sigma2_u <- FH_prommat$est$fit$refvar

# Información auxiliar en la muestra
X_sd <- InfoAux[InfoAux$ID_DANE %in% mpiossel, 
                c("TASA_AFILIADOS_SUBSIDIADO", "CONSUMO_ENERGIA_PER_HABIT", 
                  "IND_INTEGRAL_MCIPAL", "NBI_RURAL_2010", "NBI_URBANO_2010", 
                  "TASA_VACUN_DPT", "TASA_VACUN_POLIO", "TASA_VACUN_TRIPLEVIRAL")]
X_sd$Unos <- 1
X_sd <- X_sd[c("Unos", "TASA_AFILIADOS_SUBSIDIADO", "CONSUMO_ENERGIA_PER_HABIT", 
               "IND_INTEGRAL_MCIPAL", "NBI_RURAL_2010", "NBI_URBANO_2010", 
               "TASA_VACUN_DPT", "TASA_VACUN_POLIO", "TASA_VACUN_TRIPLEVIRAL")]
X_sd <- as.matrix(X_sd)

# \hat{V}(\hat{\boldsymbol{\beta}}): varianza de los parámetros
V_beta_est <- t(X_sd) %*% diag(sigma2_u + Datos$vest_dir) %*% X_sd  # 24 X 1

# Error cuadrático para dominios no observados
MSE_Y.FH <- X_rd %*% solve(V_beta_est) %*% t(X_rd) + sigma2_u
MSE_Y.FH <- diag(MSE_Y.FH)
MSE_Y.FH <- as.data.frame(as.table(MSE_Y.FH))
colnames(MSE_Y.FH) <- c("ID_DANE", "MSE_Y_FH_rd")

Resultados_rd <- merge(YFH_rd, MSE_Y.FH, by = "ID_DANE")
# Pegarle el municipio a Resultados_rd
Municipios <- InfoAux %>% group_by(ID_DANE, MCPIO) %>% summarise(temp = n())
Municipios$temp <- NULL
Resultados_rd <- merge(Resultados_rd, Municipios, by = "ID_DANE")

# Combinarla con las estimaciones de los municipios observados
names(Resultados_sd); names(Resultados_rd)
names(Resultados_sd)[c(5, 6, 7)] <- c("Y_FH_sd", "MSE_Y_FH_sd", "cve_FH_sd")

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
