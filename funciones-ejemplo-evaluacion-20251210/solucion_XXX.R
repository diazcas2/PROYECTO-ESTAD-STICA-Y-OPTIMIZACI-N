# solucion trivial ejemplo
remove(list=ls())
setwd("/home/emiliano/Documents/estadistica/estadistica_y_optimizacion_master/proyecto/")
dir()
# cargamos funciones con soluciones triviales. 
# Cada equipo debera definir en su archivo de solucion_teamName.R todas las
# funciones incluidas abajo

#############################################################################################
# MODIFICAR DESDE AQUI...


teamName <- "????"
# integrante 1: 
# integrante 2:
# integrante 3:

##########################
# seccion 2 - predicciones
##########################

# Funcion para predecir UN activo para el periodo test en el momento t
getPred <- function(x_train, x_test_past){
  # INPUTS: x_train serie de tiempo de todo periodo train para UN activo
  #       : x_test_past serie de tiempo de periodo test que va desde 0 hast t-1
  
  # OUTPUTS: mu_hat: prediccion del activo para el momento t del periodo test
  #          se_hat: desviación estándar de la predicción. 
  
  
  # INSERTAR ESTIMACION DE MODELO DE SERIES DE TIEMPO PARA MOMENTO T PARA UN ACTIVO
  mu_hat <- # INSERTAR CALCULO FINAL DE LA PREDICCION (UN SOLO NUMERO)
  se_hat <- # INSERTAR CALCULO FINAL DE LA DESVIACIONE STANDAR (UN SOLO NUMERO)
  
    return(list(mu_hat=mu_hat, se_hat=se_hat))
}

#####################################
# seccion 3 - utilidad media-varianza
###########################################################
# 3.1 utilidad media-varianza, alfa_i positiva o negativa
###########################################################
gammaMV <- #INSERTAR VALOR EN REALES 

# Funcion para estimar la matriz de covarianzas entre los rendimientos de los 5
# activos a partir de las desviaciones estándares (que vendran de su modelo Arima)
# y el historico de rendimientos (que pueden usar para estimar correlaciones) 
getSigmaMV <- function(sig, Xpast){
  # INPUT: sig, vector en R^5 con desviaciones estandar de 5 activos al momento t
  #        Xpast, matriz en R^(T x 5) con rendimientos de 5 activos desde t=0 hasta t-1
  # OUTPUT: Sigma: matriz en 5 x 5 con covarianzas entre activos
  
  #INSERTAR CONSTRUCCION DE MATRIZ DE COVARIANZAS 
  Sigma <- # INSERTAR CALCULO FINAL DE SIGMA (MATRIZ DE 5 X 5)
  return(Sigma)
}

# Funcion para optimizar la asignacion de cada activo dentro del portafolio
# tomando en cuenta la utilidad Media-Varianza CON posiciones cortas
getAlphaMV <- function(mu,Sigma, gamma){
  # INPUT: mu: vector en R^5 con los rendimientos esperados de los 5 activos para el periodo t
  #        Sigma: matriz en R^{5 x 5} con las covarianzas de los 5 activos
  #        gamma: valor en R que representa el apetito de riesgo. Entre mas bajo mas riesgo.
  # OUTPUT: alpha: vector en R^5 con la asignación elegida para los 5 activos resultante de optimizar
  # U-MV sujeto a que alpha sume a 1.
  
  #INSERTAR PASOS DE OPTIMIZACION
  alpha <- # INSERTAR CALCULO FINAL DE ALPHA
  return(alpha)
} 

############################################
# 3.2 utilidad media-varianza, alfa_i positiva 
############################################

gammaMVPos <- #INSERTAR VALOR EN REALES

# Funcion para estimar la matriz de covarianzas entre los rendimientos de los 5
# activos a partir de las desviaciones estándares (que vendran de su modelo Arima)
# y el historico de rendimientos (que pueden usar para estimar correlaciones) 
getSigmaMVPos <- function(sig, Xpast){
  # INPUT: sig, vector en R^5 con desviaciones estandar de 5 activos al momento t
  #        Xpast, matriz en R^(T x 5) con rendimientos de 5 activos desde t=0 hasta t-1
  # OUTPUT: Sigma: matriz en 5 x 5 con covarianzas entre activos
  
  #INSERTAR CONSTRUCCION DE MATRIZ DE COVARIANZAS 
  Sigma <- # INSERTAR CALCULO FINAL DE SIGMA (MATRIZ DE 5 X 5)
  return(Sigma)
}

# Funcion para optimizar la asignacion de cada activo dentro del portafolio
# tomando en cuenta la utilidad Media-Varianza SIN posiciones cortas
getAlphaMVPos <- function(mu,Sigma, gamma){
  # INPUT: mu: vector en R^5 con los rendimientos esperados de los 5 activos para el periodo t
  #        Sigma: matriz en R^{5 x 5} con las covarianzas de los 5 activos
  #        gamma: valor en R que representa el apetito de riesgo. Entre mas bajo mas riesgo.
  # OUTPUT: alpha: vector en R^5 con la asignación elegida para los 5 activos resultante de optimizar
  # U-MV sujeto a que alpha sume a 1 y alpha_i>0.
  
  #INSERTAR PASOS DE OPTIMIZACION
  alpha <- # INSERTAR CALCULO FINAL DE ALPHA
  return(alpha)
}


################################################
# seccion 4 - 
# utilidad log, alfa_i positiva o negativa
################################################
gammaLog = # INSERTAR VALOR EN REALES

# Funcion para estimar la matriz de covarianzas entre los rendimientos de los 5
# activos a partir de las desviaciones estándares (que vendran de su modelo Arima)
# y el historico de rendimientos (que pueden usar para estimar correlaciones) 
getSigmaLog <- function(sig, Xpast){
  # INPUT: sig, vector en R^5 con desviaciones estandar de 5 activos al momento t
  #        Xpast, matriz en R^(T x 5) con rendimientos de 5 activos desde t=0 hasta t-1
  # OUTPUT: Sigma: matriz en 5 x 5 con covarianzas entre activos
  
  #INSERTAR CONSTRUCCION DE MATRIZ DE COVARIANZAS 
  Sigma <- # INSERTAR CALCULO FINAL DE SIGMA (MATRIZ DE 5 X 5)
  return(Sigma)
}

# Funcion para optimizar la asignacion de cada activo dentro del portafolio
# tomando en cuenta la utilidad log CON posiciones cortas
getAlphaLog <- function(mu,Sigma, gamma){
  # INPUT: mu: vector en R^5 con los rendimientos esperados de los 5 activos para el periodo t
  #        Sigma: matriz en R^{5 x 5} con las covarianzas de los 5 activos
  #        gamma: valor en R que representa el apetito de riesgo. Entre mas bajo mas riesgo.
  # OUTPUT: alpha: vector en R^5 con la asignación elegida para los 5 activos resultante de optimizar
  # U-log sujeto a que alpha sume a 1.
  
  #INSERTAR PASOS DE OPTIMIZACION
  alpha <- # INSERTAR CALCULO FINAL DE ALPHA
  return(alpha)
}


# ... HASTA AQUI
#############################################################################################


###############################################################
# Evaluación de soluciones
###############################################################
source("funciones/eval_funcs.R")

setwd("/home/emiliano/Documents/estadistica/estadistica_y_optimizacion_master/proyecto/")
X <- read.csv("stock_returns_train_2.csv")
X <- ts(X)

# Validation mode - para que se evaluen asi mismos con el 
Xtrain <- window(X, start=1,end=8*12) # el start-end es un ejemplo, pueden cambiarlo
Xtest <- window(X, start=8*12+1,end=10*12)

# Test mode - no tendran el archivo stock_returns_test.csv asi que esto lo 
# ejecutaremos una vez entreguen soluciones
#Xtrain <- X
#Xtest <- ts(read.csv("stock_returns_test.csv"))


#seccion 2 - predicciones
set.seed(43)
res <- getPred_ts(Xtrain, Xtest, getPred)
mu_hat = res$mu_hat
se_hat = res$se_hat

# MAPE
i <- 2
plot(as.data.frame(Xtest)[,i], ty="l")
lines(mu_hat[,i], col="blue", ty="l")



rmse <- sqrt(mean((Xtest-mu_hat)^2))
evals <- c(rmse=rmse)
evals

# seccion 3 - utilidad media varianza
# utilidad media-varianza, alfa_i positiva o negativa

alpha_hat <- getAlpha_ts(mu_hat, se_hat, gammaMV, getSigmaMV, getAlphaMV, Xtrain, Xtest)
passChecks <- getChecks(alpha_hat, mode="sum1")
ret <- getRet(alpha_hat, Xtest, passChecks)
evals <- c(evals, retMV=ret)
Umv_rel <- getUEval(alpha_hat, mu_hat, se_hat, Xtrain, Xtest, gammaMV, getSigmaMV, passChecks, Umv)
evals <- c(evals,  Umv=Umv_rel)

# utilidad media-varianza, alfa_i positiva 

alpha_hat <- getAlpha_ts(mu_hat, se_hat, gammaMVPos, getSigmaMVPos, getAlphaMVPos, Xtrain, Xtest)
passChecks <- getChecks(alpha_hat, mode=c("sum1","pos"))
ret <- getRet(alpha_hat, Xtest, passChecks)
evals <- c(evals, retMVPos=ret)
Umv_rel <- getUEval(alpha_hat, mu_hat, se_hat, Xtrain, Xtest, gammaMVPos, getSigmaMVPos, passChecks, Umv)
evals <- c(evals,  UmvPos=Umv_rel)


# seccion 4 - 
# utilidad log, alfa_i positiva o negativa

alpha_hat <- getAlpha_ts(mu_hat, se_hat, gammaLog, getSigmaLog, getAlphaLog, Xtrain, Xtest)
passChecks <- getChecks(alpha_hat, mode=c("sum1"))
ret <- getRet(alpha_hat, Xtest, passChecks)
evals <- c(evals, retLog=ret)
Umv_rel <- getUEval(alpha_hat, mu_hat, se_hat, Xtrain, Xtest, gammaLog, getSigmaLog, passChecks, Umv)
evals <- c(evals,  UmvPosInt=Umv_rel)

evals


