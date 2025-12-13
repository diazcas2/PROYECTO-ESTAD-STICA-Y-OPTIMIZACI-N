remove(list=ls())

teamName <- "ARIMAS"
# integrante 1: María de los Ángeles Díaz Castro 
# integrante 2: Florencia Pellegrini
# integrante 3: Iyán Álvarez Rodriguez
# integrante 4: Azahara Martinez Moraño

# Sección 2 - predicciones

oneStepAhead <- function(x_train, x_test_past) {
  
  # Convertimos a vector numérico limpio
  x_train <- as.numeric(na.omit(x_train))
  x_test_past <- as.numeric(na.omit(x_test_past))
  
  if (length(x_test_past) == 0) {
    x_past <- x_train
  } else {
    x_past <- c(x_train, x_test_past)
  }
  
  # 1. Ajustamos ARIMA al histórico
  mod <- auto.arima(x_train, stepwise = FALSE, approximation = FALSE)
  
  # 2. Actualizamos el modelo
  fit_upd <- Arima(x_past, model = mod)
  
  # 3. Forecast one-step-ahead
  fc <- forecast(fit_upd, h = 1)
  mu_hat <- as.numeric(fc$mean[1])
  
  # 4. Estimamos el error estándar usando intervalo 80%
  z80 <- qnorm(0.8)
  se_from_up <- (fc$upper[,"80%"][1] - fc$mean[1]) / z80
  se_from_lo <- (fc$mean[1] - fc$lower[,"80%"][1]) / z80
  se_hat <- as.numeric(max(se_from_up, se_from_lo))
  
  return(list(mu_hat = mu_hat, se_hat = se_hat))
}


#RENOMBRAMOS LA FUNCIÓN:
getPred<-oneStepAhead

# Seccion 3 - utilidad media-varianza, 
# utilidad media-varianza, alfa_i positiva o negativa

construccion_sigma_t <- function(sig, Xpast){
  # 1. Matriz diagonal de desviaciones estándar
  D <- diag(sig)
  
  # 2. Matriz de correlaciones históricas
  R <- cor(Xpast, use = "pairwise.complete.obs")
  
  # 3. Construcción de la matriz de covarianza
  Sigma <- D %*% R %*% D
  
  return(Sigma)
}

posiciones_cortas <- function(mu, Sigma, gamma){
  N <- length(mu)
  uno <- matrix(1, nrow = 1, ncol = N)
  mu <- matrix(mu, ncol = 1)
  
  # Inversa de Sigma
  Sigma_inv <- solve(Sigma)
  
  # Cálculo de lambda_star para la restricción sum(alpha)=1
  A <- as.numeric(uno %*% Sigma_inv %*% t(uno))
  B <- as.numeric(uno %*% Sigma_inv %*% mu)
  lambda_star <- (B - gamma) / A
  
  # Cálculo de alpha
  parte_mu <- (1 / gamma) * (Sigma_inv %*% mu)
  parte_restriccion <- Sigma_inv %*% t(uno) * (lambda_star / gamma)
  alpha <- parte_mu - parte_restriccion
  
  alpha_vec <- as.numeric(alpha)
  names(alpha_vec) <- paste0("alpha_", 1:N)
  
  
  return(alpha_vec)
}


library(quadprog)
sin_posiciones_cortas  <- function(mu, Sigma, gamma){
  N <- length(mu)
  
  # Definir parámetros para solve.QP
  Dmat <- gamma * Sigma
  dvec <- mu
  Aeq <- matrix(1, nrow = N, ncol = 1)
  G <- diag(N)
  Amat <- cbind(Aeq, G)
  bvec <- c(1, rep(0, N))
  
  # Resolver problema de optimización cuadrática
  sol <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
  
  # Ajustes finales: redondear y eliminar negativos
  alpha_vec <- round(sol$solution, 8)
  alpha_vec[alpha_vec < 0] <- 0
  names(alpha_vec) <- paste0("alpha_", 1:N)
  
  return(alpha_vec)
}


gammaMV=1
getSigmaMV<-construccion_sigma_t
getAlphaMV<-posiciones_cortas 

# utilidad media-varianza, alfa_i positiva
gammaMVPos=34.5
getSigmaMVPos<-construccion_sigma_t
getAlphaMVPos<-sin_posiciones_cortas

# Seccion 4
# utilidad log, alfa_i positiva o negativa

getAlphaLog <- function(mu, Sigma, gamma){
  N <- length(mu)
  
  # Transformación softmax para imponer suma=1
  softmax <- function(z){
    e <- exp(z - max(z))
    return(e / sum(e))
  }
  
  # Función objetivo sobre z (no sobre alpha directamente)
  f_obj <- function(z){
    alpha <- softmax(z)
    muTalpha <- sum(alpha * mu)
    alphaTSigmaAlpha <- t(alpha) %*% Sigma %*% alpha
    
    Ulog <- log(1 + muTalpha) -
      (gamma/2) * alphaTSigmaAlpha / (1 + muTalpha)^2
    
    return(-Ulog)
  }
  
  # inicialización
  z0 <- rep(0, N)
  
  sol <- optim(par = z0, fn = f_obj, method="BFGS")
  
  alpha <- softmax(sol$par)
  names(alpha) <- paste0("alpha_Log_", 1:N)
  return(alpha)
}


gammaLog=4.9
getSigmaLog<-construccion_sigma_t
getAlphaLog<-getAlphaLog


###############################################################
# Evaluación de soluciones
###############################################################
source("eval_funcs.R")

#setwd("/home/emiliano/Documents/estadistica/estadistica_y_optimizacion_master/proyecto/")
X <- read.csv("stock_returns_train_2.csv")
X <- ts(X/100)

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

###############################################################################
# SELECCIÓN DE GAMMAS ÓPTIMOS SEGÚN SHARPE (USANDO Xtrain + Xtest)
###############################################################################

T_test <- nrow(Xtest)

# Función auxiliar para calcular Sharpe para un método dado
calc_sharpe_grid <- function(gammas_grid, getSigma_fun, getAlpha_fun, 
                             mu_hat, se_hat, Xtrain, Xtest, check_mode){
  
  res_df <- data.frame(
    gamma  = gammas_grid,
    sharpe = NA_real_,
    mean_ret = NA_real_,
    vol = NA_real_
  )
  
  for (k in seq_along(gammas_grid)) {
    gamma_tmp <- gammas_grid[k]
    
    alpha_hat <- getAlpha_ts(
      mu_hat, se_hat, gamma_tmp,
      getSigma_fun, getAlpha_fun,
      Xtrain, Xtest
    )
    
    passChecks <- getChecks(alpha_hat, mode = check_mode)
    if (!passChecks) next
    if (any(is.na(alpha_hat))) next
    
    # retornos del portafolio en test (aquí usamos Xtest local)
    r_p <- apply(alpha_hat * Xtest, 1, sum)
    
    mu_r <- mean(r_p)
    sd_r <- sd(r_p)
    
    if (sd_r > 0) {
      sharpe <- mu_r / sd_r
    } else {
      sharpe <- NA
    }
    
    res_df$sharpe[k]   <- sharpe
    res_df$mean_ret[k] <- mu_r
    res_df$vol[k]      <- sd_r
  }
  
  return(res_df)
}

############################
# 3.1 MEDIA-VARIANZA CON CORTOS
############################

gammas_MV <- seq(0.01, 1, by = 0.01)  # rango fino alrededor de valores pequeños

res_sharpe_MV <- calc_sharpe_grid(
  gammas_grid  = gammas_MV,
  getSigma_fun = getSigmaMV,
  getAlpha_fun = getAlphaMV,
  mu_hat       = mu_hat,
  se_hat       = se_hat,
  Xtrain       = Xtrain,
  Xtest        = Xtest,
  check_mode   = "sum1"          # permite cortos
)

best_idx_MV <- which.max(res_sharpe_MV$sharpe)
gammaMV_opt <- res_sharpe_MV$gamma[best_idx_MV]

cat("Gamma óptimo MV (con cortos) según Sharpe:", gammaMV_opt, "\n")
print(res_sharpe_MV[best_idx_MV, ])

############################
# 3.2 MEDIA-VARIANZA SIN CORTOS
############################

gammas_MVPos <- seq(0.1, 40, by = 0.1)

res_sharpe_MVPos <- calc_sharpe_grid(
  gammas_grid  = gammas_MVPos,
  getSigma_fun = getSigmaMVPos,
  getAlpha_fun = getAlphaMVPos,
  mu_hat       = mu_hat,
  se_hat       = se_hat,
  Xtrain       = Xtrain,
  Xtest        = Xtest,
  check_mode   = c("sum1","pos")   # sin cortos
)

best_idx_MVPos <- which.max(res_sharpe_MVPos$sharpe)
gammaMVPos_opt <- res_sharpe_MVPos$gamma[best_idx_MVPos]

cat("Gamma óptimo MVPos (sin cortos) según Sharpe:", gammaMVPos_opt, "\n")
print(res_sharpe_MVPos[best_idx_MVPos, ])

############################
# 3.3 UTILIDAD LOGARÍTMICA
############################

gammas_Log <- seq(0.1, 40, by = 0.1)

res_sharpe_Log <- calc_sharpe_grid(
  gammas_grid  = gammas_Log,
  getSigma_fun = getSigmaLog,
  getAlpha_fun = getAlphaLog,
  mu_hat       = mu_hat,
  se_hat       = se_hat,
  Xtrain       = Xtrain,
  Xtest        = Xtest,
  check_mode   = "sum1"            # misma restricción que antes
)

best_idx_Log <- which.max(res_sharpe_Log$sharpe)
gammaLog_opt <- res_sharpe_Log$gamma[best_idx_Log]

cat("Gamma óptimo Log según Sharpe:", gammaLog_opt, "\n")
print(res_sharpe_Log[best_idx_Log, ])

# Sobrescribimos las gammas que usarán las evaluaciones de abajo
gammaMV    <- gammaMV_opt
gammaMVPos <- gammaMVPos_opt
gammaLog   <- gammaLog_opt

gammaMV
gammaMVPos
gammaLog
