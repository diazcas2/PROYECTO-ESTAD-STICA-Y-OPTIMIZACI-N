# funciones para evaluar resultados de sus soluciones
library(forecast)

# aplicar funcion de prediccion getPredFunc recursivamente
getPred_ts<- function(Xtrain, Xtest, getPredFunc){
  
  H <- nrow(Xtest)
  mu_hat <- matrix(NA, H, 5)
  se_hat <- matrix(NA, H, 5)
  

  X_test_past <- Xtest[0,]
  print(paste("dim X test past: ", dim(X_test_past)))
  for (h in seq_len(H)) {
    for (i in 1:5){
      # update model state with data up to t-1 (parameters fixed)
      pred = do.call(getPredFunc, list(Xtrain[,i], X_test_past[,i]))
      mu_hat[h,i] <- pred$mu_hat
      se_hat[h,i] <- pred$se_hat
    }
    X_test_past <- Xtest[1:h,,drop=FALSE]
  }
  return(list(mu_hat=mu_hat, se_hat=se_hat))
}



# obtener alpha_t en funcion de mu_t, sig_t, gamma y la funcion Sigma(sig_t, Xpast) elegida
getAlpha <- function(mu,sig, gamma, getSigmaFunc, getAlphaFunc, Xpast){
  Sigma <- do.call(getSigmaFunc, list(sig, Xpast))
  alpha <- do.call(getAlphaFunc, list(mu, Sigma, gamma))
  return(alpha)  
}

# obtener alpha para todo periodo de test
getAlpha_ts <- function(mus, sigs, gamma, getSigmaFunc, getAlphaFunc, Xtrain, Xtest){
  H <- nrow(Xtest)
  alpha_hat <- matrix(NA, H, 5)
  Xpast <- Xtrain
  for (h in seq_len(H)) {
    alpha_hat[h,] <- getAlpha(mus[h,], sigs[h,], gamma, getSigmaFunc, getAlphaFunc, Xpast)
    Xpast <- rbind(Xpast, Xtest[h,])
  }  
  return(alpha_hat)
}

# funcion para checar si las alfas cumplen los requisitos segun el inciso
getChecks <- function(alpha_hat, mode=c("sum1","pos","int")){
  passChecks <- TRUE
  if(("sum1" %in% mode) & passChecks){
    print("sum1 check")
    passChecks <- all( (apply(alpha_hat, 1, sum)-1) <1e-6)
  }
  if(("pos" %in% mode) & passChecks){
    print("pos check")
    passChecks <- all(alpha_hat>=-10e-14)
  }
  if( ("int" %in% mode) & passChecks){
    passChecks <- sapply(seq(0,5)/5, function(i) abs(alpha_hat-i) < 1e-6, simplify="array")
    passChecks <- apply(passChecks, c(1,2), any)
    passChecks <- all(passChecks)
  }
  return(passChecks)
}

# obtener el rendimeinto del portafolio
getRet <- function(alpha_hat, Xtest, passChecks){
  if(!passChecks) return(NA)
  ret <-prod(1+apply(alpha_hat*Xtest, 1, sum))-1
  return(ret)
  
}

# función de utilidad mean-variance
Umv <- function(alpha, gamma, mu_hat, Sigma_hat){
  return(sum(alpha*mu_hat)- (gamma/2)*(t(alpha)%*%Sigma_hat%*%alpha)[1,1])
}

# función de utilidad log
Ulog <- function(alpha, gamma, mu_hat, Sigma_hat){
  Er <- sum(alpha*mu_hat)
  logTerm <- log(1+Er)
  riskTerm <- (t(alpha)%*%Sigma_hat%*%alpha)[1,1] / (1+Er)^2
  res <- logTerm -0.5*riskTerm
  
  return(res)
}

# solución en la que alpha es igual para todos activos. Usaremos esta
# como referencia para evaluar la calidad de nuestra solucion dado que cada
# quien puede elegir distinta gamma, Sigma, mu.
getAlphaEqual <- function(mu,Sigma, gamma){
  alpha <- rep(1,length(mu))
  alpha <- alpha/sum(alpha)
  return(alpha)
}

# funcion para evaluar la calidad de las utilidades obtenidas segun la funcion de utilidad y en comparacion
# a ls solucion "getAlphaEqual"
getUEval <- function(alpha_hat, mu_hat, se_hat, Xtrain, Xtest, gamma, getSigma, passChecks, Ufunc){
  if(!passChecks) return(NA)
  U_val <- mean(sapply(1:nrow(alpha_hat), function(i) do.call(Ufunc, list(alpha_hat[i,],gamma, mu_hat[i,], getSigma(se_hat[i,], Xtrain)))))
  alpha_hat_ref <- getAlpha_ts(mu_hat, se_hat, gamma, getSigma, getAlphaEqual, Xtrain, Xtest)
  U_ref <- mean(sapply(1:nrow(alpha_hat), function(i) do.call(Ufunc, list(alpha_hat_ref[i,], gamma, mu_hat[i,], getSigma(se_hat[i,], Xtrain)))))
  U_rel <-  (U_val-U_ref)/abs(U_ref)
  return(U_rel)
}
