# funciones para mostrar el formato de los parametros de entrada y del resultado

# prediccion usanod (0,0,0)(0,0,0) arima model
getPred000 <- function(x_train, x_test_past){
  mod <- arima(x_train, order=c(0,0,0), seasonal=list(order=c(0,0,0), period=12))
  x_past = c(x_train, x_test_past)
  fit_upd <- Arima(x_past, model = mod)
  fc <- forecast::forecast(fit_upd, h = 1)
  mu_hat  <- as.numeric(fc$mean[1])
  z80 <- qnorm(0.8)
  se_from_up <- (fc$upper[,"80%"][1] - fc$mean[1]) / z80
  se_from_lo <- (fc$mean[1]  - fc$lower[,"80%"][1]) / z80
  se_hat_aux <- pmax(se_from_up, se_from_lo) 
  se_hat  <- as.numeric(se_hat_aux)
  return(list(mu_hat=mu_hat, se_hat=se_hat))
}


# estimar Sigma como la diagonal que tiene la varianza de cada activo
getSigmaDiag <- function(sig, Xpast){
  return(diag(sig))
}

# alpha random
getAlphaRandom <- function(mu,Sigma, gamma){
  alpha <- rnorm(length(mu))
  alpha <- alpha/sum(alpha)
  return(alpha)
}

# alpha random y positivo
getAlphaRandomPos <- function(mu,Sigma, gamma){
  alpha <- abs(rnorm(length(mu)))
  alpha <- alpha/sum(alpha)
  return(alpha)
}

# alpha random y positivo y en enteros/5
getAlphaRandomPosInt <- function(mu, Sigma, gamma){
  smpl <- c()
  sumSmpl <- 0
  for(i in 1:5){
    smpl <- c(smpl, sample(0:(5-sumSmpl),1))
    sumSmpl <- sum(smpl)
  }
  alpha <- smpl/5
  return(alpha)
}


