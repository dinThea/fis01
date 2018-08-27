
library(tidyverse)
library(magrittr)

alturap = c(16.62, 16.60, 16.40, 16.40, 16.40, 16.36)
alturam = c(16, 15.975, 15.92, 15.915, 16.415, 16.425)
diametrop = c(16, 15.9, 15.9, 15.82, 15.92, 15.68)
diametrom = c(18.97, 15.88, 16, 16.115, 15.88, 15.75)
mass = c(8.2, 8.2, 8.2, 8.2, 8.2, 8.2)

incertezasB = c(0.02, 0.005, 0.02, 0.005, 0.25)

data = tibble(altp = alturap, altm = alturam, diap = diametrop, diam = diametrom, m = mass)

mean(data$altp)
mean(data$altm)
mean(data$diap)
mean(data$diam)

# Calculando os tipos de incerteza

incertezA <- function(val) {
  val
  media <- mean(val)
  n <- length(val)
  soma <- 0
  for (i in 1:n) {
    soma <- soma + (val[i] - media)^2
  }
  return(sqrt(soma/(n*(n-1))))
}

incertezasA <- c(incertezA(data$altp), incertezA(data$altm), incertezA(data$diap), incertezA(data$diam), incertezA(data$m))
incertezasR <- sqrt(incertezasA^2 + incertezasB^2) 

# 
