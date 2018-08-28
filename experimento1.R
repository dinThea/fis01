
library(tidyverse)
library(magrittr)

alturap = c(16.62, 16.60, 16.40, 16.40, 16.40, 16.36)
alturam = c(16, 15.975, 15.92, 15.915, 16.415, 16.425)
diametrop = c(16, 15.9, 15.9, 15.82, 15.92, 15.68)
diametrom = c(15.97, 15.88, 16, 16.115, 15.88, 15.75)
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

volp = data$altp*pi*((data$diap/2)^2)
volm = data$altm*pi*((data$diam/2)^2)

# Calculando a incerteza acumulada do volume

incAvol <- function(alt, dia, incertezalt, incertezadia) {
  
  return ( sqrt(((pi * (mean(dia)^2) / 4)^2)*incertezadia^2 + ((pi * (mean(dia)*mean(alt))/2)^2))*incertezalt^2 )
  
}

# Calculando a incerteza acumulada da densidade

incAd <- function(vol, mass, incertezavol, incertezamass) {
  
  return ( sqrt((1/mean(vol))^2*incertezavol^2 + (mean(mass)/(mean(vol)^2))*incertezamass^2) )
  
}

incertezaAcumuladaVolP <- incAvol(data$altp, data$diap, incertezasR[1], incertezasR[3])
incertezaAcumuladaVolM <- incAvol(data$altm, data$diam, incertezasR[2], incertezasR[4])
incertezaAccDP <- incAd(volp, data$m, incertezaAcumuladaVolP, incertezasR[5])
incertezaAccDM <- incAd(volm, data$m, incertezaAcumuladaVolM, incertezasR[5])

print ('Incertezas do tipo B de cada mediçao: 1')
print (cat('Altura Paquimetro: ', incertezasB[1], "\n"))
print (cat('Altura Micrometro: ', incertezasB[2], "\n"))
print (cat('Diametro Paquimetro: ', incertezasB[3], "\n"))
print (cat('Diametro Micrometro: ', incertezasB[4], "\n"))
print (cat('Mass Balança: ', incertezasB[5], "\n"))
print ('Incertezas do tipo A de cada mediçao: ')
print (cat('Altura Paquimetro: ', incertezasA[1], "\n"))
print (cat('Altura Micrometro: ', incertezasA[2], "\n"))
print (cat('Diametro Paquimetro: ', incertezasA[3], "\n"))
print (cat('Diametro Micrometro: ', incertezasA[4], "\n"))
print (cat('Mass Balança: ', incertezasA[5], "\n"))
print ('Incertezas combinadas: ')
print (cat('Altura Paquimetro: ', incertezasR[1], "\n"))
print (cat('Altura Micrometro: ', incertezasR[2], "\n"))
print (cat('Diametro Paquimetro: ', incertezasR[3], "\n"))
print (cat('Diametro Micrometro: ', incertezasR[4], "\n"))
print (cat('Mass Balança: ', incertezasR[5], "\n"))
print ('Propagaçao de incerteza no volume: ')
print (cat('Paquimetro: ', incertezaAcumuladaVolP, "\n"))
print (cat('Micrometro: ', incertezaAcumuladaVolM, "\n"))
print ('Propagaçao de incerteza na densidade: ')
print (cat('Paquimetro: ', incertezaAccDP, "\n"))
print (cat('Micrometro: ', incertezaAccDM, "\n"))