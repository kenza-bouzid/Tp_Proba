#Ce fichier regroupe les différentes fonctions qui nous permettent de réaliser nos tests de 
#qualité pour les générateurs étudiés 


#sources# 

source('generateurs.R')
source('calculProba.R')
library('rngWELL')
library('randtoolbox')

########


#####################################Test Visuel################################################

## cette fonction fournit les histogrammes des séquences produites par les différents générateurs 
testVisuelHist<-function (k=1000,g=215)
{
  par(mfrow=c(2,2))
  
  x<-VonNeumann(k,graine=g)
  hist(x[,1],100,xlab='',main='Von Neumann')
  #le 100 est pour définir le nombre de rectangle (plus de finesse)
  
  y<-MersenneTwister(k,graine=g)
  hist(y[,1],xlab='',main='Mersenne Twister')
  
  z<-Randu(k,graine = g)
  hist(z,xlab='',main='Randu')
  
  t<-StandardMinimal(k,graine = g)
  hist(t,xlab='',main='StandardMinimal')
    
}

## cette fonction fournit les courbes des séquences produites par les différents générateurs
testVisuelPlot<-function (n=1000, g=215)
{
  par(mfrow=c(2,2))
  
  x<-VonNeumann(n,graine=g)
  plot(x[1:(n-1)], x[2:n],main='Von Neumann')
  
  y<-MersenneTwister(n,graine=g)
  plot(y[1:(n-1)], y[2:n],main='Mersenne Twister')
  
  z<-Randu(n,graine = g)
  plot(z[1:(n-1)], z[2:n],main='Randu')
  
  t<-StandardMinimal(n,graine = g)
  plot(t[1:(n-1)], t[2:n],main='StandardMinimal')
}

##############################################################################################




##################################Test fréquence##############################################

#Cette fonction fournit un test de fréquence pour le générateur Von Neumann 
# il s'agit de visualiser un histogramme des probabilités calculés pour t initialisations 
# différentes , une courbe des probabilité en fonctions de ces graines d'initialisation
# Ainsi qu'une représentation de la loi de probabilité suivi par le générateur 
# n représente le nombre de bits pris en compte dans chacune des séqeunces, il s'agit de 14
# pour Mersenne obtenu grâce à la partie entière de log2(9999) + 1
testFreqVon<-function(k=1000,t=100,n=14)
{
  par(mfrow=c(2,2))
  
  freq<-rep(1,t)
  graine<-rep(1,t)
  sobs<-rep(1,t)
  
  for( i in 1:t)
  {
    graine[i]<-sample.int(10000,1)
    y<-VonNeumann(k,graine=graine[i])
    freq[i]<-Frequency(y,n)
    sobs[i]<-Sobs(y,n)
  }
  
  hist(freq,100,xlab='',main='Fréquence de probabilité Von Neuman')
  plot(graine,freq, main='Courbe des probabilités en fonction des graines Von Neuman')
  abline(h=0.01, col="red")
  plot(sobs,freq,main='Verification de la loi normal pour Von Neumann')
}

#Cette fonction fournit un test de fréquence pour le générateur Mersenne Twister
# il s'agit de visualiser un histogramme des probabilités calculés pour t initialisations 
# différentes , une courbe des probabilité en fonctions de ces graines d'initialisation
# Ainsi qu'une représentation de la loi de probabilité suivi par le générateur 
# n représente le nombre de bits pris en compte dans chacune des séqeunces, il s'agit de 32
# pour Mersenne 
testFreqMers<-function(k=1000,t=100,n=32)
{
  par(mfrow=c(2,2))
  
  freq<-rep(1,t)
  graine<-rep(1,t)
  sobs<-rep(1,t)
  
  for( i in 1:t)
  {
    graine[i]<-sample.int(10000,1)
    y<-MersenneTwister(k,graine=graine[i])
    freq[i]<-Frequency(y,n)
    sobs[i]<-Sobs(y,n)
  }
  
  hist(freq,100,xlab='',main='Fréquence de probabilité Mersenne Twister')
  plot(graine,freq, main='Courbe des probabilités en fonction des graines Mersenne Twister')
  abline(h=0.01, col="red")
  plot(sobs,freq,main='Verification de la loi normal pour Mersenne Twister')
}

#Cette fonction fournit un test de fréquence pour le générateur Randu
# il s'agit de visualiser un histogramme des probabilités calculés pour t initialisations 
# différentes , une courbe des probabilité en fonctions de ces graines d'initialisation
# Ainsi qu'une représentation de la loi de probabilité suivi par le générateur 
# n représente le nombre de bits pris en compte dans chacune des séqeunces, il s'agit de 31
# pour Randu 
testFreqRandu<-function(k=1000, t=100, n=31)
{
  par(mfrow=c(2,2))
  
  freq<-rep(1,t)
  graine<-rep(1,t)
  sobs<-rep(1,t)
  
  for( i in 1:t)
  {
    graine[i]<-sample.int(10000,1)
    y<-Randu(k,graine=graine[i])
    freq[i]<-Frequency(y,n)
    sobs[i]<-Sobs(y,n)
  }
  
  hist(freq,100,xlab='',main='Fréquence de probabilité Randu')
  plot(graine,freq, main='Courbe des probabilités en fonction des graines Randu')
  abline(h=0.01, col="red")
  plot(sobs,freq,main='Verification de la loi normal pour Randu')
}

#Cette fonction fournit un test de fréquence pour le générateur Standard Minimal
# il s'agit de visualiser un histogramme des probabilités calculés pour t initialisations 
# différentes , une courbe des probabilité en fonctions de ces graines d'initialisation
# Ainsi qu'une représentation de la loi de probabilité suivi par le générateur
# n représente le nombre de bits pris en compte dans chacune des séqeunces, il s'agit de 31
# pour Standard Minimal 
testFreqStd<-function(k=1000, t=100, n=31)
{
  par(mfrow=c(2,2))
  
  freq<-rep(1,t)
  graine<-rep(1,t)
  sobs<-rep(1,t)
  
  for( i in 1:t)
  {
    graine[i]<-sample.int(10000,1)
    y<-StandardMinimal(k,graine=graine[i])
    freq[i]<-Frequency(y,n)
    sobs[i]<-Sobs(y,n)
  }
  
  hist(freq,100,xlab='',main='Fréquence de probabilité StandardMinimal')
  plot(graine,freq, main='Courbe des probabilités en fonction des graines StandardMinimal')
  abline(h=0.01, col="red")
  plot(sobs,freq,main='Verification de la loi normal pour StandardMinimal')
}

##############################################################################################




###################################Test Runs##################################################

testRuns<-function()
{
  ##varier les valeurs de graines 
  g<-sample.int(10000,1)
  
  x<-VonNeumann(1000,graine=g)
  ##??? on prend log2 de 9999 pour définir le nbre de bit de von neumann
  Runs(x,14)
  Runsbis(x,14)
  
  y<-MersenneTwister(100,graine=g)
  Runs(y,32)
  Runsbis(y,32)
  
  z<-Randu(1000,graine = g)
  Runs(z,31)
  Runsbis(z,31)
  
  t<-StandardMinimal(1000,graine = g)
  Runs(t,31)
  Runsbis(t,31)
}

#Cette fonction fournit un test de runs pour le générateur Von Neumann 
# il s'agit de visualiser un histogramme des probabilités calculés pour t initialisations 
# différentes et une courbe des probabilité en fonctions de ces graines d'initialisation
# n représente le nombre de bits pris en compte dans chacune des séqeunces, il s'agit de 14
# pour Von Neumann obtenu grâce à la partie entière de log2(9999) + 1
testRunsVon<-function(k=1000,t=100,n=14)
{
  par(mfrow=c(1,2))
  
  freq<-rep(1,t)
  graine<-rep(1,t)
  
  for( i in 1:t)
  {
    graine[i]<-sample.int(10000,1)
    freq[i]<-Runs(y<-VonNeumann(k,graine=graine[i]),n)
  }
  
  hist(freq,100,xlab='',main='Fréquence de probabilité Von Neuman version Runs')
  plot(graine,freq, main='Courbe des probabilités en fonction des graines Von Neuman (Runs)')
  abline(h=0.01, col="red")
  
}
#Cette fonction fournit un test de runs pour le générateur Mersenne Twister 
# il s'agit de visualiser un histogramme des probabilités calculés pour t initialisations 
# différentes et une courbe des probabilité en fonctions de ces graines d'initialisation
# n représente le nombre de bits pris en compte dans chacune des séqeunces, il s'agit de 32
# pour Mersenne
testRunsMers<-function(k=1000,t=100,n=32)
{
  par(mfrow=c(1,2))
  
  freq<-rep(1,t)
  graine<-rep(1,t)
  
  for( i in 1:t)
  {
    graine[i]<-sample.int(10000,1)
    freq[i]<-Runs(y<-MersenneTwister(k,graine=graine[i]),n)
  }
  
  hist(freq,100,xlab='',main='Fréquence de probabilité Mersenne Twister version Runs')
  plot(graine,freq, main='Courbe des probabilités en fonction des graines Mersenne Twister (Runs)')
  abline(h=0.01, col="red")
}

#Cette fonction fournit un test de runs pour le générateur Randu 
# il s'agit de visualiser un histogramme des probabilités calculés pour t initialisations 
# différentes et une courbe des probabilité en fonctions de ces graines d'initialisation
# n représente le nombre de bits pris en compte dans chacune des séqeunces, il s'agit de 31
# pour Randu
testRunsRandu<-function(k=1000,t=100,n=31)
{
  par(mfrow=c(1,2))
  
  freq<-rep(1,t)
  graine<-rep(1,t)
  
  for( i in 1:t)
  {
    graine[i]<-sample.int(10000,1)
    freq[i]<-Runs(y<-Randu(k,graine=graine[i]),n)
  }
  
  hist(freq,100,xlab='',main='Fréquence de probabilité Randu version Runs')
  plot(graine,freq, main='Courbe des probabilités en fonction des graines Randu (Runs)')
  abline(h=0.01, col="red")
  
}

#Cette fonction fournit un test de runs pour le générateur Standard Minimal 
# il s'agit de visualiser un histogramme des probabilités calculés pour t initialisations 
# différentes et une courbe des probabilité en fonctions de ces graines d'initialisation
# n représente le nombre de bits pris en compte dans chacune des séqeunces, il s'agit de 31
# pour Standard Minimal
testRunsStd<-function(k=1000,t=100,n=31)
{
  par(mfrow=c(1,2))
  
  freq<-rep(1,t)
  graine<-rep(1,t)
  
  for( i in 1:t)
  {
    graine[i]<-sample.int(10000,1)
    freq[i]<-Runs(y<-StandardMinimal(k,graine=graine[i]),n)
  }
  
  hist(freq,100,xlab='',main='Fréquence de probabilité Standard Minimal version Runs')
  plot(graine,freq, main='Courbe des probabilités en fonction des graines Standard Minimal(Runs)')
  abline(h=0.01, col="red")
}
##############################################################################################




###################################Test Ordre#################################################

testOrdre<-function()
{
  ##varier les valeurs de graines 
  g<-sample.int(10000,1)
  g
  x<-VonNeumann(1000,graine=g)
  ##??? on prend log2 de 9999 pour définir le nbre de bit de von neumann
  Von<-order.test(x[,1],d=4,echo=FALSE)$p.value
  Von
  
  y<-MersenneTwister(1000,graine=g)
  Mers<-order.test(y[,1],d=4,echo=FALSE)$p.value
  Mers
  z<-Randu(1000,graine = g)
  Ran<-order.test(z,d=4,echo=FALSE)$p.value
  Ran
  t<-StandardMinimal(1000,graine = g)
  Std<-order.test(t,d=4,echo=FALSE)$p.value
  Std
}

##############################################################################################

testOrdreVon<-function(k=1000,t=100)
{
  par(mfrow=c(1,2))
  
  ordre<-rep(1,t)
  graine<-rep(1,t)
  
  for( i in 1:t)
  {
    graine[i]<-sample.int(10000,1)
    x<-VonNeumann(k,graine=graine[i])
    ordre[i]<-order.test(x[,1],d=4,echo=FALSE)$p.value
  }
  
  hist(ordre,100,xlab='',main='Fréquence de probabilité Von Neuman version Ordre')
  plot(graine,ordre, main='Courbe des probabilités en fonction des graines Von Neuman (Ordre)')
  abline(h=0.01, col="red")
}

testOrdreMers<-function(k=1000,t=100)
{
  par(mfrow=c(1,2))
  
  ordre<-rep(1,t)
  graine<-rep(1,t)
  
  for( i in 1:t)
  {
    graine[i]<-sample.int(10000,1)
    x<-MersenneTwister(k,graine=graine[i])
    ordre[i]<-order.test(x[,1],d=4,echo=FALSE)$p.value
  }
  
  hist(ordre,100,xlab='',main='Fréquence de probabilité MT version Ordre')
  plot(graine,ordre, main='Courbe des probabilités en fonction des graines MT (Ordre)')
  abline(h=0.01, col="red")
}

testOrdreRandu<-function(k=1000,t=100)
{
  par(mfrow=c(1,2))
  
  ordre<-rep(1,t)
  graine<-rep(1,t)
  
  for( i in 1:t)
  {
    graine[i]<-sample.int(10000,1)
    ordre[i]<-order.test(Randu(k,graine=graine[i]),d=4,echo=FALSE)$p.value
  }
  
  hist(ordre,100,xlab='',main='Fréquence de probabilité Randu version Ordre')
  plot(graine,ordre, main='Courbe des probabilités en fonction des graines Randu (Ordre)')
  abline(h=0.01, col="red")
}

testOrdreStd<-function(k=1000,t=100)
{
  par(mfrow=c(1,2))
  
  ordre<-rep(1,t)
  graine<-rep(1,t)
  
  for( i in 1:t)
  {
    graine[i]<-sample.int(10000,1)
  
    ordre[i]<-order.test(StandardMinimal(k,graine=graine[i]),d=4,echo=FALSE)$p.value
  }
  
  hist(ordre,100,xlab='',main='Fréquence de probabilité Std version Ordre')
  plot(graine,ordre, main='Courbe des probabilités en fonction des graines Std (Ordre)')
  abline(h=0.01, col="red")
}


