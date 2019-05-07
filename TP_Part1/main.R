#1.2.1 Test Visuel 

source('D:/MesSauvegardes/Drive_d/insa/3if/S2/Proba/TP_Part1/generateurs.R')
library('rngWELL')
library('randtoolbox')

testVisuel1<-function ()
{
  g<-215
  x<-VonNeumann(1000,graine=g)
  hist(x[,1],100,xlab='',main='Von Neumann')
  #le 100 est pour définir le nombre de rectanle (plus de finesse)
  y<-MersenneTwister(100,graine=g)
  hist(y[,1],xlab='',main='Mersenne Twister')
  z<-Randu(1000,graine = g)
  hist(z,xlab='',main='Randu')
  t<-StandardMinimal(1000,graine = g)
  hist(t,xlab='',main='StandardMinimal')
    
}

testVisuel2<-function ()
{
  g<-215
  n<-1000
  x<-VonNeumann(1000,graine=g)
  plot(x[1:(n-1)], x[2:n],main='Von Neumann')
  y<-MersenneTwister(100,graine=g)
  plot(y[1:(n-1)], y[2:n],main='Mersenne Twister')
  z<-Randu(1000,graine = g)
  plot(z[1:(n-1)], z[2:n],main='Randu')
  t<-StandardMinimal(1000,graine = g)
  plot(t[1:(n-1)], t[2:n],main='StandardMinimal')
  
}

testFrequence<-function()
{
  ##varier les valeurs de graines 
  g<-sample.int(10000,1)
  
  x<-VonNeumann(1000,graine=g)
  ##??? on prend log2 de 9999 pour définir le nbre de bit de von neumann
  Von<-Frequency(x,14)
  Von
  
  y<-MersenneTwister(100,graine=g)
  Mers<-Frequency(y,32)
  Mers
  z<-Randu(1000,graine = g)
  Ran<-Frequency(z,31)
  Ran
  t<-StandardMinimal(1000,graine = g)
  Std<-Frequency(t,31)
  Std
}

testRuns<-function()
{
  ##varier les valeurs de graines 
  g<-sample.int(10000,1)
  
  x<-VonNeumann(1000,graine=g)
  ##??? on prend log2 de 9999 pour définir le nbre de bit de von neumann
  Von<-Runs(x,14)
  Von
  
  y<-MersenneTwister(100,graine=g)
  Mers<-Runs(y,32)
  Mers
  z<-Randu(1000,graine = g)
  Ran<-Runs(z,31)
  Ran
  t<-StandardMinimal(1000,graine = g)
  Std<-Runs(t,31)
  Std
}

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



