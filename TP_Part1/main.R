#1.2.1 Test Visuel 

source('generateurs.R')
library('rngWELL')
library('randtoolbox')

testVisuel1<-function ()
{
  par(mfrow=c(2,2))
  
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
  par(mfrow=c(2,2))
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

testFreqVon<-function()
{
  freq<-rep(1,100)
  for( i in 1:100)
  {
    freq[i]<-Frequency(VonNeumann(1000,graine=sample.int(10000,1)),14)
  }
  hist(freq,xlab='',main='Histo frequence Von Neumann pour 100 valeurs de graines aléatoires')

}

testFreqMers<-function()
{
  freq<-rep(1,100)
  graine<-rep(1,100)
  for( i in 1:100)
  {
    graine[i]<-sample.int(10000,1)
    freq[i]<-Frequency(MersenneTwister(1000,graine=graine[i]),32)
  }
  hist(freq,100,xlab='',main='Histo frequence Mersenne Twister pour 100 valeurs de graines aléatoires')
  plot(graine,freq, main='plot frequence Mersenne Twister pour 100 valeurs de graines aléatoires')
  
}

testLoiNormaleVon <-function()
{
  freq<-rep(1,100)
  graine<-rep(1,100)
  sobs<-rep(1,100)
  for( i in 1:100)
  {
    graine[i]<-sample.int(10000,1)
    y<-VonNeumann(1000,graine=graine[i])
    freq[i]<-Frequency(y,32)
    sobs[i]<-Sobs(y,32)
  }
  
  plot(sobs,freq,main='Verification de la loi normal pour Von Neumann')
}

testLoiNormaleMers <-function()
{
  freq<-rep(1,100)
  graine<-rep(1,100)
  sobs<-rep(1,100)
  for( i in 1:100)
  {
    graine[i]<-sample.int(10000,1)
    y<-MersenneTwister(1000,graine=graine[i])
    freq[i]<-Frequency(y,32)
    sobs[i]<-Sobs(y,32)
  }
  
  plot(sobs,freq,main='Verification de la loi normal pour Mersenne Twister')
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



