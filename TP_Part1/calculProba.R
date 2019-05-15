Frequency <-function (x,nb)
{
  sn<-0
  for(i in 1:length(x))
  {
    c<-binary(x[i])
    
    for(j in (33-nb):32)
    {
      sn<-sn+2*c[j]-1
    } 
  }
  
  sobs<-abs(sn)/sqrt(length(x)*nb)
  pvaleur<-2*(1-pnorm(sobs))
  return(pvaleur)
}

Sobs <-function (x,nb)
{
  sn<-0
  for(i in 1:length(x))
  {
    c<-binary(x[i])
    
    for(j in (33-nb):32)
    {
      sn<-sn+2*c[j]-1
    } 
  }
  
  sobs<-abs(sn)/sqrt(length(x)*nb)
  return(sobs)
}
#Ceci est l'implémentation de la fonction Runs de manière naive sans tenir compte de la compléxité mémoire 
Runs<-function(x,nb)
{
  vBit<-0 
  n<-length(x)*nb
  for(i in 1:length(x))
  {
    c<-binary(x[i])
    
    vBit<-c(vBit,c[(33-nb):32]); 
  }
  
  p<-sum(vBit)/n
  
  if(abs(p-1/2)>=(2/sqrt(n)))
  {
    return (0.0)
  }
  vobs<-1
  for (i in 2:n)
  {
    if(vBit[i]!=vBit[i+1])
    {
      vobs<-vobs+1
    }
  }
  return (2*( 1-pnorm( abs(vobs-2*n*p*(1-p)) / (2*sqrt(n)*p*(1-p))) ) )
}
#Cela est une implémentation moins gourmande en mémoire, nous utiliserons celle ci pour nos tests 
Runsbis<-function(x,nb)
{
  p<-0
  n<-length(x)*nb
  
  for(i in 1:length(x))
  {
    c<-binary(x[i])
    p<-p+sum(c[(33-nb):32])
  }
  p<-p/n
  
  if(abs(p-1/2)>=(2/sqrt(n)))
  {
    return (0.0)
  }
  vobs<-1
  temp<-0
  for (i in 1:length(x))
  {
    c<-binary(x[i])
    
    if (i>1 && c[(33-nb)]!=temp)
    {
      vobs<-vobs+1
    }
    
    for(j in (33-nb):31)
    {
      if(c[j]!=c[j+1])
      {
        vobs<-vobs+1
      }
    }
    temp<-c[32]
  }
  return (2*( 1-pnorm( abs(vobs-2*n*p*(1-p)) / (2*sqrt(n)*p*(1-p))) ) )
}