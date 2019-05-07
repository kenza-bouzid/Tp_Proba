VonNeumann <- function(n, p=1, graine)
{
  x <-  rep(graine,n*p+1)
  for(i in 2:(n*p+1))
  {
    numbers <- strsplit(format(x[i-1]^2,scientific=FALSE),'')[[1]]
    while(length(numbers)>4){ 
      numbers <- numbers[2:(length(numbers)-1)] 
    }
    x[i] <- as.numeric(numbers)%*%(10^seq(length(numbers)-1,0,-1))
  }
  x <- matrix(x[2:(n*p+1)],nrow=n,ncol=p)
  return(x)
}


MersenneTwister <- function(n, p=1, graine)
{
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(x)
}


binary <- function(x)
{
  if((x<2^31)&(x>=0))
    return( as.integer(rev(intToBits(as.integer(x)))) )
  else{
    if((x<2^32)&(x>0))
      return( c(1,binary(x-2^31)[2:32]) )
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}

Congruence <- function(k, graine, m, a, b=0)
{
  x<-rep(graine,k)
  for(i in 1:(k-1))
  {
    x[i+1]<-(a*x[i]+b)%%m
  }
 #for(i in 1:k)
  #{
    #x[i]<-x[i]/m
  #}
  return(x)
}

Randu <- function (k , graine)
{
  return(Congruence(k,graine, 2^31,65539))
}

StandardMinimal <- function (k , graine)
{
  return(Congruence(k,graine, 2^31-1,16807))
}

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


