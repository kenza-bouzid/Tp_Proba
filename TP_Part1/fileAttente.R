FileMM1<-function(lambda,mu,D)
{
  
  tarrivee<-rexp(1,rate=lambda); 
  xarrivee<-0; 
  while (tarrivee<D){
    xarrivee<-c(xarrivee,tarrivee);
    tsuivant<-tarrivee+rexp(1,rate=lambda); 
    tarrivee<-tsuivant; 
  }
  xdepart<-0; 
  if (length(xarrivee)>1)
  {
    tdepart<-xarrivee[2]+rexp(1,rate=mu);
    if (tdepart<D)
    {
      xdepart<-c(xdepart,tdepart);
      for(i in 3:length(xarrivee))
      {
        tdepart<-xarrivee[i]+rexp(1,rate=mu);
        
        if(xdepart[i-1]>xarrivee[i])
        {
          tdepart<-xdepart[i-1]+rexp(1,rate=mu);
        } 
        if (tdepart<D)
        {
          xdepart<-c(xdepart,tdepart);
        }
        else 
        {
          break;
        }
      }
    }
  }
  
  return (list(xarrivee,xdepart))
}

Evolution<-function(xarrivee,xdepart)
{
  carrivee <-2; #compteur des arrivees 
  cdepart<-2;   #compteur des départs
  nombreDePersonnes<-0;
  temps<-0;
  i<-1; 
  while((carrivee+cdepart)<(length(xarrivee)+length(xdepart)))
  {
    if(carrivee<length(xarrivee) && cdepart<length(xdepart))
    {
      if(xarrivee[carrivee]<xdepart[cdepart])
      {
        temps<-c(temps,xarrivee[carrivee]);
        nombreDePersonnes<-c(nombreDePersonnes,nombreDePersonnes[i]+1);
        carrivee<-carrivee+1; 
      }else{
        temps<-c(temps,xdepart[cdepart]);
        if(xarrivee[carrivee]==xdepart[cdepart])
        {
          nombreDePersonnes<-c(nombreDePersonnes,nombreDePersonnes[i]);
          carrivee<-carrivee+1;
        }
        else {
          nombreDePersonnes<-c(nombreDePersonnes,nombreDePersonnes[i]-1);
        }
        cdepart<-cdepart+1; 
      }
    }else if(carrivee<length(xarrivee)){
      temps<-c(temps,xarrivee[carrivee]);
      nombreDePersonnes<-c(nombreDePersonnes,nombreDePersonnes[i]+1);
      carrivee<-carrivee+1;
    }else{
      temps<-c(temps,xdepart[cdepart]);
      nombreDePersonnes<-c(nombreDePersonnes,nombreDePersonnes[i]-1);
      cdepart<-cdepart+1; 
    }
    i<-i+1; 
  }
  return(list(nombreDePersonnes,temps))
}

MoyenneClient<-function(nombrePersonnes,temps)
{
  somme<-0; 
  for(i in 1:(length(nombrePersonnes)-1)){
    somme<-somme+((temps[i+1]-temps[i])*nombrePersonnes[i+1])
  }
  return (somme/temps[length(temps)])
}

TempsPresence<-function(xarrivee,xdepart)
{
  somme<-0; 
  for (i in 1:length(xdepart))
  {
    somme<-somme+xdepart[i]-xarrivee[i];
  }
  return(somme/length(xdepart)); 
}



  
  
  

     
  
