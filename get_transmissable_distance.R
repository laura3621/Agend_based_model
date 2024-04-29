beta=0.3

get_transmissable_distance=function(transmission_rate){
  p=transmission_rate
  i=0
  while(p>0.001){
    p=p/(2^i)
    i=i+1
    print(paste(p,i))
  }
  return(i)
}

distance=get_transmissable_distance(beta)
