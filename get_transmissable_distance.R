beta=0.8

## This is a function that calculates maximum transmissible distance from transmission rate(or beta)
get_transmissable_distance=function(transmission_rate, threshold){
  p=transmission_rate
  i=0
  while(p>threshold){
    p=p/(2^i)
    i=i+1
    print(paste(p,i))
  }
  return(i)
}

distance=get_transmissable_distance(beta, threshold = 0.01)
