# This is a function that gets binary state from a probability
probability_to_binary=function(p){
  interaction_yes=sample(0:1, 1, prob = c(1-p,p))
  return(interaction_yes)
}
