rows=5
columns=6
students=25
seats=expand.grid(rows=1:rows,columns=1:columns)

initial_prob=0.1

#interaction probs
prob_adj_row = 0.7
prob_adj_col = 0.3
prob_diagonal = 0.01

#functions

#track infected individual in 8 adjacant seats
interaction_prob=function(seat, infected_seats, prob_adj_row, prob_adj_col, prob_diagonal){
  individual_prob=c()
  if(paste(seat[1]-1,seat[2]-1) %in% infected_seats){
    individual_prob=c(individual_prob, prob_diagonal)
  }
  if(paste(seat[1]-1,seat[2]) %in% infected_seats){
    individual_prob=c(individual_prob, prob_adj_col)
  }
  if(paste(seat[1]-1,seat[2]+1) %in% infected_seats){
    individual_prob=c(individual_prob, prob_diagonal)
  }
  if(paste(seat[1],seat[2]-1) %in% infected_seats){
    individual_prob=c(individual_prob, prob_adj_row)
  }
  if(paste(seat[1],seat[2]+1) %in% infected_seats){
    individual_prob=c(individual_prob, prob_adj_row)
  }
  if(paste(seat[1]+1,seat[2]-1) %in% infected_seats){
    individual_prob=c(individual_prob, prob_diagonal)
  }
  if(paste(seat[1]+1,seat[2]) %in% infected_seats){
    individual_prob=c(individual_prob, prob_adj_col)
  }
  if(paste(seat[1]+1,seat[2]+1) %in% infected_seats){
    individual_prob=c(individual_prob, prob_diagonal)
  }
  #calculate probability to interact with infected ppl
  product=1
  for(prob in individual_prob){
    product=product*(1-prob)
  }
  p=1-product
  return(p)
}

interaction_binary=function(p){
  interaction_yes=sample(0:1, 1, prob = c(1-p,p))
  return(interaction_yes)
}


#make df
df=data.frame(SUBJECT=1:students, 
              SEAT=rep(NA, students),
              INFECTED=sample(0:1,students, replace = TRUE, prob = c(1-initial_prob, initial_prob)),
              P=rep(NA,students),
              INTERACTION=rep(NA,students)
)

#sample seats
seated.seats=sample(1:nrow(seats), students)

df$SEAT=seats[seated.seats,]

infected_seats=df$SEAT[which(df$INFECTED==1),]
infected_seats=paste(infected_seats[,1], infected_seats[,2])

for(i in 1:nrow(df)){
  seat=df$SEAT[i,]
  print(seat)
  p=interaction_prob(seat, infected_seats, prob_adj_row, prob_adj_col, prob_diagonal)
  df$P[i]=p
  df$INTERACTION[i]=interaction_binary(p)
}

#validate
