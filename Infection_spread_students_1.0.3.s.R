####### Infection_spread_students_1.0.3.s.R  (line plot with 3 lines)

rm(list=ls())

#### 0. Retrieve functions, set working directory
setwd("~/UZH/Agent-based modelling in R/Agend_based_model")
rm(list=ls())



#### 1. Determine Parameters
beta <- 0.3
students <- 100
transmission_dist <- 1.5 #in number of seats between students
random_absence <- 0.05
lectures_per_week <- 1 #per week
weeks <- 13 #fix in the end to 18 weeks -> one semester + study phase
random_absence <- 0.05
rounds <- lectures_per_week*weeks
mean_stored <-  0


# create new metasheet
new_meta=function(){
  meta=data.frame(
    infected = rep(NA, rounds),
    immunity = NA,
    recovering = NA,
    sick_but_going = NA,
    mean_stored = NA
  )
  return(meta)
}




#### 2. Make a dataframe that contains student information
new_health=function(){
  health <- data.frame(
    ID = 1:students,
    infected_pre = 0,
    absence = 0,
    location = 0,
    row=0,
    col=0,
    missed_rounds = 0, 
    past_affections = 0, 
    p = 0, #probability of getting infected
    infected_post = 0, #infection status after this round
    immunity = 0,
    sick_but_going = 0)
  return(health)
}



one_round=function(nth_round, beta, students, transmission_dist,random_absence,lectures_per_week,weeks,initial_prob,rounds, meta, seats, health){
  #### 4. Randomly assign seats for attending students
  # 4a. get absent students
  ## making voluntary decision not to come..
  health$absence <- sample(0:1, students, replace = TRUE, prob = c(1-random_absence, random_absence))
  ## cannot come anyway due to infection
  health$absence[which(health$missed_rounds>=1)] <- 1
  
  # 4b. assign seats to attending students
  attending_students <- which(health$absence==0)
  health$location[attending_students]=sample(1:nrow(seats), length(attending_students), replace = FALSE)
  health$row[attending_students]=seats$rows[health$location]
  health$col[attending_students]=seats$cols[health$location]
  
  
  ### 5. Calculate probability of infection for each students
  # 5a. get students(who are not immune)who got infected outside of class
  possible_hosts=which(health$immunity == 0)
  health$infected_pre[possible_hosts]=sample(0:1, length(possible_hosts), replace = TRUE, prob=c(1-initial_prob,initial_prob))
  health$immunity[which(health$infected_pre ==1)]=1
  
  
  # 5b. calculate probability of infection for each students
  #distance matrix for all seats
  distances <- as.matrix(dist(cbind(health$row,health$col)))
  distances <- distances[,which(health$infected_pre==1), drop=FALSE]
  #get number of exposures for each students
  health$exposures<-rowSums(distances<=transmission_dist)
  #get probability of infection by number of exposures
  health$p=1-(1-beta)^health$exposures
  
  ### 6. From the probabilities, determine which student get infected
  for(i in 1:nrow(health)){
    if(health$immunity[i] == 1){
      health$infected_post[i] <- 0
    } else{
      health$infected_post[i] <- sample(0:1, 1, prob = c(1-health$p[i],health$p[i]))
    }
  }
  
  
  ### 7. Loop multiple times and track infection status each round
  # 7a. update infection status for each round in meta sheet
  meta$infected[nth_round] <- sum(health$infected_post)
  meta$immunity[nth_round] <- sum(health$immunity)
  meta$recovering[nth_round] <- length(which(health$missed_rounds>=1))
  meta$sick_but_going[nth_round] <- length(which(health$sick_but_going>=1))
  
  #7b. update "health" dataframe
  quarantine <- which(health$sick_but_going==lectures_per_week)
  # print(quarantine)
  back_to_school <- which(health$missed_rounds==lectures_per_week)
  # print(back_to_school)
  #clear infection status of students in quarantine
  health$sick_but_going[quarantine] <- 0
  health$infected_post[quarantine] <- 0
  #count missed rounds of students in quarantine
  health$missed_rounds[quarantine] <- health$missed_rounds[quarantine]+1
  #get students back to class + immunity
  health$missed_rounds[back_to_school] <- 0
  health$immunity[back_to_school] <- 1
  health$infected_pre[back_to_school] <- 0
  #add counts for students who got sick and still goes to school
  health$sick_but_going[which(health$infected_post==1)] <-health$sick_but_going[which(health$infected_post==1)] +1
  #transfer infected_post as infect_pre of next round
  health$infected_pre <- health$infected_post
  #clear other columns
  health$attendance <- 0
  health$location <- 0
  health$p <- 0
  health$infected_post <- 0
  
  # mean for the defined weeks (now 13 weeks)
  if(nth_round <= weeks) {
    meta$infected_stored[nth_round] <- meta$infected[nth_round]
  } else {
    meta$infected_stored[nth_round] <- 0
  }
  
  return(list(health = health, meta = meta))
}



#### 3. Build the class room - dataframe$ rows, columns, ID

n=100 #number of simulations
nrows <- ncols <- c(10, 15, 20)

# make empty dataframe that will store the peak of infection
max_values_df= data.frame(matrix(NA, nrow = n, ncol = length(nrows)))
colnames(max_values_df)=paste("size", 1:length(nrows), sep="_")

# make empty dataframe that will store the mean infected students of each week
means_byweek_df=data.frame(matrix(NA, nrow = weeks, ncol = length(nrows)))


for(j in nrows) {
  seats <- expand.grid(rows=1:j, cols=1:j) 
  seats$ID <- 1:nrow(seats)
  
  # make a dataframe to track result in each simulation
  column_names <- paste("trial", 1:n, sep= "_")
  infected_df <- data.frame(matrix(NA, nrow = rounds, ncol = n))
  colnames(infected_df) <- column_names
  
  # repeat simulation 100 times for each conditions (for statistical validation)
  ## empty vector to update means of infected students
  mean_simulation=rep(NA, n)
  
  ## simulate!
  for(trial in 1:n){
    health=new_health()
    meta=new_meta()
    for(nth_round in 1:rounds) {
      results <- one_round(nth_round, beta, students, transmission_dist,random_absence,lectures_per_week,weeks,initial_prob,rounds, meta, seats, health)
      health <- results$health
      meta <- results$meta
    }
    
    ## update mean of infected students
    meta[is.na(meta)]=0
    mean_one_simulation <- sum(meta$infected_stored)/weeks
    mean_simulation[trial]=mean_one_simulation
    
    ## update the result - infected_df - dataframe
    infected_df[trial]=meta$infected
  }
  
  ## get descriptive statistics from infected_df
  
  ### set NA as 0
  infected_df[is.na(infected_df)]=0
  
  ### get means and sd and max and min
  mean_weeks=rowMeans(infected_df)
  sd_weeks=apply(infected_df,1,sd)
  sd_simulation=apply(infected_df,2,sd)
  max_simulation=apply(infected_df,2,max)
  min_simulation=apply(infected_df,2,min)
  
  ### update max_values_df
  max_values_df[which(nrows==j)]=max_simulation
  
  
  ### update means_byweek_df
  means_byweek_df[which(nrows==j)]=mean_weeks
  
  
}

max_means=colMeans(max_values_df)
max_sd=apply(max_values_df, 1, sd)



plot(means_byweek_df$X1, type = "l", col = "lightblue", lwd = 3, xlab = "Index", ylab = "Values", main = "Plot of Three Columns")
lines(means_byweek_df$X2, col = "lightgreen", lwd = 3)
lines(means_byweek_df$X3, col = "#FFA07A", lwd = 3)

# Add legend
legend("topright", legend = c("10x10", "15x15", "20x20"), col = c("lightblue", "lightgreen", "#FFA07A"), lwd = 3, lty = 1, title = "Classroom Size")

# 
# boxplot(max_values_df, 
#         main = "Boxplots for Each Condition", 
#         xlab = "Variable", 
#         ylab = "Value", 
#         col = "lightblue")




##############
