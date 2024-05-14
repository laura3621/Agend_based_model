####### Infection_spread_students_1.0.3.R
### 0. Retrieve functions
### 1. Determine Parameters
#### a. number of students
#### b. number of seats and rows
#### c. transmission rate
#### d. recovery period
#### e. random absence rate
#### f. lectures per week
#### g. probability of getting an infection without contact
#### h. number of rounds
### 2. Build a classroom and create "seat dataframe"
### 3. Make a dataframe of students, contains health status and attendance of all students
### 4. Randomly assign seats for attending students
#### a. get attendance status for each students
#### b. assign seats to attending students
### 5. Calculate probability of infection for each students
#### a. get students(who are not immune)who got infected outside of class
#### b. calculate probability of infection for each students
### 6. From the probabilities, determine which student get infected
### 7. Loop multiple times and track infection status each round
#### a. update infection status for each round in meta sheet (?)
#### b. update "health" dataframe
rm(list=ls())


#### 0. Retrieve functions, set working directory

setwd("~/UZH/Agent-based modelling in R/Agend_based_model")
setwd("~/Documents/GitHub/Agend_based_model") #Miriam

#setwd("~/UZH/Agent-based modelling in R/Agend_based_model")
setwd("C:/Users/Laura Andres/Documents/GitHub/Agend_based_model/")


rm(list=ls())

source("get_transmissable_distance.R")
source("probability_to_binary.R")

#### 1. Determine Parameters
beta <- 0.3
students <- 100
transmission_dist <- 1.5 #in number of seats between students
random_absence <- 0.05
lectures_per_week <- 1 #per week
weeks <- 13 #fix in the end to 18 weeks -> one semester + study phase
transmission_dist <- 2 #get_transmissable_distance(beta, threshold = 0.05) #dist 1 = one seat(60cm)
random_absence <- 0.05
lectures_per_week <- 1 #per week
initial_prob <- 0.05
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
      health$infected_post[i] == 0
    } else{
      health$infected_post[i]<-probability_to_binary(health$p[i])
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
nrows <- ncols <- c(10:20)

# make empty dataframe that will store the peak of infection
max_values_df= data.frame(matrix(NA, nrow = n, ncol = length(nrows)))
colnames(max_values_df)=paste("size", 1:length(nrows)+9, sep="_")

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
  max_values_df[j-9]=max_simulation
  
}

max_means=colMeans(max_values_df)
max_sd=apply(max_values_df, 1, sd)

# change the column names of max_values 
colnames(max_values_df) <- c("10x10", "11x11", "12x12", "13x13", "14x14", "15x15", "16x16", "17x17", "18x18", "19x19", "20x20")

boxplot(max_values_df, 
        main = "Boxplots for each Condition", 
        xlab = "Classroom Size", 
        ylab = "Highest nr. of infected Individuals per Condition", 
        col = "lightpink")


# One plot for both curves
## values for mean_infected curve
max_means=colMeans(max_values_df)
max_sd=apply(max_values_df, 1, sd)

## values for cost
cost=c(1:11)
cost = log(cost)
plot(cost, type='l')

## plots
plot(max_means, type='l', col='violet', lwd = 2, ylab ='cost')
par(new = TRUE)
plot(cost, type='l', col='blue', lwd = 2, axes = FALSE, xlab = '', ylab = '')

# Add the x-axis 
axis(4, at = 1:11, labels = round(log(1:11), 2), axis.title = 'mean maximum Infected')
mtext("log(cost)", side = 4, line = 2, col = "blue")


# Add legends
legend(legend = c("Mean Infected", "log(Cost)"), col = c("violet", "blue"), lty = 1, lwd = 2)

# Add a title
title(main = "Plot of Mean Infected and log(Cost) with Intersection Highlighted")

##############

# One plot for both curves
## values for mean_infected curve
max_means <- colMeans(max_values_df)
max_sd <- apply(max_values_df, 1, sd)

## values for cost
extended_range <- 20  # Desired range for the x-axis of the second plot
cost <- log(seq(1, extended_range))

# Plot the first curve (red)
plot(max_means, type = 'l', col = 'red', lwd = 2, ylab = 'Mean maximum Infected', col.axis = 'red')
# Overlay the second curve (blue)
par(new = TRUE)
plot(cost, type = 'l', col = 'blue', lwd = 2, axes = FALSE, xlab = '', ylab = '')

# Adjust the x-axis of the second plot
axis(4, at = 1:11, labels = round(log(1:11), 2), col.axis = "blue", col.ticks = "blue")
mtext("log(cost)", side = 4, line = 2, col = "blue")
mtext("Mean Maximum Infected", side = 4, line = 3, col = "red" ) # Add y-axis title for axis 4

# Add legends
legend(legend = c("Mean Infected", "log(Cost)"), col = c("red", "blue"), lty = 1, lwd = 2)

# Add a title
title(main = "Plot of Mean Infected and log(Cost)")

