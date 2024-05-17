####### Infection_spread_students_1.0.3.R
#### update: commented up to ####7 + fixed console spamming

#### 0. Set working directory ####
# clear environment and set working directory
rm(list=ls())
# setwd("~/UZH/Agent-based modelling in R/Agend_based_model") #Hyewon
# setwd("~/Documents/GitHub/Agend_based_model") #Miriam
# setwd("C:/Users/Laura Andres/Documents/GitHub/Agend_based_model/") #Laura


#### 1. Determine Parameters ####
beta <- 0.3
students <- 100
transmission_dist <- 1.5 # in number of seats between students. dist 1 = one seat(60cm)
random_absence <- 0.05 # absence of student for reasons other than illness
lectures_per_week <- 1 # per week
weeks <- 13 # fix in the end to 18 weeks -> one semester + study phase
initial_prob <- 0.05
rounds <- lectures_per_week*weeks

#### 2. Make a dataframe that monitors student status each week(round) ####
new_meta=function(){
  meta=data.frame(
    infected = rep(NA, rounds),
    infected_random = NA,
    immunity = NA,
    recovering = NA,
    sick_but_going = NA,
    mean_stored = NA
  )
  return(meta)
}

#### 3. Make a dataframe that contains student information. Will be renewed every week(round) ####
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


#### 4. Get infection status for each week(round) ####
one_round=function(nth_round, beta, students, transmission_dist,random_absence,lectures_per_week,weeks,initial_prob,rounds, meta, seats, health){
  # 4a. Randomly assign seats for attending students
  ## 1) check absence
  ### absence due to voluntary decision (random absence)
  health$absence <- sample(0:1, students, replace = TRUE, prob = c(1-random_absence, random_absence))
  ### absence due to infection
  health$absence[which(health$missed_rounds>=1)] <- 1
  
  ## 2) assign seats to attending students
  attending_students <- which(health$absence==0)
  health$location[attending_students] <- sample(1:nrow(seats), length(attending_students), replace = FALSE)
  health$row[attending_students] <- seats$rows[health$location]
  health$col[attending_students] <- seats$cols[health$location]
  
  
  # 4b. Calculate probability of infection for each students
  ## 1) assign students - who are not immune - who got infected outside of class by chance
  possible_hosts <- which(health$immunity == 0)
  infected_lastround <- which(health$infected_pre==1) # save students who got infected in class
  health$infected_pre[possible_hosts] <- sample(0:1, length(possible_hosts), replace = TRUE, prob=c(1-initial_prob,initial_prob))
  infected_random <- setdiff(which(health$infected_pre[possible_hosts] == 1), infected_lastround) # vector of students who got infected outside of class by chance
  health$immunity[which(health$infected_pre == 1)] <- 1 # students who are infected are immune
  
  
  ## 2) calculate probability of infection for each students
  #distance matrix for all seats
  distances <- as.matrix(dist(cbind(health$row,health$col)))
  distances <- distances[,which(health$infected_pre==1), drop=FALSE]
  #get number of exposures for each students
  health$exposures <- rowSums(distances<=transmission_dist)
  #get probability of infection by number of exposures
  health$p <- 1-(1-beta)^health$exposures
  
  # 4c. From the probabilities, determine which student get infected
  for(i in 1:nrow(health)){
    if(health$immunity[i] == 1){
      health$infected_post[i] <- 0
    } else{
      health$infected_post[i] <- sample(0:1, 1, prob = c(1-health$p[i],health$p[i]))
    }
  }
  
  # 4d. Loop for each week and track infection status each round
  ## 1) update infection status in "meta" dataframe
  meta$infected[nth_round] <- sum(health$infected_post)
  meta$immunity[nth_round] <- sum(health$immunity)
  meta$recovering[nth_round] <- length(which(health$missed_rounds>=1))
  meta$sick_but_going[nth_round] <- length(which(health$sick_but_going>=1))
  meta$infected_random[nth_round] <- length(infected_random)
  
  ## 2) renew "health" dataframe for next week(round)
  ### determine student who will stay home due to infection and return to school after recovery
  quarantine <- c(which(health$sick_but_going==lectures_per_week), infected_random)
  back_to_school <- which(health$missed_rounds==lectures_per_week)
  ### clear infection status of students in quarantine
  health$sick_but_going[quarantine] <- 0
  health$infected_post[quarantine] <- 0
  ### count missed rounds of students in quarantine
  health$missed_rounds[quarantine] <- health$missed_rounds[quarantine]+1
  ### get students back to class
  health$missed_rounds[back_to_school] <- 0
  health$infected_pre[back_to_school] <- 0
  # add counts for students who got sick and still goes to school 
  health$sick_but_going[which(health$infected_post==1)] <- health$sick_but_going[which(health$infected_post==1)] +1
  #transfer infected_post as infect_pre of next round
  health$infected_pre <- health$infected_post
  #clear other columns
  health$attendance <- 0
  health$location <- 0
  health$p <- 0
  health$infected_post <- 0
  
  ## 3) get mean for the defined weeks (now 13 weeks)
  if(nth_round <= weeks) {
    meta$infected_stored[nth_round] <- meta$infected[nth_round]
  } else {
    meta$infected_stored[nth_round] <- 0
  }
  
  
  return(list(health = health, meta = meta))
}


#### 5. Prepare - conditions(number of seats), number of simulations, dataframe to track results - to run the simulation ####
n=100 # number of simulations
nrows <- ncols <- c(10:20) # conditions
max_values_df= data.frame(matrix(NA, nrow = n, ncol = length(nrows))) # make empty dataframe that will store all peaks of infection for each condition
colnames(max_values_df) <- c("10x10", "11x11", "12x12", "13x13", "14x14", "15x15", "16x16", "17x17", "18x18", "19x19", "20x20")

#### 6. Run simulations for each conditions ####
for(j in nrows) {
  # 6a. make a classroom with j rows of seats
  seats <- expand.grid(rows=1:j, cols=1:j) 
  seats$ID <- 1:nrow(seats) # "seats" dataframe - rows, columns, ID
  
  # 6b. make a dataframe to track result in each simulation
  column_names <- paste("trial", 1:n, sep= "_")
  infected_df <- data.frame(matrix(NA, nrow = rounds, ncol = n))
  colnames(infected_df) <- column_names
  
  ## empty vector to update means of infected students
  mean_simulation=rep(NA, n)
  
  # repeat simulation 100 times for each conditions (for statistical validation)
  # 6c. simulate for each condtion n times
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
  
  # 6d. get descriptive values from infected_df
  ## set NA as 0
  infected_df[is.na(infected_df)]=0
  
  ## get means and sd and max and min
  mean_weeks=rowMeans(infected_df)
  sd_weeks=apply(infected_df,1,sd)
  sd_simulation=apply(infected_df,2,sd)
  max_simulation=apply(infected_df,2,max)
  min_simulation=apply(infected_df,2,min)
  
  # 6e. update max_values_df
  max_values_df[j-9]=max_simulation
}


#### 7. Get means of maximum value ####
max_means=colMeans(max_values_df)
max_sd=apply(max_values_df, 1, sd)


#### 8. Plot results ####

# Boxplot for each Classroom size and its highest number of infected students within the 13 weeks
boxplot(max_values_df, 
        main = "Boxplot for each Size Condition", 
        xlab = "Classroom Size", 
        ylab = "Highest Number of infected Individuals within 13 Weeks", 
        col = "#DDA0DD")

# Plot the optimum of size and cost
## max_means and max_sd 
max_means <- colMeans(max_values_df)
max_sd <- apply(max_values_df, 1, sd)

## Define cost based on classroom size using a logarithmic scale
classroom <- 10:20
starting_value <- 10000

## Use a logarithmic function to define the cost
cost <- starting_value * log(classroom)

## Plot mean infected
plot(x = classroom, y = max_means, type = 'l', col = 'violet', lwd = 2, ylab = 'Mean Infected', xlab = 'Classroom Size', xaxt = 'n')
axis(1, at = classroom)

## Overlay log(Cost)
par(new = TRUE)
plot(x = classroom, y = cost, type = 'l', col = 'blue', lwd = 2, axes = FALSE, xlab = '', ylab = '')
axis(4) 
mtext("Cost in CHF", side = 4, line = 2, col = "blue")

## Add legends
legend("top", legend = c("Mean Infected", "Cost in CHF"), col = c("violet", "blue"), lty = 1, lwd = 2)

## Add a title
title(main = "Plot of Mean Infected and Cost in CHF")

# Boxplot for highest, lowest and mean value over the 13 weeks
## Make a dataframe with max, min, mean
values_df <- data.frame(Minimum = min_simulation, Mean = mean_simulation, Maximum = max_simulation)

## Plot it
boxplot(values_df, 
        main = "Variation for Classroom of Size 20x20", 
        xlab = "Infected Values", 
        ylab = "Number of Infected Individuals", 
        col = "#FFA07A", #for other plots: lightblue, lightgreen
        ylim = c(0,30),
        cex.axis = 1.0, cex.lab = 1.0, cex.main = 2) #adjust size of text
axis(2, at = seq(0, 30, by = 5))
## Comment: This was adjusted manually for size 10x10, 15x15, and 20x20 as it was easier for as to do so than store informaion within a loop

##############
