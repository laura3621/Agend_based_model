#######
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

#### 0. Retrieve functions
source("get_transmissable_distance.R")
source("probability_to_binary.R")

#### 1. Determine Parameters
beta <- 0.7
students <- 25
transmission_dist <- get_transmissable_distance(beta, threshold = 0.01) #in meters
random_absence <- 0.05
lectures_per_week <- 2 #per week
weeks <- 5
initial_prob <- 0.3
rounds <- lectures_per_week*weeks

# create metasheet
meta=data.frame(
  infected = rep(NA, rounds),
  immunity = NA,
  recovering = NA
)


#### 2. Build the class room - dataframe$ rows, columns, ID
nrows <- 5 
ncols <- 6
seats <- expand.grid(rows=1:nrows, cols=1:ncols) 
seats$ID <- 1:nrow(seats)

#### 3. Make a dataframe that contains student information
health <- data.frame(
  ID = 1:25,
  infected_pre = 0,
  attendance = 0,
  location = 0,
  missed_rounds = 0, 
  past_affections = 0,
  p = 0,
  infection_post = 0,
  immunity = 0,
  sick_but_going = 0)

for(round in 1:rounds){
  #### 4. Randomly assign seats for attending students
  # 4a. get absent students
  ## making voluntary decision not to come..
  health$attendance <- sample(0:1, students, replace = TRUE, prob = c(1-random_absence, random_absence))
  ## cannot come anyway due to infection
  health$attendance[which(health$missed_rounds>=1)] <- 1
  
  # 4b. assign seats to attending students
  attending_students <- which(health$attendance==0)
  health$location[attending_students]=sample(1:nrow(seats), length(attending_students), replace = FALSE)
  
  
  ### 5. Calculate probability of infection for each students
  # 5a. get students(who are not immune)who got infected outside of class
  possible_hosts=which(health$immunity == 0)
  health$infected_pre[possible_hosts]=sample(0:1, length(possible_hosts), replace = TRUE, prob=c(1-initial_prob,initial_prob))
  
  # 5b. calculate probability of infection for each students
  #### temporary function and loop to test
  infected_seats=which(health$infected_pre==1)
  p_vector=c()
  get_infection_probability=function(beta, seat, infected_seats){
    if(seat-ncols %in% infected_seats){
      p_vector=c(p_vector, beta)
    }
    if(seat+ncols %in% infected_seats){
      p_vector=c(p_vector, beta)
    }
    # get p from p_vector
    product=1
    for(prob in p_vector){
      product=product*(1-prob)
    }
    p=1-product
    return(p)
  }
  for(student in attending_students){
    seat=health$location[student]
    health$p[student]=get_infection_probability(beta, seat, infected_seats)
  }
  #### function to calculate infection probability
  ## didn't touch except variable name - hw
  # infection_prob <- function() {
  #   individual_prob <- c()
  #   for (seat in health$location) {
  #     for (i in 1:dist)
  #       if (seat-(ncols + 1) %in% health$infected_pre) {
  #         individual_prob=c(individual_prob, prob_diagonal)
  #       }
  #     if (seat - ncols %in% health$infected_pre) {
  #       individual_prob=c(individual_prob, prob_diagonal)
  #     }
  #     if (seat - (ncol - 1) %in% health$infected_pre) {
  #       individual_prob=c(individual_prob, prob_diagonal)
  #     }
  #   }
  # }
  # 
  # 
  # temporary probability to test 
  # health$p[attending_students]<-sample(0:100, length(attending_students), replace = TRUE)/100
  
  
  ### 6. From the probabilities, determine which student get infected
  health$infected_post<-rep(0, nrow(health))
  for(i in 1:nrow(health)){
    if(health$infected_pre[i] == 1){
      health$infected_post[i] == 1
    }
    else{
      health$infected_post[i]<-probability_to_binary(health$p[i])
    }
    
    
  }
  
  ### 7. Loop multiple times and track infection status each round
  # 7a. update infection status for each round in meta sheet
  meta$infected[round] <- sum(health$infected_post)
  meta$immunity[round] <- sum(health$immunity)
  meta$recovering[round] <- length(which(health$missed_rounds>=1))
  
  #7b. update "health" dataframe
  quarantine <- which(health$sick_but_going==lectures_per_week-1)
  back_to_school <- which(health$missed_rounds==lectures_per_week-1)
  #clear infection status of students in quarantine
  health$infected_pre[quarantine] <- 0
  health$sick_but_going[quarantine] <- 0
  health$infected_post[quarantine] <-0
  #count missed rounds of students in quarantine
  health$missed_rounds[quarantine] <- health$missed_rounds[quarantine]+1
  #get students back to class + immunity
  health$missed_rounds[back_to_school] <- 0
  health$immunity[back_to_school] <- 1
  #add counts for students who got sick and still goes to school
  health$sick_but_going[which(health$infected_post==1)] <-health$sick_but_going[which(health$infected_post==1)] +1
  #transfer infected_post as infect_pre of next round
  health$infected_pre <- health$infected_post
  #clear other columns
  health$attendance <- 0
  health$location <- 0
  health$p <- 0
  health$infected_post <- 0
  
  
  print(back_to_school)
  print(meta)
  
}