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
### 2. Build a classroom and create "seat dataframe"
### 3. Make a dataframe of students, contains health status and attendance of all students
### 4. Randomly assign seats for attending students
#### a. get attendance status for each students
#### b. assign seats to attending students
### 5. Calculate probability of infection for each students
### 6. From the probabilities, determine which student get infected
### 7. Loop multiple times and track infection status each round

#### 0. Retrieve functions
source("get_transmissable_distance.R")
source("probability_to_binary.R")

#### 1. Determine Parameters
beta <- 0.3
students <- 25
transmission_dist <- 1 #in meters
random_absence <- 0.05
lectures <- 1 #per week
initial_prob=0.1

#### 2. Build the class room - dataframe$ rows, columns, ID
nrows <- 5 
ncols <- 6
seats <- expand.grid(rows=1:nrows, cols=1:ncols) 
seats$ID <- 1:nrow(seats)
health$location <- sample(seats$ID, students, replace=FALSE)

#### 3. Moniter student status - dataframe$ ID, infectionstatus, attendance, location, missed days, past affections, immunity
health <- data.frame(
  ID = 1:25,
  infection_status = 0,
  attendance = 0,
  location = 0,
  missed_days = 0, 
  past_affections = 0, 
  immunity = 0)


# assign students to seats
health$row <- seats$rows[health$location]
health$col <- seats$cols[health$location]


# calculate the starting number of infected students 
exp_infection_at_start <- round(students * beta)

# assign 1 to infection_status column for infected students
health$infection_status[sample(1:students, exp_infection_at_start)] <- 1

#### Can we combine above two lines as health$infection_status=sample(0:1, students, replace = TRUE, prob=c(1-initial_prob,initial_prob))?

#### 4. Randomly assign seats for attending students
# 4a. get absent students
## making voluntary decision not to come..
health$attendance <- sample(0:1, students, replace = TRUE, prob = c(1-random_absence, random_absence))
## cannot come anyway due to infection
health$attendance[which(health$missed_days>1)] <- 1

# 4b. assign seats to attending students
attending_students <- which(health$attendance==0)
health$location[attending_students]=sample(1:nrow(seats), length(attending_students), replace = FALSE)

health$location[sample(1:students, round(students * random_absence))] <- 1

### 5. Calculate probability of infection for each students

#### function to calculate infection probability
## didn't touch except variable name 
infection_prob <- function() {
  individual_prob <- c()
  for (seat in health$location) {
    for (i in 1:dist)
    if (seat-(ncols + 1) %in% health$infection_status) {
      individual_prob=c(individual_prob, prob_diagonal)
    }
    if (seat - ncols %in% health$infection_status) {
      individual_prob=c(individual_prob, prob_diagonal)
    }
    if (seat - (ncol - 1) %in% health$infection_status) {
      individual_prob=c(individual_prob, prob_diagonal)
    }
  }
}


# temporary probability to test 
health$p[attending_students]=sample(0:100, length(attending_students), replace = TRUE)/100


### 6. From the probabilities, determine which student get infected
health$infected=rep(NA, nrow(health))
for(i in 1:nrow(health)){
  health$infected[i]=probability_to_binary(health$p[i])
  
}


