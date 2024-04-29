#######
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
#### a. students getting an infection without contact
#### b. get attendance: students in their recovery period + random absence
#### c. calculate probability of infection for each students
### 6. From the probabilities, determine which student get infected
### 7. Loop multiple times and track infection status each round



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

#### Can we combine line 52~56 as health$infection_status=sample(0:1, students, replace = TRUE, prob=c(1-initial_prob,initial_prob))?

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


#### function to calculate infection probability
infection_prob <- function() {
  individual_prop <- c()
  for (seat in health$location) {
    for (i in 1:dist)
    if (seat-(ncols + 1) %in% health$infection_status) {
      infection_prob=c(infection_prob, prob_diagonal)
    }
    if (seat - ncols %in% health$infection_status) {
      infection_prob=c(infection_prob, prob_diagonal)
    }
    if (seat - (ncol - 1) %in% health$infection_status) {
      infection_prob=c(infection_prob, prob_diagonal)
    }
  }
}

