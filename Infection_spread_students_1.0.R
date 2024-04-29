#######
### 1. Determine Parameters
### 2. Build a classroom and create "seat dataframe"
### 3. Make a dataframe of students, contains health status and attendance of all students
### 4. Randomly assign seats for each students
### 5. Calculate probability of infection for each students
### 6. From the probabilities, determine which student get infected
### 7. Loop multiple times and track infection status each round



#### set some variables
beta <- 0.3
students <- 25
transmission_dist <- 1 #in meters
random_absence <- 0.05
lectures <- 1 #per week
initial_prob=0.1

##### build the class room
nrows <- 5 
ncols <- 6
seats <- expand.grid(rows=1:nrows, cols=1:ncols) 
seats$ID <- 1:nrow(seats)
health$location <- sample(seats$ID, students, replace=FALSE)

#### make a dataframe - students
health <- data.frame(
  individual = 1:25,
  infection_status = 0,
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

# assign starting location with random abscenses
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

