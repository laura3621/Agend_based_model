# set some variables
beta <- 0.3
students <- 25
transmission_dist <- 2 #in meters
hall <- matrix(0, nrow = 6, ncol = 5)
random_absence <- 0.05
lectures <- 1 #per week

# make a dataframe
health <- data.frame(
  individual = 1:25,
  infection_status = rep(0, students),
  location = rep(0, students),
  missed_days = 0, 
  past_affections = 0, 
  immunity = 0)

# calculate the starting number of infected students 
exp_infection_at_start <- round(students * beta)

# assign 1 to infection_status column for infected students
health$infection_status[sample(1:students, exp_infection_at_start)] <- 1

# assign starting location with random abscenses
health$location[sample(1:students, round(students * random_absence))] <- 1



