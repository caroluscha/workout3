#private checker functions for inputs

#this function checks whether prob is between 0 and 1
check_prob <- function(prob) {
  if (prob > 0 & prob < 1 & length(prob) == 1) {
    return(TRUE)
  }
  else {
    stop("p has to be a number betwen 0 and 1")
  }
}


#checks if trials (number of trials) is a non-negative integer
check_trials <- function(trials) {
  if (trials > 0 & trials%%1 ==0) {
    return(TRUE)
  }
  else {
    stop("trials must be a non-negative integer")
  }
}

#checks whether success is a non-negative integer and less than or equal to trials
check_success <- function(success, trials) {
  if (success >= 0 & success%%1 == 0 & success <= trials) {
    return(TRUE)
  }
  else{
    stop("invalid success value")
  }
}
