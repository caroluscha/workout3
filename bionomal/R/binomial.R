# code for the binomial package





#this function checks whether prob is between 0 and 1
check_prob <- function(prob) {
  if (prob > 0 & prob < 1) {
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


#calculates binomal mean given trials and prob
aux_mean <- function(trials, prob) {
  x <- trials * prob
  x
}


#calculates binomal variance given trials and prob
aux_variance <- function(trials, prob) {
  x <- trials * prob * (1 - prob)
  x
}

#calculates binomal mode given trials and prob
aux_mode <- function(trials, prob) {
  m <- round(trials * prob + prob)
  if (m%%1==0 & trials%%2 != 0) {
    return(c(m, m-1))
  }
  else {
  m
  }
}

#calculates binomal skewness given trials and prob
aux_skewness <- function(trials, prob) {
  skew <- (1 - 2 * prob) / sqrt(trials * prob * (1 - prob))
  skew
}


#calculates binomal kurtosis given trials and prob
aux_kurtosis <- function(trials, prob) {
  kurt <- (1 - 6 * prob * (1 - prob)) / (trials * prob * (1 - prob))
  kurt
}


#' @title binomial choose
#' @description alculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials (non-negative integer)
#' @param k number of successes (non-negative integer less than trials)
#' @return combinations of trials and success given n and k
#' @examples bin_choose(n = 5, k = 2), bin_choose(5, 0), bin_choose(5, 1:3)
#' @export
bin_choose <- function(n, k){
  if (n >= k) {
    bchoose <- factorial(n) / (factorial(k) * factorial(n - k))
    return (bchoose)
  }
  else {
    stop("k cannot be greater than n")
  }
}

#' @title binomial probability
#' @description calculates binomial probability given success, trials, and probability of success
#' @param success number of successes given number of trials (non-negative integer less than trials)
#' @param trials number of trials (non-negative integer)
#' @param prob probability of success (real number between 0 and 1)
#' @return the binomal probability (real number between 0 and 1)
#' @examples bin_probability(success = 2, trials = 5, prob = 0.5), bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' @export
bin_probability <- function(success, trials, prob) {
  if (check_success(success, trials) != TRUE){
    return (check_success(success, trials))
  }
    else if (check_trials(trials) != TRUE) {
      return (check_trials(trials))
    }
    else if (check_prob(prob) != TRUE) {
      return (check_prob(prob))
    }
  else {
    bprob <- bin_choose(trials, success) *
      prob ^ success *
      (1 - prob) ^ (trials - success)
    bprob
  }
}

#' @title binomal distribution
#' @description calculates the binomial distribution given number of trials and probability of success
#' @param trials number of trials (non-negative integer)
#' @param prob probability of success (real number between 0 and 1)
#' @return data frame with binomial distribution with class "bindis"
#' @example bin_distribution(trials = 5, prob = 0.5)
#' @export
bin_distribution <- function (trials, prob) {
  success = c(0:trials)
  bprob_series <- vector("double")
  for (i in success) {
    bprob <- bin_probability(i, trials, prob)
    bprob_series[i+1] <- bprob
  }
  bindis <- data.frame("success" = success, "probability" = bprob_series)
  class(bindis) <- c("bindis", "data.frame")
  bindis
}

#' @export
plot.bindis <- function(bindis) {
  library(ggplot2)
  ggplot(data = bindis, mapping = aes(success, probability)) +
  geom_col()
}



#' @title binomial cumulative distribution
#'
#'
#' @export
bin_cumulative <- function(trials, prob) {
  bindis <- bin_distribution(trials, prob)
  bincum_series <- vector("double")
  bincum_series[1] <- bindis$probability[1]
  success = c(0:trials)
  for (i in 1:(length(success)-1)) {
    bcum <- bincum_series[i] + bindis$probability[i+1]
    bincum_series[i+1] <- bcum
  }
  bincum <- data.frame("success" = bindis$success,
                       "probability" = bindis$probability,
                       "cumulative" = bincum_series)
  class(bincum) <- c("bincum", "data.frame")
  bincum
}
#' @export
plot.bindis <- function(bincum) {
  library(ggplot2)
  ggplot(data = bincum, mapping = aes(success, cumulative)) +
  geom_line()
}

#' @title binomial variable
#'
#'
#' @export
bin_variable <- function(trials, prob) {
  if (check_trials(trials) != TRUE) {
    return (check_trials(trials))
  }
  else if (check_prob(prob) != TRUE) {
    return (check_prob(prob))
  }
  else {
    binvar <- list ("trials" =  trials, "prob" = prob)
    class(binvar) <- "binvar"
    binvar
  }
}
#' @export
print.binvar <- function (binvar) {
  cat("Binomial variable\n\nParameters\n",
                 "- number of trials\t:", binvar$trials, "\n",
                 "- prob of success\t:", binvar$prob, "\n")
}


#' @export
summary.binvar <- function(binvar) {
  trials <- binvar$trials
  prob <- binvar$prob
  bmean <- aux_mean(trials, prob)
  bvariance <- aux_variance(trials, prob)
  bmode <- aux_mode(trials, prob)
  bskewness <- aux_skewness(trials, prob)
  bkurtosis <- aux_kurtosis(trials, prob)
  sum_binvar <- list("trials" = trials, "prob" = prob,
       "mean" = bmean, "variance" = bvariance,
       "mode" = bmode, "skewness" =bskewness,
       "kurtosis" = bkurtosis)
  class(sum_binvar) <- "summary.binvar"
  sum_binvar
}

#' @export
print.summary.binvar <- function(sum_binvar) {
  cat("Binomial variable\n\nParameters\n",
      "- number of trials\t:", sum_binvar$trials, "\n",
      "- prob of success\t:", sum_binvar$prob, "\n", "\n")
  cat("Measures", "\n")
  cat("- mean\t\t:", sum_binvar$mean, "\n")
  cat("- variance\t:", sum_binvar$variance, "\n")
  cat("- mode\t\t:", sum_binvar$mode, "\n")
  cat("- skewness\t:", sum_binvar$skewness, "\n")
  cat("- kurtosis\t:", sum_binvar$kurtosis, "\n")
}


#' @export
bin_mean <- function(trials, prob) {
  if (check_trials(trials) != TRUE) {
    return (check_trials(trials))
  }
  else if (check_prob(prob) != TRUE) {
    return (check_prob(prob))
  }
  else {
  aux_mean(trials, prob)
  }
}

#'@export
bin_variance <- function(trials, prob) {
  if (check_trials(trials) != TRUE) {
    return (check_trials(trials))
  }
  else if (check_prob(prob) != TRUE) {
    return (check_prob(prob))
  }
  else {
    aux_variance(trials, prob)
  }
}

#'@export
bin_mode <- function(trials, prob) {
  if (check_trials(trials) != TRUE) {
    return (check_trials(trials))
  }
  else if (check_prob(prob) != TRUE) {
    return (check_prob(prob))
  }
  else {
    aux_mode(trials, prob)
  }
}

#'@export
bin_skewness <- function(trials, prob) {
  if (check_trials(trials) != TRUE) {
    return (check_trials(trials))
  }
  else if (check_prob(prob) != TRUE) {
    return (check_prob(prob))
  }
  else {
    aux_skewness(trials, prob)
  }
}

#'@export
bin_kurtosis <- function(trials, prob) {
  if (check_trials(trials) != TRUE) {
    return (check_trials(trials))
  }
  else if (check_prob(prob) != TRUE) {
    return (check_prob(prob))
  }
  else {
    aux_kurtosis(trials, prob)
  }
}



