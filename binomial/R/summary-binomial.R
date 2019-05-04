#functions for summary stats


#' @title binomial variable
#' @description function used to generate summary stats for the binomial probability
#' @param trials number of trials (non-negative integer)
#' @param prob probability of success (real number between 0 and 1)
#' @return generates a "binvar" class object that is a list of trials and prob
#' summary will generate
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



