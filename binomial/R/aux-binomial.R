#private auxilary functions for summary stats

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
