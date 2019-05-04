#functions for main binomial functions




#' @title binomial choose
#' @description alculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials (non-negative integer)
#' @param k number of successes (non-negative integer less than trials)
#' @return combinations of trials and success given n and k
#' @examples bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
#' @export
bin_choose <- function(n, k){
  if (n >= k & n > 0 & k >= 0) {
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
#' @return a "bindis" class object with "data.frame" secondary class.
#' data frame has two columns success and probability
#' function includes plot function to plot histogram of success and probability
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
  ggplot(data = bindis, mapping = aes(success, probability)) +
  geom_col()
}









#' @title binomial cumulative distribution
#' @description function calculates the cumulative
#' @param trials number of trials (non-negative integer)
#' @param prob probability of success (real number between 0 and 1)
#' @return a "bincum" class object with "data.frame" secondary class.
#' data frame has three columns success, probability, cumulative
#' function also includes and plot function to plot a line plot of success and cumulative
#' @examples
#' dis2 <- bin_cumulative(trials = 5, prob = 0.5)
#' plot(dis2)
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
plot.bincum <- function(bincum) {

  ggplot(data = bincum, mapping = aes(success, cumulative)) +
  geom_line()
}



