
#' Problem 2 
#'
#' @param mu true mean
#' @param n sample size
#' @param sigma standard deviation
#'
#' @returns
#' @export
#'
#' @examples
sim_ttest <- function(mu, n = 30, sigma = 5) {
  x  <- rnorm(n, mean = mu, sd = sigma)
  tt <- t.test(x, mu = 0)
  tibble(
    mu_true = mu,
    mu_hat  = mean(x),
    p_value = broom::tidy(tt)$p.value,
    reject  = p_value < 0.05
  )
}