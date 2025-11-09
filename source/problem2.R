
#' Simulation of One sample t-test 
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


#' Run Many Simulations 
#'
#' @param mu_vals 
#' @param reps 
#' @param n 
#' @param sigma 
#'
#' @returns
#' @export
#'
#' @examples
many_ttest_sims <- function(mu_vals, reps = 5000, n = 30, sigma = 5) {
  tidyr::expand_grid(
    mu_true = mu_vals,
    iter    = seq_len(reps)) |>
    dplyr::mutate(
      res = purrr::map(mu_true, ~ sim_ttest(mu = .x, n = n, sigma = sigma))) |>
    tidyr::unnest(res)
}
