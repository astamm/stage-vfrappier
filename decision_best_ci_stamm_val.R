library(ggplot2)
library(tidyverse)



ci_minimal_length <- function(w, n = 100, s = 2, alpha = 0.05) {
  qsup <- qchisq(1-w*alpha, df = n - 1)
  qinf <- qchisq((1-w)*alpha, df = n - 1)
  s * sqrt(n-1) * (sqrt(1/qinf) - sqrt(1/qsup))
}

ci_symmetric <- function(w, n = 100, s = 2, alpha = 0.05) {
  qsup <- qchisq(1-w*alpha, df = n - 1)
  qinf <- qchisq((1-w)*alpha, df = n - 1)
  (s * sqrt((n-1)/2)/qinf)- sqrt((n-1)/2)*(sqrt(1/qinf) - sqrt(1/qsup))-(s * sqrt((n-1)/2)/qsup)}

ci_centered_symmetric <- function(w, n = 100, s = 2, alpha = 0.05) {
  qsup <- qchisq(1-w*alpha, df = n - 1)
  qinf <- qchisq((1-w)*alpha, df = n - 1)
  s * (sqrt(n-1) * (sqrt(1/qinf) + sqrt(1/qsup)) - 2)
}

ci_centered <- function(w, n = 100, s = 2, alpha = 0.05) {
  qsup <- qchisq(1-w*alpha, df = n - 1)
  qinf <- qchisq((1-w)*alpha, df = n - 1)
  sqrt((n-1)/2) * (sqrt(1/qinf) + sqrt(1/qsup)) - 1
}

eps0 <- .Machine$double.eps
df <- crossing(
  n = 2^(4:14),
  alpha = seq(0.01, 0.1, by = 0.01)
) |> 
  mutate(
    minimal_length = map2_dbl(n, alpha, ~ optimise(
      f = ci_minimal_length,
      interval = c(eps0, 1-eps0),
      n = .x,
      alpha = .y
    )$minimum),
    centered_symmetric = map2_dbl(n, alpha, ~ uniroot(
      f = ci_centered_symmetric,
      interval = c(eps0, 1-eps0),
      n = .x,
      alpha = .y
    )$root),
    centered = map2_dbl(n,alpha, ~ uniroot(
      f= ci_centered
      interval = c(eps0, 1-eps0),
      n = .x,
      alpha = .y
    )$root),
    symetric = map2_dbl(n,alpha,~ uniroot(
      f=ci_symetric
      interval = c(eps0,1-eps0)
      n=.x
      alpha=.y
    )$root)
  )

df |> 
  mutate(alpha = as_factor(alpha)) |> 
  pivot_longer(-c(n, alpha)) |> 
  ggplot(aes(n, value, color = name)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  facet_wrap(vars(alpha), nrow = 2) +
  scale_x_log10() +
  theme_bw() +
  labs(
    title = "Impact of asymmetric weighting of the chi-square tails for CI on variance",
    x = "Sample size",
    y = "Weighting of the right tail",
    color = "Optimizing strategy"
  ) +
  theme(legend.position = "top")

 