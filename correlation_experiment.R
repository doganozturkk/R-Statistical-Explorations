# correlation_experiment.R
# Explore correlation and effect of noise
library(tidyverse)

set.seed(2025)

n <- 500
x <- runif(n, -5, 5)
y_strong <- 2 * x + rnorm(n, sd = 0.5)   # strong linear relationship
y_weak <- 0.5 * x + rnorm(n, sd = 2)     # weak linear relationship with more noise
y_nonlinear <- x^2 + rnorm(n, sd = 4)    # nonlinear

df <- tibble(x = x, y_strong = y_strong, y_weak = y_weak, y_nonlinear = y_nonlinear) %>% pivot_longer(-x, names_to = "scenario", values_to = "y")

# compute correlations
corrs <- df %>% group_by(scenario) %>% summarise(correlation = cor(x, y), .groups = 'drop')
print(corrs)

# plot
p <- df %>% ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = 'lm', se = FALSE, color = "red") +
  facet_wrap(~scenario, scales = "free") +
  theme_minimal() +
  labs(title = "Correlation experiments: noise and nonlinearity", x = "x", y = "y")

print(p)
