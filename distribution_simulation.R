# distribution_simulation.R
# Simulate different distributions and plot histograms/densities using tidyverse
library(tidyverse)

set.seed(42)

n <- 10000

df <- tibble(
  normal = rnorm(n, mean = 0, sd = 1),
  uniform = runif(n, min = -2, max = 2),
  exponential = rexp(n, rate = 1)
) %>% pivot_longer(everything(), names_to = "dist", values_to = "value")

# summary
df %>% group_by(dist) %>% summarise(mean = mean(value), sd = sd(value), .groups = 'drop') %>% print()

# plots (will be rendered if run interactively)
p <- df %>% ggplot(aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 60, fill = "steelblue", alpha = 0.6) +
  geom_density(color = "darkred", size = 1) +
  facet_wrap(~dist, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution simulation: Normal / Uniform / Exponential", x = "Value", y = "Density")

print(p)
