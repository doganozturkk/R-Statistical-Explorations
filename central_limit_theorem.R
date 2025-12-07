# central_limit_theorem.R
# Demonstrate the Central Limit Theorem by sampling means
library(tidyverse)

set.seed(123)

population <- rnorm(100000, mean = 10, sd = 5)

sample_sizes <- c(1, 2, 5, 30, 100)
results <- list()

for (s in sample_sizes) {
  sample_means <- replicate(1000, mean(sample(population, s)))
  results[[as.character(s)]] <- tibble(sample_size = s, mean = sample_means)
}

df <- bind_rows(results)

# summary
df %>% group_by(sample_size) %>% summarise(mean = mean(mean), sd = sd(mean), .groups = 'drop') %>% print()

# plot
p <- df %>% ggplot(aes(x = mean)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "cornflowerblue", alpha = 0.7) +
  geom_density(color = "darkgreen") +
  facet_wrap(~sample_size, scales = "free") +
  theme_minimal() +
  labs(title = "Central Limit Theorem: Distribution of sample means", x = "Sample mean", y = "Density")

print(p)
