# outlier_effects.R
# Show how outliers affect mean and median, and influence regression
library(tidyverse)

set.seed(7)

n <- 200
x <- rnorm(n, 0, 1)
y <- 3 * x + rnorm(n, 0, 1)

df <- tibble(x = x, y = y)

# add outliers
df_out <- bind_rows(df, tibble(x = c(10, 12), y = c(-30, 40)))  # extreme points

# compute statistics
stats_clean <- df %>% summarise(mean_y = mean(y), median_y = median(y))
stats_out <- df_out %>% summarise(mean_y = mean(y), median_y = median(y))
print(list(clean = stats_clean, with_outliers = stats_out))

# regression impact
lm_clean <- lm(y ~ x, data = df)
lm_out <- lm(y ~ x, data = df_out)
print(summary(lm_clean))
print(summary(lm_out))

# plot
p <- df_out %>% ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  theme_minimal() +
  labs(title = 'Effect of outliers on regression and central tendency')

print(p)
