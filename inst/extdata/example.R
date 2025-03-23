# Example R script for tldrAI package
# This script demonstrates various R programming patterns

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Define a helper function
calculate_stats <- function(data, col_name) {
  # Calculate basic statistics on a column
  stats <- data %>%
    summarize(
      mean = mean({{ col_name }}, na.rm = TRUE),
      median = median({{ col_name }}, na.rm = TRUE),
      sd = sd({{ col_name }}, na.rm = TRUE),
      min = min({{ col_name }}, na.rm = TRUE),
      max = max({{ col_name }}, na.rm = TRUE),
      n = n(),
      missing = sum(is.na({{ col_name }}))
    )
  
  return(stats)
}

# Create some example data
set.seed(123)
example_data <- data.frame(
  id = 1:100,
  group = sample(c("A", "B", "C"), 100, replace = TRUE),
  value1 = rnorm(100, mean = 10, sd = 2),
  value2 = rnorm(100, mean = 20, sd = 5)
)

# Add some missing values
example_data$value1[sample(1:100, 10)] <- NA
example_data$value2[sample(1:100, 15)] <- NA

# Process the data
processed_data <- example_data %>%
  group_by(group) %>%
  mutate(
    value1_z = scale(value1),
    value2_z = scale(value2)
  ) %>%
  filter(!is.na(value1) & !is.na(value2)) %>%
  arrange(group, desc(value1))

# Calculate statistics by group
group_stats <- processed_data %>%
  group_by(group) %>%
  summarize(
    value1_mean = mean(value1, na.rm = TRUE),
    value1_sd = sd(value1, na.rm = TRUE),
    value2_mean = mean(value2, na.rm = TRUE),
    value2_sd = sd(value2, na.rm = TRUE),
    n = n()
  )

# Create a visualization
plot1 <- ggplot(group_stats, aes(x = group, y = value1_mean, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = value1_mean - value1_sd, ymax = value1_mean + value1_sd), width = 0.2) +
  theme_minimal() +
  labs(
    title = "Mean Value1 by Group",
    x = "Group",
    y = "Mean Value1",
    caption = "Error bars represent standard deviation"
  )

# Create a second visualization
plot2 <- ggplot(processed_data, aes(x = value1, y = value2, color = group)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ group) +
  theme_minimal() +
  labs(
    title = "Relationship between Value1 and Value2",
    x = "Value1",
    y = "Value2"
  )

# Save the results
# write.csv(group_stats, "group_stats.csv", row.names = FALSE)
# ggsave("plot1.png", plot1, width = 8, height = 6)
# ggsave("plot2.png", plot2, width = 10, height = 6)

# Print a summary of the results
cat("Analysis complete.\n")
cat("Processed", nrow(processed_data), "rows of data.\n")
cat("Found", nrow(group_stats), "groups.\n")
cat("Summary statistics:\n")
print(group_stats)