# BASIC DATA UNDERSTANDING

install.packages("tidyverse")
library(tidyverse)
inputData <- read.csv("chicago_taxi.csv")
mean(inputData$tips)
median(inputData$tips)
summary(inputData$tips)
summary(inputData$tips)
var(inputData$tips)
var(inputData$seconds)
var(inputData$miles)
var(inputData$total)
var(inputData$speed)
install.packages("dplyr")
library(magrittr)
library(dplyr)

inputData %>%
  group_by(payment_type) %>%
  summarise(freq = n()) %>%
  arrange(freq)

# LINEAR REGRESSION

cat("FITTING LINEAR REGRESSION")

install.packages("e1071")

data <- read.csv("chicago_taxi.csv")
credit_data <- data[data$payment_type == "Credit", ]
variables_to_normalize <- credit_data[, c("seconds", "miles", "speed", "total", "tips")]

# Normalize the variables
normalized_credit_data <- as.data.frame(scale(variables_to_normalize))

linear_model <- lm(tips ~ seconds + miles + speed + total, data=normalized_credit_data)
summary(linear_model)
summary(data) # For median and mean

# Now without the total
linear_model <- lm(tips ~ seconds + miles + speed, data=normalized_credit_data)
summary(linear_model)

# Filter out non-numeric columns
numeric_data <- data[, sapply(data, is.numeric)]

# Calculate variance for numeric columns
sapply(numeric_data, var)
# Calculate variance for numeric columns while ignoring NA values
sapply(numeric_data, function(x) var(x, na.rm = TRUE))

library(e1071)
sapply(numeric_data, skewness) # For skewness
aggregate(tips ~ daytype, data=data, mean)

# BASIC CORRELATION MATRIX

cat("BASIC CORRELATION MATRIX")

cor_matrix <- cor(data[c("seconds", "miles", "speed", "tips")])

print(cor_matrix)

# We observe high correlation between Speed and Miles, we will attempt to omit miles in our LR model

# REDUCED LINEAR REGRESSION MODEL

cat("REDUCED LINEAR REGRESSION MODEL")

reduced_linear_model <- lm(tips ~ seconds + miles + speed, data=data)
summary(reduced_linear_model)
summary(data) # For median and mean

# VISUALIZING DATA

cat("GENERATING DATA VISUALIZATIONS")

# Visualizing miles variable
filtered_data <- data[data$miles < 25, ]
hist(filtered_data$miles, breaks=25)

# Visualizing speed variable
filtered_data <- data[data$speed < 60, ]
hist(filtered_data$speed, breaks=25)

# Visualizing tips variable
filtered_data <- data[data$tips < 15, ]
hist(filtered_data$tips, breaks=25)

plot(data$speed, data$tips, main = "Scatter Plot of Speed vs Tips", 
     xlab = "Speed", ylab = "Tips", pch = 16, col = "blue",
     xlim = c(0, 75), ylim = c(0, 20))

# MONTE CARLO DISTRIBUTION

# # Calculate mean and standard deviation for each variable
# # Parameters for Monte Carlo Simulation
# mean_of_speed <- mean(data$speed)
# sd_of_speed <- sd(data$speed)
# 
# mean_of_miles <- mean(data$miles)
# sd_of_miles <- sd(data$miles)
# 
# mean_of_seconds <- mean(data$seconds)
# sd_of_seconds <- sd(data$seconds)
# 
# mean_of_tips <- mean(data$tips)
# sd_of_tips <- sd(data$tips)
# 
# # Set the seed for reproducibility
# set.seed(123)
# 
# # Number of samples
# n_samples <- 10000
# 
# # Generate samples for each variable
# speed_samples <- abs(rnorm(n_samples, mean = mean_of_speed, sd = sd_of_speed))
# miles_samples <- abs(rnorm(n_samples, mean = mean_of_miles, sd = sd_of_miles))
# seconds_samples <- abs(rnorm(n_samples, mean = mean_of_seconds, sd = sd_of_seconds))
# tips_samples <- abs(rnorm(n_samples, mean = mean_of_tips, sd = sd_of_tips))
# 
# # Combine the samples into a data frame
# monte_carlo_data <- data.frame(speed = speed_samples,
#                                miles = miles_samples,
#                                seconds = seconds_samples,
#                                tips = tips_samples)
# 
# # View the first few rows of the generated data
# monte_carlo_data
# 
# # Linear Regresion using the Monte Carlo Data
# mc_linear_model <- lm(tips ~ seconds + miles + speed, data=monte_carlo_data)
# summary(mc_linear_model)

# START ANALYSIS ON CASH VS CREDIT

cat("CASH VS CREDIT ANALYSIS")

# Read Excel file
inputData <- data
# Explore the data
glimpse(inputData)
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

# Load necessary libraries
library(tidyverse)
library(dplyr)

# Analyze tips based on payment_type
summary_stats <- inputData %>%
  group_by(payment_type) %>%
  summarize(
    mean_tip = mean(tips),
    median_tip = median(tips),
    max_tip = max(tips),
    min_tip = min(tips)
  )

# Display the summary statistics
print(summary_stats)

# CONSULTATION

cat("SPEED TO TIPS ANALYSIS")

# Which speeds result in the highest tips?

# Create speed intervals, e.g., 0-10, 10-20, ..., 70-80
speed_intervals <- seq(0, 80, by = 5)
# Use tapply to calculate average tip for each speed interval
average_tips <- tapply(data$tips, cut(data$speed, breaks = speed_intervals, include.lowest = TRUE), mean)
# Create a bar plot
barplot(average_tips, names.arg = levels(cut(data$speed, breaks = speed_intervals, include.lowest = TRUE)),
        main = "Average Tips for Speed Intervals", xlab = "Speed Intervals", ylab = "Average Tips")

# TESTING CONFIDENCE INTERVAL BETWEEN CASH AND CREDIT

cat("CASH VS CREDIT HYPOTHESIS TESTING")

cash_tips <- data$tips[data$payment_type == "Cash"]
credit_tips <- data$tips[data$payment_type == "Credit"]

hist(cash_tips, main="Histogram of Cash Tips", xlab="Tips", breaks=50)
hist(credit_tips, main="Histogram of Credit Tips", xlab="Tips", breaks=50)

qqnorm(cash_tips); qqline(cash_tips)
qqnorm(credit_tips); qqline(credit_tips)

t_test_result <- t.test(cash_tips, credit_tips, var.equal = FALSE)

t_test_result$p.value

cat("Null Hypothesis: Cash and Credit tips are the same.")
cat("Alt. Hypothesis: Cash and Credit tips are different")

confidence_level = 0.01

if (t_test_result$p.value < confidence_level/2) {
  cat("Reject the null hypothesis: There is significant evidence of a difference.\n")
} else {
  cat("Fail to reject the null hypothesis: There is not enough evidence of a difference.\n")
}

# TESTING CONFIDENCE INTERVAL BETWEEN WEEKDAY AND WEEKEND

cat("WEEKEND VS WEEKDAY HYPOTHESIS TESTING")

weekend_tips <- credit_data$tips[credit_data$daytype == "weekday"]
weekday_tips <- credit_data$tips[credit_data$daytype == "weekend"]

hist(weekend_tips, main="Histogram of Weekend Tips", xlab="Tips", breaks=50)
hist(weekday_tips, main="Histogram of Weekday Tips", xlab="Tips", breaks=50)

qqnorm(weekend_tips); qqline(weekend_tips)
qqnorm(weekday_tips); qqline(weekday_tips)

t_test_result <- t.test(weekend_tips, weekday_tips, var.equal = FALSE)

cat("P-Value = ", t_test_result$p.value)

cat("Null Hypothesis: Weekend and Weekday tips are the same.")
cat("Alt. Hypothesis: Weekend and Weekday tips are different")

confidence_level = 0.01

if (t_test_result$p.value < confidence_level/2) {
  cat("Reject the null hypothesis: There is significant evidence of a difference.\n")
} else {
  cat("Fail to reject the null hypothesis: There is not enough evidence of a difference.\n")
}
summary(weekend_tips)
summary(weekday_tips)

# ANOVA FOR CASH VS CREDIT

cat("CASH VS CREDIT ANOVA")

model <- aov(tips ~ payment_type, data = data)
summary(model)

# FINDING THE MOST COMMON TIP % OF THE FARE

cat("TIP % ANALYSIS")

credit_data$fare <- credit_data$total - credit_data$tips
credit_data$percent <- credit_data$tips / credit_data$fare
credit_data$percent <- credit_data$percent * 100

filtered_credit_data <- credit_data[credit_data$percent < 100, ]

# Create a histogram with automatically determined breaks
hist(filtered_credit_data$percent, breaks = seq(0, 100, by = 5), main = "Histogram of filtered_credit_data$percent", xlab = "Percentage", ylab = "Frequency")

# MONTE CARLO

cat("MONTE CARLO SIMULATION")

# Build the parametric function from independent variables to tips
mc_linear_model <- lm(tips ~ speed + seconds + miles, data=credit_data)

# Generate random input data
num_credit_data <- credit_data[, c('speed', 'seconds')]
means <- colMeans(num_credit_data, na.rm=TRUE)
stdevs <- apply(num_credit_data, 2, sd, na.rm=TRUE)

num_simulations <- 1000
r_inputs <- matrix(
  abs(rnorm(length(means)*num_simulations, mean = rep(means, each = num_simulations), sd = rep(stdevs, each = num_simulations))), ncol=length(means)
)

r_inputs_df <- as.data.frame(r_inputs)
colnames(r_inputs_df) <- colnames(num_credit_data)
r_inputs_df$miles <- (r_inputs_df$seconds * r_inputs_df$speed) / 3600

mc_predictions <- predict(mc_linear_model, newdata = r_inputs_df)

hist(mc_predictions)

credit_data_hist <- credit_data[credit_data$tips < 20, ]

monte_carlo_tips <- mc_predictions
hist(monte_carlo_tips, xlim=c(0,20), breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20))
hist(credit_data_hist$tips, xlim=c(0,20), breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20))

# BAYES THEOREM

cat("BAYES THEOREM ANALYSIS")

library(dplyr)
chicago_taxi$total <- round(chicago_taxi$total, 0)
chicago_taxi$tips <- round(chicago_taxi$tips, 0)
tips_threshold <- 5
prior_total_high <- sum(chicago_taxi$total > 50) / nrow(chicago_taxi)
prior_total_low <- 1 - prior_total_high
prior_tips_low <- sum(chicago_taxi$tips <= tips_threshold) / nrow(chicago_taxi)
prior_tips_high <- 1 - prior_tips_low
print(prior_total_high)
likelihood_tips_high_given_total_high <- sum(chicago_taxi$tips > tips_threshold & chicago_taxi$total > 50) / sum(chicago_taxi$total > 50)
print(likelihood_tips_high_given_total_high)
likelihood_tips_high_given_total_low <- sum(chicago_taxi$tips > tips_threshold & chicago_taxi$total <= 50) / sum(chicago_taxi$total <= 50)
print(likelihood_tips_high_given_total_low)
posterior_tips_high_given_total <- (likelihood_tips_high_given_total_high * prior_tips_high) / denominator
cat("Posterior Probability of Higher Tips given Total > 50:", posterior_tips_high_given_total, "\n")
