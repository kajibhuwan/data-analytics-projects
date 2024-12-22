#title: "R_Assignment"
#author: "Bhuwan Basnet"
#date: "2024-08-13"
#output: pdf_document


#Setting the path variable
setwd("/Users/Lenovo/Desktop/R_Tech3100")

# Loading necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

#Loading Data set 

data <- read.csv("mtcars.csv")
show_col_types = FALSE
car_data <- data
summary(car_data)

# Check for missing values
missing_values <- sum(is.na(car_data))
print(paste("Number of missing values:", missing_values))

#Display the head of data-set
head(car_data)

#Check the structure of data-set
str(car_data)

# Calculation of Basic Summary Statistics

# Calculation of Mean
mean_mpg <- mean(car_data$mpg, na.rm = TRUE)
mean_hp <- mean(car_data$hp, na.rm = TRUE)

# Calculation of Median
median_mpg <- median(car_data$mpg, na.rm = TRUE)
median_hp <- median(car_data$hp, na.rm = TRUE)

#Summary of the given data-set
summary(car_data)

#This is the key numeric variables for mean, median and variance.
descriptive_stats <- car_data %>%
  summarise(
    mean_mpg = mean(mpg),
    median_mpg = median(mpg),
    var_mpg = var(mpg),
    mean_hp = mean(hp),
    median_hp = median(hp),
    var_hp = var(hp)
  )

print(descriptive_stats)


# Mode calculation since R doesn't have a built-in mode function
mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_mpg <- mode_function(car_data$mpg)
mode_hp <- mode_function(car_data$hp)

cat("Mode of MPG:", mode_mpg, "\n")
cat("Mode of HP:", mode_hp, "\n")

# Standard deviation, quartiles, and IQR

sd_mpg <- sd(car_data$mpg)
quartiles_mpg <- quantile(car_data$mpg)
iqr_mpg <- IQR(car_data$mpg)

sd_hp <- sd(car_data$hp)
quartiles_hp <- quantile(car_data$hp)
iqr_hp <- IQR(car_data$hp)

cat("Standard Deviation of MPG:", sd_mpg, "\n")
cat("Quartiles of MPG:", quartiles_mpg, "\n")
cat("IQR of MPG:", iqr_mpg, "\n")

cat("Standard Deviation of HP:", sd_hp, "\n")
cat("Quartiles of HP:", quartiles_hp, "\n")
cat("IQR of HP:", iqr_hp, "\n")

#Data Visualization of MPG and HP

# Histogram of MPG
ggplot(car_data, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Miles Per Gallon (MPG)", x = "MPG", y = "Frequency")

# Histogram of HP
ggplot(car_data, aes(x = hp)) +
  geom_histogram(binwidth = 20, fill = "green", alpha = 0.7) +
  labs(title = "Histogram of Horsepower (HP)", x = "HP", y = "Frequency")

# Boxplot of MPG by number of cylinders
ggplot(car_data, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Boxplot of MPG by Number of Cylinders", x = "Number of Cylinders", y = "MPG")

# Scatter plot of MPG vs. HP
ggplot(car_data, aes(x = hp, y = mpg)) +
  geom_point(color = "red", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Scatter Plot of MPG vs. HP", x = "Horsepower (HP)", y = "Miles Per Gallon (MPG)")


# Check the structure of the data frame
str(car_data)

# Select only numeric columns
numeric_data <- car_data[, sapply(car_data, is.numeric)]

# Correlation matrix
correlation_matrix <- cor(numeric_data)
print(correlation_matrix)

# Visualization of the correlation matrix
library(corrplot)
corrplot(correlation_matrix, method = "circle")


# ------------------ * Descriptive Statistics * ------------------------

# Calculate measures of central tendency and dispersion

descriptive_stats <- car_data %>%
  summarise(
    mean_mpg = mean(mpg),
    median_mpg = median(mpg),
    mode_mpg = mode_function(mpg),
    var_mpg = var(mpg),
    sd_mpg = sd(mpg),
    iqr_mpg = IQR(mpg),
    range_mpg = range(mpg),
    
    mean_hp = mean(hp),
    median_hp = median(hp),
    mode_hp = mode_function(hp),
    var_hp = var(hp),
    sd_hp = sd(hp),
    iqr_hp = IQR(hp),
    range_hp = range(hp),
    
    mean_wt = mean(wt),
    median_wt = median(wt),
    mode_wt = mode_function(wt),
    var_wt = var(wt),
    sd_wt = sd(wt),
    iqr_wt = IQR(wt),
    range_wt = range(wt),
    
    mean_qsec = mean(qsec),
    median_qsec = median(qsec),
    mode_qsec = mode_function(qsec),
    var_qsec = var(qsec),
    sd_qsec = sd(qsec),
    iqr_qsec = IQR(qsec),
    range_qsec = range(qsec)
  )

print(descriptive_stats)

# Custom mode function
mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Summarized output for descriptive statistics
descriptive_stats

# Visualization of Descriptive Statistics Using Box-plot for MPG, HP, WT and QSEC

# Box-plot for MPG

ggplot(car_data, aes(y = mpg)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Miles Per Gallon (MPG)", y = "MPG")

# Box-plot for HP
ggplot(car_data, aes(y = hp)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of Horsepower (HP)", y = "HP")

# Box-plot for WT
ggplot(car_data, aes(y = wt)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Boxplot of Weight (WT)", y = "Weight (1000 lbs)")

# Box-plot for QSEC
ggplot(car_data, aes(y = qsec)) +
  geom_boxplot(fill = "lightgoldenrod") +
  labs(title = "Boxplot of 1/4 Mile Time (QSEC)", y = "1/4 Mile Time (sec)")



