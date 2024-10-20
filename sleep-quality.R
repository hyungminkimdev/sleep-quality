setwd("/Users/henry/Desktop/Virginia Tech/2024 FALL/2024 Fall Lectures/Emerging topics in CS/Projects/sleep-quality")


# Import Libraries
library(bnlearn) # bnlearn
library(ggplot2) # visualization
library(VIM) # kNN
library(lubridate) # change date
library(dplyr) # data manipulation

data <- read.csv(file='Sleep_Efficiency.csv', header=TRUE, sep=",", na.strings=".")
original_data <- data
summary(data)
View(data)
head(data)


# Preprocessing
# Age
data$Age <- as.factor(data$Age)

# Gender
data$Gender <- as.factor(data$Gender)

# Bedtime
data$Bedtime <- ymd_hms(data$Bedtime)
data$Bedtime <- format(data$Bedtime, "%H:%M")
data$Bedtime <- as.factor(data$Bedtime)

# Wakeup.time
data$Wakeup.time <- ymd_hms(data$Wakeup.time)
data$Wakeup.time <- format(data$Wakeup.time, "%H:%M")
data$Wakeup.time <- as.factor(data$Wakeup.time)

# Sleep duration
data$Sleep.duration <- as.factor(data$Sleep.duration)

# Sleep efficiency
data$Sleep.efficiency <- as.numeric(data$Sleep.efficiency)

# Rem sleep percentage
data$REM.sleep.percentage <- as.numeric(data$REM.sleep.percentage)

# Deep sleep percentage
data$Deep.sleep.percentage <- as.numeric(data$Deep.sleep.percentage)

# Light sleep percentage
data$Light.sleep.percentage <- as.numeric(data$Light.sleep.percentage)

# Awakenings
data$Awakenings <- as.factor(data$Awakenings)

# Caffein.consumption
data$Caffeine.consumption <- factor(data$Caffeine.consumption)

# Alcohol.consumption
data$Alcohol.consumption <- factor(data$Alcohol.consumption)

# Smoking.status
data$Smoking.status <- as.factor(data$Smoking.status)

# Exercise.frequency
data$Exercise.frequency <- as.factor(data$Exercise.frequency)


# Missing Values
na_counts <- colSums(is.na(data))
na_counts[na_counts > 0]

# Use kNN for imputation
# Awakenings : 20
# Caffeine.consumption : 25
# Alcohol.consumption : 14
# Exercise.frequency : 6
imputed_data <- kNN(data)

# changed data
awakenings_imputed <- imputed_data[imputed_data$Awakenings_imp == TRUE, "Awakenings"]
caffeine_imputed <- imputed_data[imputed_data$Caffeine.consumption_imp == TRUE, "Caffeine.consumption"]
alcohol_imputed <- imputed_data[imputed_data$Alcohol.consumption_imp == TRUE, "Alcohol.consumption"]
exercise_imputed <- imputed_data[imputed_data$Exercise.frequency_imp == TRUE, "Exercise.frequency"]

imputed_data_filtered <- imputed_data %>% select(-(16:30))
any(is.na(imputed_data_filtered))

# Plot to compare Original and Imputed
imputed_columns <- c("Awakenings", "Caffeine.consumption", "Alcohol.consumption", "Exercise.frequency")

for (col in imputed_columns) {
  original_data_long <- data.frame(Value = original_data[[col]], Dataset = "Original")
  imputed_data_filtered_long <- data.frame(Value = imputed_data_filtered[[col]], Dataset = "Imputed")
  combined_data <- rbind(original_data_long, imputed_data_filtered_long)
  
  p <- ggplot(combined_data, aes(x = Value, fill = Dataset)) +
    geom_density(alpha = 0.5) +
    labs(title = paste(col, ": Original vs Imputed"), x = col, y = "Density") +
    theme_minimal()
  
  print(p)
}

# Discretize continuous variables
continuous_vars <- c("Sleep.efficiency", "REM.sleep.percentage", "Deep.sleep.percentage", "Light.sleep.percentage")
imputed_data_discretized <- discretize(imputed_data_filtered, method = "hartemink", breaks = 3, ordered = TRUE, data = imputed_data_filtered[, continuous_vars])
head(imputed_data_discretized)

imputed_data_filtered[, continuous_vars] <- imputed_data_discretized
head(imputed_data_filtered)

table(imputed_data_discretized$Sleep.efficiency)
table(imputed_data_discretized$REM.sleep.percentage)
table(imputed_data_discretized$Deep.sleep.percentage)
table(imputed_data_discretized$Light.sleep.percentage)

# Fit Bayesian Network
original_bn_df <- data.frame(imputed_data_filtered)
original_bn_df <- original_bn_df %>% select(-ID)
head(bn_df)

# Visualize Model
original_bn <- hc(original_bn_df)
plot(original_bn)

# Evaluate Model
bic_score_original_bn <- score(original_bn, data = original_bn_df, type = "bic")
print(paste("BIC Score:", bic_score_original_bn))
