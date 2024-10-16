setwd("/Users/henry/Desktop/Virginia Tech/2024 FALL/2024 Fall Lectures/Emerging topics in CS/Projects/sleep-quality")

# Import Libraries
library(bnlearn) # bnlearn
library(ggplot2) # visualization
library(VIM) # kNN
library(lubridate) # change date

data <- read.csv(file='Sleep_Efficiency.csv', header=TRUE, sep=",", na.strings=".")
original_data <- data
summary(data)
View(data)
head(data)

# Preprocessing
## Feature encoding
### Gender
data$Gender <- as.factor(data$Gender)

### Bedtime
data$Bedtime <- ymd_hms(data$Bedtime)
data$Bedtime <- format(data$Bedtime, "%H:%M")

### Wakeup.time
data$Wakeup.time <- ymd_hms(data$Wakeup.time)
data$Wakeup.time <- format(data$Wakeup.time, "%H:%M")

### Smoking.status
data$Smoking.status <- as.factor(data$Smoking.status)

### Caffein.consumption
data$Caffeine.consumption <- as.numeric(data$Caffeine.consumption)

### Alcohol.consumption
data$Alcohol.consumption <- as.numeric(data$Alcohol.consumption)

### Exercise.frequency
data$Exercise.frequency <- as.numeric(data$Exercise.frequency)

## Missing Values
na_counts <- colSums(is.na(data))
na_counts[na_counts > 0]

### Use kNN for imputation
### Awakenings : 20
### Caffeine.consumption : 25
### Alcohol.consumption : 14
### Exercise.frequency : 6
imputed_data <- kNN(data)

any(is.na(imputed_data))

### changed data
awakenings_imputed <- imputed_data[imputed_data$Awakenings_imp == TRUE, "Awakenings"]
caffeine_imputed <- imputed_data[imputed_data$Caffeine.consumption_imp == TRUE, "Caffeine.consumption"]
alcohol_imputed <- imputed_data[imputed_data$Alcohol.consumption_imp == TRUE, "Alcohol.consumption"]
exercise_imputed <- imputed_data[imputed_data$Exercise.frequency_imp == TRUE, "Exercise.frequency"]

### Plot to compare
imputed_columns <- c("Awakenings", "Caffeine.consumption", "Alcohol.consumption", "Exercise.frequency")

for (col in imputed_columns) {
  original_data_long <- data.frame(Value = original_data[[col]], Dataset = "Original")
  imputed_data_long <- data.frame(Value = imputed_data[[col]], Dataset = "Imputed")
  combined_data <- rbind(original_data_long, imputed_data_long)
  
  p <- ggplot(combined_data, aes(x = Value, fill = Dataset)) +
    geom_density(alpha = 0.5) +
    labs(title = paste(col, ": Original vs Imputed"), x = col, y = "Density") +
    theme_minimal()
  
  print(p)
}


