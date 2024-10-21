setwd("/Users/henry/Desktop/Virginia Tech/2024 FALL/2024 Fall Lectures/Emerging topics in CS/Projects/sleep-quality")

# Install packages
install.packages(c("bnlearn", "ggplot2", "VIM", "lubridate", "dplyr", "BiocManager"))
BiocManager::install("Rgraphviz")
install.packages("gRain")


# Import Libraries
library(bnlearn) # bnlearn
library(ggplot2) # visualization
library(VIM) # kNN
library(lubridate) # change date
library(dplyr) # data manipulation
library(Rgraphviz) # rgraphviz
library(gRain)


original_data <- read.csv(file='Health_Sleep_Statistics.csv', header=TRUE, sep=",", na.strings=".")
data_1 <- original_data
summary(data_1)
# View(data_1)
head(data_1)


# Preprocessing
# Age
data_1$Age <- as.numeric(data_1$Age)

# Gender
data_1$Gender <- as.factor(data_1$Gender)

# Sleep.Quality
data_1$Sleep.Quality <- as.factor(data_1$Sleep.Quality)

# Bedtime
data_1$Bedtime <- as.factor(data_1$Bedtime)

# Wake.up.Time
data_1$Wake.up.Time <- as.factor(data_1$Wake.up.Time)

# Daily Steps
data_1$Daily.Steps <- as.numeric(data_1$Daily.Steps)

# Calories.Burned
data_1$Calories.Burned <- as.numeric(data_1$Calories.Burned)

# Physical.Activity.Level
data_1$Physical.Activity.Level <- as.factor((data_1$Physical.Activity.Level))

# Dietary.Habits
data_1$Dietary.Habits <- as.factor(data_1$Dietary.Habits)

# Sleep.Disorders
data_1$Sleep.Disorders <- as.factor(data_1$Sleep.Disorders)

# Medication.Usage
data_1$Medication.Usage <- as.factor((data_1$Medication.Usage))


# Missing Values
na_counts <- colSums(is.na(data_1))
na_counts[na_counts > 0]


# Fit BN1
bn_1_df <- data.frame(data_1)
bn_1_df <- bn_1_df %>% select(-User.ID)
bn_1 <- hc(bn_1_df)
plot(bn_1)
graphviz.plot(bn_1, layout = "dot")



# Discretize continuous variables
# Age
data_2 <- data_1
data_2 <- data_2 %>% mutate(Age.group = cut(Age, 
                                            breaks = seq(0, 100, by = 10), 
                                            right = FALSE, 
                                            labels = paste(seq(0, 90, by = 10), seq(9, 99, by = 10), sep = "-")))
data_2 <- data_2 %>% select(-Age)
data_2$Age.group <- as.factor(data_2$Age.group)

# Fit BN2
bn_2_df <- data.frame(data_2)
bn_2_df <- bn_2_df %>% select(-User.ID)
bn_2 <- hc(bn_2_df)
plot(bn_2)
graphviz.plot(bn_2, layout = "dot")


# Sleep duration
data_3 <- data_2
data_3 <- data_3 %>% mutate(Bedtime = as.POSIXct(Bedtime, format = "%H:%M"),
                            Wake.up.Time = as.POSIXct(Wake.up.Time, format = "%H:%M"),
                            Sleep.duration = ifelse(Wake.up.Time < Bedtime, 
                                                    as.numeric(difftime(Wake.up.Time + 86400, Bedtime, units = "hours")), 
                                                    as.numeric(difftime(Wake.up.Time, Bedtime, units = "hours"))))

data_3 <- data_3 %>% mutate(Sleep.duration.group = cut(Sleep.duration, 
                                                       breaks = seq(3, 10, by = 0.5), 
                                                       right = FALSE, 
                                                       labels = paste(seq(3, 8.5, by = 0.5), seq(3.5, 10, by = 0.5), sep = "-")))
data_3 <- data_3 %>% select(-Sleep.duration)

data_3$Wake.up.Time <- ymd_hms(data_3$Wake.up.Time)
data_3$Wake.up.Time <- format(data_3$Wake.up.Time, "%H:%M")

data_3$Bedtime <- ymd_hms(data_3$Bedtime)
data_3$Bedtime <- format(data_3$Bedtime, "%H:%M")

data_3$Bedtime <- as.factor(data_3$Bedtime)
data_3$Wake.up.Time <- as.factor(data_3$Wake.up.Time)
data_3$Sleep.duration.group <- as.factor(data_3$Sleep.duration.group)

# Fit BN3
bn_3_df <- data.frame(data_3)
bn_3_df <- bn_3_df %>% select(-User.ID)
bn_3 <- hc(bn_3_df)
plot(bn_3)
graphviz.plot(bn_3, layout = "dot")


# Daily.Steps and Calories.Burned
data_4 <- data_3
continuous_vars <- c("Daily.Steps", "Calories.Burned")
data_4_cols_discretized <- discretize(data_4, method = "hartemink", breaks = 3, ordered = TRUE, data = data_4[, continuous_vars])
table(data_4_cols_discretized$Daily.Steps)
table(data_4_cols_discretized$Calories.Burned)

temp <- data_4
temp[, continuous_vars] <- data_4_cols_discretized
data_4_discretized <- temp
head(data_4_discretized)

# Fit BN4
bn_4_df <- data.frame(data_4_discretized)
bn_4_df <- bn_4_df %>% select(-User.ID)
bn_4 <- hc(bn_4_df)
plot(bn_4)
graphviz.plot(bn_4, layout = "dot")


# Evaluate Model
bic_score_bn_4 <- score(bn_4, data = bn_4_df, type = "bic")
print(paste("BIC Score:", bic_score_bn_4))




# Plot using graphviz
fitted_bn_2 <- bn.fit(bn_2, data = bn_2_df)
graphviz.chart(
  fitted_bn_2,
  type = "barprob",
  layout = "fdp",
  scale = c(30, 30),
  grid = TRUE,
  bar.col = "red",
  strip.bg = "lightskyblue"
)
fitted_bn_2




# Todolist
# Add Melatonin
melatonin_data <- read.csv(file='SocialMediaUsage_SleepLatencyAnalysis_Singapore.csv', header=TRUE, sep=",", na.strings=".")
melatonin_subset <- melatonin_data[, c("Age", "Gender", "Total.Sleep.Time..hours.", "Number.of.Awakenings..during.sleep.", "Melatonin.Level..pg.mL.")]
colnames(melatonin_subset) <- c("Age", "Gender", "Total_Sleep_Time", "Awakenings", "Melatonin_Level")

data_1_subset <- data_1_discretized[, c("Age", "Gender", "Sleep.duration", "Awakenings")]
colnames(data_1_subset) <- c("Age", "Gender", "Total_Sleep_Time", "Awakenings")

melatonin_continuous_vars <- c("Total_Sleep_Time", "Melatonin_Level")
melatonin_subset_cols_discretized <- discretize(melatonin_subset, method = "hartemink", breaks = 3, ordered = TRUE, data = melatonin_subset[, melatonin_continuous_vars])
table(melatonin_subset_cols_discretized$Total_Sleep_Time)
table(melatonin_subset_cols_discretized$Melatonin_Level)

temp <- melatonin_subset
temp[, melatonin_continuous_vars] <- melatonin_subset_cols_discretized
melatonin_subset_discretized <- temp

bn_melatonin_subset <- melatonin_subset_discretized[, c("Age", "Gender", "Total_Sleep_Time", "Awakenings", "Melatonin_Level")]
bn_melatonin_subset$Age <- as.factor(bn_melatonin_subset$Age)
bn_melatonin_subset$Gender <- as.factor(bn_melatonin_subset$Gender)
bn_melatonin_subset$Total_Sleep_Time <- as.factor(bn_melatonin_subset$Total_Sleep_Time)
bn_melatonin_subset$Awakenings <- as.factor(bn_melatonin_subset$Awakenings)
bn_melatonin_subset$Melatonin_Level <- as.factor(bn_melatonin_subset$Melatonin_Level)

bn_melatonin <- hc(bn_melatonin_subset)
bn_melatonin_model <- bn.fit(bn_melatonin, data = bn_melatonin_subset)

new_data <- data_1_discretized[, c("Age", "Gender", "Sleep.duration", "Awakenings")]
colnames(new_data) <- c("Age", "Gender", "Total_Sleep_Time", "Awakenings")


levels(new_data$Total_Sleep_Time)
levels(melatonin_subset_discretized$Total_Sleep_Time)

data_1_discretized$Sleep.duration <- cut(as.numeric(data_1_discretized$Sleep.duration), breaks = 3, labels = levels(melatonin_subset_discretized$Total_Sleep_Time))
data_1_discretized$Awakenings <- cut(as.numeric(data_1_discretized$Awakenings), breaks = 3, labels = levels(melatonin_subset_discretized$Awakenings))

summary(new_data)
summary(bn_melatonin_subset)



new_data$Inferred.melatonin.level <- predict(bn_melatonin_model, newdata = new_data)
data_1_discretized$Inferred.melatonin.level <- new_data$Inferred.melatonin.level
head(data_1_discretized)


# Whitelist, Blacklist

