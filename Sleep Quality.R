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
data_1$Sleep.Quality <- as.numeric(data_1$Sleep.Quality)

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
                                                       labels = paste(seq(3, 9.5, by = 0.5), seq(3.5, 10, by = 0.5), sep = "-")))
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
data_4[, continuous_vars] <- data_4_cols_discretized

# Fit BN4
bn_4_df <- data.frame(data_4)
bn_4_df <- bn_4_df %>% select(-User.ID)
bn_4 <- hc(bn_4_df)
plot(bn_4)
graphviz.plot(bn_4, layout = "dot")


# Sleep.Quality.Ranking
data_5 <- data_4
data_5 <- data_5 %>% mutate(Sleep.Quality.Rating = cut(Sleep.Quality, 
                                                       breaks = c(0, 2, 4, 6, 8, 10), 
                                                       labels = c(1, 2, 3, 4, 5), 
                                                       right = TRUE))
data_5 <- data_5 %>% select(-Sleep.Quality)

# Fit BN5
bn_5_df <- data.frame(data_5)
bn_5_df <- bn_5_df %>% select(-User.ID)
bn_5 <- hc(bn_5_df)
plot(bn_5)
graphviz.plot(bn_5, layout = "dot")


# Evaluate BN5
bic_score_bn_5 <- score(bn_5, data = bn_5_df, type = "bic")
print(paste("BIC Score:", bic_score_bn_5))



# Todolist
# Add Melatonin
melatonin_data <- read.csv(file='SocialMediaUsage_SleepLatencyAnalysis_Singapore.csv', header=TRUE, sep=",", na.strings=".")

# Age.group
melatonin_data <- melatonin_data %>% mutate(Age.group = cut(Age, 
                                                            breaks = seq(0, 100, by = 10), 
                                                            right = FALSE, 
                                                            labels = paste(seq(0, 90, by = 10), seq(9, 99, by = 10), sep = "-")))
table(melatonin_data$Age.group)

# Sleep.duration.group
melatonin_data <- melatonin_data %>% mutate(Sleep.duration.group = cut(Total.Sleep.Time..hours., 
                                                                       breaks = seq(3, 10, by = 0.5), 
                                                                       right = FALSE, 
                                                                       labels = paste(seq(3, 9.5, by = 0.5), seq(3.5, 10, by = 0.5), sep = "-")))
table(melatonin_data$Sleep.duration.group)

# Sleep.Quality.Rating
melatonin_data <- melatonin_data %>% mutate(Sleep.Quality.Rating = cut(Sleep.Quality.Rating, 
                                                       breaks = c(0, 1, 2, 3, 4, 5), 
                                                       labels = c(1, 2, 3, 4, 5), 
                                                       right = TRUE))
table(melatonin_data$Sleep.Quality.Rating)

# Melatnonin.Level..pg.mL.

melatonin_data <- melatonin_data %>% mutate(Melatonin.Level.group..pg.mL. = cut(Melatonin.Level..pg.mL.,
                                                                                method = "hartemink",
                                                                                breaks = 3))

melatonin_data <- melatonin_data %>% mutate(Melatonin.Level.group..pg.mL. = cut(Melatonin.Level..pg.mL.,
                                                                               breaks = c(-Inf, 22.7, 40.4, Inf),
                                                                               labels = c("(-Inf, 22.7]", "(22.7, 40.4]", "(40.4, Inf]")))
table(melatonin_data$Melatonin.Level.group..pg.mL.)



# Fit Model
melatonin_subset <- melatonin_data[, c("Age.group", "Sleep.duration.group", "Sleep.Quality.Rating", "Melatonin.Level.group..pg.mL.")]
data_5_subset <- data_5[, c("Age.group", "Sleep.duration.group", "Sleep.Quality.Rating")]
melatonin_subset$Age.group <- as.factor(melatonin_subset$Age.group)
melatonin_subset$Sleep.duration.group <- as.factor(melatonin_subset$Sleep.duration.group)
melatonin_subset$Sleep.Quality.Rating <- as.factor(melatonin_subset$Sleep.Quality.Rating)
melatonin_subset$Melatonin.Level.group..pg.mL. <- as.factor(melatonin_subset$Melatonin.Level.group..pg.mL.)

bn_melatonin <- hc(melatonin_subset)
bn_melatonin_model <- bn.fit(bn_melatonin, data = melatonin_subset)

# Infer Melatonin Level using fitted model
data_6 <- data_5
data_6 <- data_6 %>% select(-User.ID)
new_predictions <- predict( object = bn_melatonin_model, 
                            data = data_6, 
                            node = "Melatonin.Level.group..pg.mL.", 
                            method = "bayes-lw")
data_6$Melatonin.Level.group..pg.mL. <- new_predictions

# Knn for imputation
data_7 <- kNN(data_6)
any(is.na(data_7))
data_7 <- data_7 %>% select(-(14:26))

# 2. 불완전한 학습 모델
# 모델이 특정 입력 값 조합에 대해 충분히 학습되지 않았거나, 추론에 필요한 변수들이 충분하지 않을 수 있습니다. Bayesian Network는 학습 데이터에 기초한 확률적 모델을 사용하는데, 일부 조합에 대한 학습이 부족하거나 해당 조합에 대한 데이터가 없을 경우 NA를 반환할 수 있습니다. (결측치나, 데이터 형식의 불일치, 데이터 불균형으로 인한 문제는 아님.)
# 해결 방법:
# 모델이 충분히 학습되었는지 확인하고, 필요하다면 학습 데이터를 더 많이 확보하거나 다른 변수들을 추가하여 모델을 개선할 수 있습니다.





# Whitelist, Blacklist






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




