setwd("/Users/henry/Desktop/Virginia Tech/2024 FALL/2024 Fall Lectures/Emerging topics in CS/Projects/sleep-quality")

# Install packages
install.packages(c("bnlearn", "ggplot2", "VIM", "lubridate", "dplyr", "BiocManager"))
BiocManager::install("Rgraphviz")
install.packages("gRain")
install.packages("infotheo")
install.packages("reshape2")
install.packages("caret")
install.packages("pdp")

# Import Libraries
library(bnlearn) # bnlearn
library(ggplot2) # visualization
library(VIM) # kNN
library(lubridate) # change date
library(dplyr) # data manipulation
library(Rgraphviz) # rgraphviz
library(gRain)
library(infotheo) # Calculate Mutual Information (MI)
library(reshape2) # for melt MI
library(caret) # Effectiveness
library(pdp) # Sensitivity Analysis

original_data <- read.csv(file='Health_Sleep_Statistics.csv', header=TRUE, sep=",", na.strings=".")


##########               bn_1                  ##########
##########           Initial BN                ##########

data_1 <- original_data
summary(data_1)
# View(data_1)
head(data_1)
colnames(data_1)

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

# MI
mi_matrix_data_1 <- mutinformation(data_1)

mi_melted_data_1 <- melt(mi_matrix_data_1)
colnames(mi_melted_data_1) <- c("Var1", "Var2", "MI")
ggplot(mi_melted_data_1, aes(Var1, Var2, fill = MI)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = median(mi_melted_data_1$MI)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +
  geom_text(aes(label = sprintf("%.3f", MI)), size = 3) +
  labs(title = "Initial Pairwise Mutual Information (MI) Table")


# Nomalized MI
entropy_vector_data_1 <- sapply(data_1, entropy)

nmi <- function(mi, ent1, ent2) {
  return(2 * mi / (ent1 + ent2))
}

nmi_matrix_data_1 <- outer(
  1:ncol(data_1),
  1:ncol(data_1),
  FUN = Vectorize(function(i, j) nmi(mi_matrix_data_1[i, j], entropy_vector_data_1[i], entropy_vector_data_1[j]))
)

colnames(nmi_matrix_data_1) <- rownames(nmi_matrix_data_1) <- colnames(data_1)

nmi_melted_data_1 <- melt(nmi_matrix_data_1)
colnames(nmi_melted_data_1) <- c("Var1", "Var2", "NMI")


ggplot(nmi_melted_data_1, aes(Var1, Var2, fill = NMI)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5, limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +
  geom_text(aes(label = sprintf("%.3f", NMI)), size = 3) +
  labs(title = "Initial Pairwise Normalized Mutual Information (NMI) Table")


# Fit BN1
data_1_df <- data.frame(data_1)
data_1_df <- data_1_df %>% select(-User.ID)
bn_1 <- hc(data_1_df)
plot(bn_1)
graphviz.plot(bn_1, layout = "dot", fontsize = 20)


##########             bn_2 ~ bn_5             ##########
##########  : Discretize continuous variables  ##########


##########               bn_2                  ##########
##########                Age                  ##########

# Age
data_2 <- data_1
data_2 <- data_2 %>% mutate(Age.group = cut(Age, 
                                            breaks = seq(0, 100, by = 10), 
                                            right = FALSE, 
                                            labels = paste(seq(0, 90, by = 10), seq(9, 99, by = 10), sep = "-")))
data_2 <- data_2 %>% select(-Age)
data_2$Age.group <- as.factor(data_2$Age.group)

# Fit BN2
data_2_df <- data.frame(data_2)
data_2_df <- data_2_df %>% select(-User.ID)
bn_2 <- hc(data_2_df)
plot(bn_2)
graphviz.plot(bn_2, layout = "dot")



##########               bn_3                  ##########
##########             Sleep duration          ##########

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
data_3_df <- data.frame(data_3)
data_3_df <- data_3_df %>% select(-User.ID)
bn_3 <- hc(data_3_df)
plot(bn_3)
graphviz.plot(bn_3, layout = "dot")

##########               bn_4                  ##########
##########   Daily.Steps and Calories.Burned   ##########

data_4 <- data_3
continuous_vars <- c("Daily.Steps", "Calories.Burned")
data_4_cols_discretized <- discretize(data_4, method = "hartemink", breaks = 3, ordered = TRUE, data = data_4[, continuous_vars])
table(data_4_cols_discretized$Daily.Steps)
table(data_4_cols_discretized$Calories.Burned)
data_4[, continuous_vars] <- data_4_cols_discretized

# Fit BN4
data_4_df <- data.frame(data_4)
data_4_df <- data_4_df %>% select(-User.ID)
bn_4 <- hc(data_4_df)
plot(bn_4)
graphviz.plot(bn_4, layout = "dot")


##########               bn_3                  ##########
##########        Sleep.Quality.Ranking        ##########

data_5 <- data_4
data_5 <- data_5 %>% mutate(Sleep.Quality.Rating = cut(Sleep.Quality, 
                                                       breaks = c(0, 2, 4, 6, 8, 10), 
                                                       labels = c(1, 2, 3, 4, 5), 
                                                       right = TRUE))
data_5 <- data_5 %>% select(-Sleep.Quality)

# Fit BN5
data_5_df <- data.frame(data_5)
data_5_df <- data_5_df %>% select(-User.ID)
bn_5 <- hc(data_5_df)
plot(bn_5)
graphviz.plot(bn_5, layout = "dot", fontsize = 30)


# Evaluate BIC Score for BN5
bic_score_bn_5 <- score(bn_5, data = data_5_df, type = "bic")
print(paste("BIC Score:", bic_score_bn_5))
aic_score_bn_5 <- score(bn_5, data = data_5_df, type = "aic")
print(paste("AIC Score:", aic_score_bn_5))


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


##########                  bn_6                    ##########
########## Infer Melatonin Level using fitted model ##########

data_6 <- data_5
data_6 <- data_6 %>% select(-User.ID)
new_predictions <- predict( object = bn_melatonin_model, 
                            data = data_6, 
                            node = "Melatonin.Level.group..pg.mL.", 
                            method = "bayes-lw")
data_6$Melatonin.Level.group..pg.mL. <- new_predictions

summary(data_6$Melatonin.Level.group..pg.mL.)

# Fit BN6
data_6_df <- data.frame(data_6)
bn_6 <- hc(data_6_df)
plot(bn_6)
graphviz.plot(bn_6, layout = "dot")

##########                  bn_7                    ##########
##########            Knn for imputation            ##########
 
data_7 <- kNN(data_6)
any(is.na(data_7))
data_7 <- data_7 %>% select(-(14:26))


# Fit BN7
data_7_df <- data.frame(data_7)
bn_7 <- hc(data_7_df)
plot(bn_7)
graphviz.plot(bn_7, layout = "dot", fontsize = 30)


# Evaluate BIC Score for BN7
bic_score_bn_7 <- score(bn_7, data = data_7_df, type = "bic")
print(paste("BIC Score:", bic_score_bn_7))
aic_score_bn_7 <- score(bn_7, data = data_7_df, type = "aic")
print(paste("AIC Score:", aic_score_bn_7))


##########                  bn_8                    ##########
##########           Whitelist, Blacklist           ##########

# Mutual Information (MI)
data_8 <- data_7

mi_matrix_data_8 <- mutinformation(data_8)
# print(mi_matrix_data_8)
# heatmap(as.matrix(mi_matrix_data_8), Rowv=NA, Colv=NA, col=heat.colors(256), scale="column")


mi_melted_data_8 <- melt(mi_matrix_data_8)
colnames(mi_melted_data_8) <- c("Var1", "Var2", "MI")
ggplot(mi_melted_data_8, aes(Var1, Var2, fill = MI)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = median(mi_melted_data_8$MI)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +
  geom_text(aes(label = sprintf("%.3f", MI)), size = 3) +
  labs(title = "Pairwise Mutual Information (MI) Table")


# Nomalized MI
entropy_vector_data_8 <- sapply(data_8, entropy)

nmi <- function(mi, ent1, ent2) {
  return(2 * mi / (ent1 + ent2))
}

nmi_matrix_data_8 <- outer(
  1:ncol(data_8),
  1:ncol(data_8),
  FUN = Vectorize(function(i, j) nmi(mi_matrix_data_8[i, j], entropy_vector_data_8[i], entropy_vector_data_8[j]))
)

colnames(nmi_matrix_data_8) <- rownames(nmi_matrix_data_8) <- colnames(data_8)

nmi_melted_data_8 <- melt(nmi_matrix_data_8)
colnames(nmi_melted_data_8) <- c("Var1", "Var2", "NMI")


ggplot(nmi_melted_data_8, aes(Var1, Var2, fill = NMI)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5, limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +
  geom_text(aes(label = sprintf("%.3f", NMI)), size = 3) +
  labs(title = "Pairwise Normalized Mutual Information (NMI) Table")


# Whitelist, Blacklist

wl = matrix(c(
  "Gender","Sleep.Disorders",
  "Dietary.Habits", "Sleep.Quality.Rating",
  "Dietary.Habits", "Sleep.duration.group",
  "Physical.Activity.Level", "Sleep.Quality.Rating",
  "Wake.up.Time", "Melatonin.Level.group..pg.mL.",
  "Bedtime", "Melatonin.Level.group..pg.mL.",
  "Physical.Activity.Level", "Melatonin.Level.group..pg.mL.",
  "Age.group", "Melatonin.Level.group..pg.mL."
),,2,byrow=TRUE)
colnames(wl) <- c("from", "to")
wl

bl = matrix(c(
  "Age.group", "Gender"
),,2,byrow=TRUE)
colnames(bl) <- c("from", "to")
bl

# Fit BN8
data_8_df <- data.frame(data_8)
bn_8 <- hc(data_8_df, whitelist = wl, blacklist = bl)
plot(bn_8)
graphviz.plot(bn_8, layout = "dot", fontsize=40)

# Evaluate BIC Score for BN8
bic_score_bn_8 <- score(bn_8, data = data_8_df, type = "bic")
print(paste("BIC Score:", bic_score_bn_8))
aic_score_bn_8 <- score(bn_8, data = data_8_df, type = "aic")
print(paste("AIC Score:", aic_score_bn_8))

# Inference Analysis
# sample data
set.seed(321)
n <- nrow(data_8)
train_index <- sample(1:n, size = 0.8 * n)
train_data <- data_8[train_index, ]
test_data <- data_8[-train_index, ]
summary(train_data)

# Augmentation
set.seed(123)

augment_data <- function(data, n) {
  augmented_data <- data
  
  for (i in 1:n) {
    new_entry <- data[sample(nrow(data), 1), ]
    
    # Continuous
    
    # Categorical
    new_entry$Gender <- sample(data$Gender, 1)
    new_entry$Physical.Activity.Level <- sample(data$Physical.Activity.Level, 1)
    new_entry$Dietary.Habits <- sample(data$Dietary.Habits, 1)
    new_entry$Sleep.Disorders <- sample(data$Sleep.Disorders, 1)
    new_entry$Medication.Usage <- sample(data$Medication.Usage, 1)
    new_entry$Age.group <- sample(data$Age.group, 1)
    new_entry$Daily.Steps <- sample(data$Daily.Steps, 1) 
    new_entry$Calories.Burned <- sample(data$Calories.Burned, 1)  
    new_entry$Sleep.duration.group <- sample(data$Sleep.duration.group, 1)
    new_entry$Sleep.Quality.Rating <- sample(data$Sleep.Quality.Rating, 1)
    new_entry$Melatonin.Level.group..pg.mL. <- sample(data$Melatonin.Level.group..pg.mL., 1)
    
    augmented_data <- rbind(augmented_data, new_entry)
  }
  
  return(augmented_data)
}
augmented_data_8 <- augment_data(train_data, 300)
train_data <- rbind(train_data, augmented_data_8)
summary(train_data)



# fit
bn_8_model <- bn.fit(bn_8, data = train_data)

# Effectiveness
# bn_8
predictions_bn_8 <- predict(bn_8_model, data = test_data, node = "Sleep.Quality.Rating", method = "bayes-lw")

predicted_values_bn_8 <- as.factor(predictions_bn_8)
conf_matrix_bn_8 <- confusionMatrix(predicted_values_bn_8, as.factor(test_data$Sleep.Quality.Rating))
accuracy_bn_8 <- conf_matrix$overall['Accuracy']
recall_bn_8 <- conf_matrix$byClass[, 'Recall']
precision_bn_8 <- conf_matrix$byClass[, 'Precision']
f_score_bn_8 <- 2 * (precision * recall) / (precision + recall)

cat("Accuracy: ", accuracy_bn_8, "\n")
cat("Recall: ", recall_bn_8, "\n")
cat("Precision: ", precision_bn_8, "\n")
cat("F-score: ", f_score_bn_8, "\n")



# Prediction using cpquery
predicted_1 <- cpquery(bn_8_model, event = (Sleep.Quality.Rating=="1"), evidence =(Age.group == "20-29" &
                                                                                   Gender == "m" &
                                                                                   Dietary.Habits == "unhealthy" &
                                                                                   Bedtime == "01:15" &
                                                                                   Sleep.Disorders == "yes"
                                                                                   ))
predicted_2 <- cpquery(bn_8_model, event = (Sleep.Quality.Rating=="2"), evidence =(Age.group == "20-29" &
                                                                                     Gender == "m" &
                                                                                     Dietary.Habits == "unhealthy" &
                                                                                     Bedtime == "01:15" &
                                                                                     Sleep.Disorders == "yes"
))
predicted_3 <- cpquery(bn_8_model, event = (Sleep.Quality.Rating=="3"), evidence =(Age.group == "20-29" &
                                                                                     Gender == "m" &
                                                                                     Dietary.Habits == "unhealthy" &
                                                                                     Bedtime == "01:15" &
                                                                                     Sleep.Disorders == "yes"
))
predicted_4 <- cpquery(bn_8_model, event = (Sleep.Quality.Rating=="4"), evidence =(Age.group == "20-29" &
                                                                                     Gender == "m" &
                                                                                     Dietary.Habits == "unhealthy" &
                                                                                     Bedtime == "01:15" &
                                                                                     Sleep.Disorders == "yes"
))
predicted_5 <- cpquery(bn_8_model, event = (Sleep.Quality.Rating=="5"), evidence =(Age.group == "20-29" &
                                                                                     Gender == "m" &
                                                                                     Dietary.Habits == "unhealthy" &
                                                                                     Bedtime == "01:15" &
                                                                                     Sleep.Disorders == "yes"
))

predicted_1
predicted_2
predicted_3
predicted_4
predicted_5

bn_8_model <- bn.fit(bn_8, data = data_8)
predicted <- cpquery(bn_8_model, event = (Sleep.Quality.Rating=="4"), evidence =(Age.group == "20-29" &
                                                                                   Gender == "m" &
                                                                                   Dietary.Habits == "unhealthy" &
                                                                                   Bedtime == "01:15" &
                                                                                   Sleep.Disorders == "yes"
))
predicted
