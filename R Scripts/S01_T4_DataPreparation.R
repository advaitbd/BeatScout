# Packages needed ====
library(data.table)
library(ggplot2)
library(caTools)
library(dplyr)
library(caret)
library(randomForest)
library(ModelMetrics)
library(rpart)
library(rpart.plot)
library(ggcorrplot)
library(DescTools)
library(car)
library(gridExtra)
library(tibble)

# Reading CSV File 
data <- fread("S01_T4_Tiktok DE Virality.csv", na.strings = c("NA", "missing", "N/A", "", "na", "M", "."))
summary(data)
# Change class of virality
data$virality <- as.factor(data$virality)

# MinMax Scaling ====
preproc <- preProcess(data, method = "range")
data <- predict(preproc, data)

# Train test split ====
set.seed(2)
train <- sample.split(Y=data$virality, SplitRatio = 0.7)
trainset <- subset(data, train==T)
testset <- subset(data, train==F)

# Feature Selection (used for all models) from Logistic Regression and RF varImp & OOB MSE ====

# Random Forest to Rank Top Features
fs <- randomForest(virality ~ ., data = trainset, importance = T)
# default parameters
fs
# OOB Rate = 17.04%
plot(fs)
# Error stablised before 500 trees

varImpPlot(fs, type = 1)
# var impt plot by MeanDecreaseAccuracy

var.impt.RF <- data.frame(importance(fs))
var.impt.RF <- rownames_to_column(var.impt.RF, var = "Variables")
var.impt.RF <- arrange(var.impt.RF, desc(MeanDecreaseAccuracy))
order <- c("virality" , var.impt.RF$Variables)

# Reorder the columns of the data frame based on the order
trainset <- setcolorder(trainset, order)

# Creating train sets with increasing variables ranked by var impt
t_list <- list()
for (i in 4:14) {
  t_list[[i-3]] <- trainset[, 1:i]
}

# Calc OOB EER for each train
oob_err <- numeric(length(t_list))
for (i in 1:length(t_list)) {
  fs <- randomForest(virality ~ ., data = t_list[[i]], importance = T)
  oob_err[i] <- fs$err.rate[500, "OOB"]
}

# Optimal number of features = 11
plot(5:15, oob_err, type = "b", xlab = "Number of Features", ylab = "Out of Bag MDA")
trainset <- trainset[,1:12]

# Logistic Regression to Conduct Feature Selection
log <- step(glm(virality ~ ., family = binomial, data = trainset), direction = "backward")
summary <- summary(log)
summary
var_namesLR <- rownames(summary$coefficients)[-1]

# Creating new train and test sets with features from Logistic Regression
trainset <- subset(trainset, select = c("virality",var_namesLR))
testset <- setcolorder(testset, order)
testset <- subset(testset, select = c("virality", var_namesLR))

# Balance Classes in Trainset ====
summary(trainset)
majority <- trainset[virality == "No"]
minority <- trainset[virality == "Yes"]
chosen <- sample(seq(1:nrow(minority)), size = (nrow(majority)-nrow(minority)), replace = TRUE)
upsampled_class <- minority[chosen]
upsampled_class <- rbind(upsampled_class, minority)
trainset.bal <- rbind(majority, upsampled_class)
summary(trainset.bal)

#Export to CSV
write.csv(trainset.bal, "Tiktok Virality Trainset.csv", row.names = FALSE)
write.csv(testset, "Tiktok Virality Testset.csv", row.names = FALSE)

