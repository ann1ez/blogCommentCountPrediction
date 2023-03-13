### MS&E 226 Project Part 3

# import libraries
set.seed(2025)
#install.packages("cvTools")
library(cvTools)
install.packages("glmnet", repos = "https://cran.us.r-project.org")
library(glmnet)
library(readr)
library(boot)
library(stats)
library(dplyr)

# import data
setwd("/Users/andrewhong/Desktop/College/MS&E 226/Project")
data <- read.csv("blogData_train.csv")

## cutting out 21,901 random rows in training set
rowsTrainSet <- nrow(data) # 52,396 rows in training set
rowsTestSet <- 7624 # rows in testing set
# 7624 / 0.25 = 30495 training set rows needed given number of test set rows, so randomly throw out 21901 training set rows
randomRows <- sample(1:rowsTrainSet, 21901, replace = F)
length(unique(randomRows)) # check
data2 <- data[-randomRows,]
nrow(data2)
traindata <- data2
head(traindata)

## create a validation set from test set. Here we are creating a validation set to get a 80-10-10 split for training, validation, and testing. We end up with 3,641 rows in the validation set, 30,496 rows in the training set, and 3,983 rows in the testing set.
test_set <- read_csv("./BlogFeedback/blogData_test-2012.03.02.00_00.csv", col_names = FALSE, show_col_types = FALSE)
numbers <- seq(1, 31)
print(numbers)
numbers <- sprintf("%02d", numbers)
for (i in 3:25) {
  path_name <- paste("./BlogFeedback/blogData_test-2012.03.", numbers[i], 
                     ".00_00.csv", sep = "")
  new_data <- read_csv(path_name, col_names = FALSE, show_col_types = FALSE)
  test_set <- rbind(test_set, new_data)
}
for (i in 26:31) {
  path_name <- paste("./BlogFeedback/blogData_test-2012.03.", numbers[i], 
                     ".01_00.csv", sep = "")
  new_data <- read_csv(path_name, col_names = FALSE, show_col_types = FALSE)
  test_set <- rbind(test_set, new_data)
}
print(nrow(test_set))
print(colnames(test_set))
head(test_set)

# NAME COLUMNS
basic_attributes <- c("total_before", "before_24hr", "from_48hr_to_24hr", "first_24hr", "change_between_two_days")
prefix <- c("avg", "sd", "min", "max", "med")
suffix <- c("comments", "links")
for (i in 1:5){
  for (j in 1:2) {
    for (k in 1:5) {
      index = 10 * (i-1) + 5 * (j-1) + k
      colnames(test_set)[index] <- paste(prefix[k], basic_attributes[i], suffix[j], sep = "_")
      colnames(traindata)[index] <- paste(prefix[k], basic_attributes[i], suffix[j], sep = "_")
    }
  }
}
for (i in 1:5){
  for (j in 1:2) {
    index = 50 + i + 5 * (j-1)
    colnames(traindata)[index] <- paste(basic_attributes[i], suffix[j], sep = "_")
    colnames(test_set)[index] <- paste(basic_attributes[i], suffix[j], sep = "_")
  }
}
colnames(test_set)[61] <- "time_length"
colnames(traindata)[61] <- "time_length"
colnames(test_set)[62] <- "post_length"
colnames(traindata)[62] <- "post_length"
for (i in 1: 200){
  index = toString(i)
  colnames(test_set)[i + 62] <- paste("freq_word_feature_", index, sep = "")
  colnames(traindata)[i + 62] <- paste("freq_word_feature_", index, sep = "")
}
days <- c("mon", "tues", "wed", "thurs", "fri", "sat", "sun")
for (i in 1:7) {
  colnames(test_set)[i + 262] <- paste("basetime", days[i], sep = "_")
  colnames(traindata)[i + 262] <- paste("basetime", days[i], sep = "_")
}
for (i in 1:7) {
  colnames(test_set)[i + 269] <- paste("publication", days[i], sep = "_")
  colnames(traindata)[i + 269] <- paste("publication", days[i], sep = "_")
}
colnames(test_set)[277] <- "parent_pages_count"
colnames(test_set)[278] <- "min_parent_pages_comments"
colnames(test_set)[279] <- "max_parent_pages_comments"
colnames(test_set)[280] <- "avg_parent_pages_comments"
colnames(test_set)[281] <- "after_24hr_comments"
colnames(traindata)[277] <- "parent_pages_count"
colnames(traindata)[278] <- "min_parent_pages_comments"
colnames(traindata)[279] <- "max_parent_pages_comments"
colnames(traindata)[280] <- "avg_parent_pages_comments"
colnames(traindata)[281] <- "after_24hr_comments"
head(test_set)
head(traindata)

######################################################################

# TRAIN set glmnet()
bestLambda <- 0.5
X2 = select(traindata, -c(after_24hr_comments)) %>% data.matrix()
Y2 = traindata$after_24hr_comments
model2 <- glmnet(x = X2, y = Y2, alpha = 1, lambda=bestLambda)
summary(model2)
coef(model2)
model2$dev.ratio #r2 train=0.3354415 --> test=0.3398696

# TEST set glmnet()
bestLambda <- 0.5
X2_test = select(test_set, -c(after_24hr_comments)) %>% data.matrix()
Y2_test = test_set$after_24hr_comments
model2_test <- glmnet(x = X2_test, y = Y2_test, alpha = 1, lambda=bestLambda)
summary(model2_test)
model2_test$dev.ratio #r2=0.3354415 --> test=0.3398696

# TRAIN lm() without 0-covariates
train_X_covariates <- as.matrix(coef(model2))
X_rows <- rownames(train_X_covariates)[train_X_covariates != 0]
X_rows <- X_rows[!X_rows == "(Intercept)"]
X2 <- X2[,X_rows]
colnames(X2)
Y2 = traindata$after_24hr_comments
model2_new <- lm(Y2 ~ X2, data=traindata)
summary(model2_new)

# TEST lm() without 0-covariates
test_X_covariates <- as.matrix(coef(model2_test))
X_rows <- rownames(test_X_covariates)[test_X_covariates != 0]
X_rows <- X_rows[!X_rows == "(Intercept)"]
X2_test <- X2_test[,X_rows]
colnames(X2_test)
Y2_test = test_set$after_24hr_comments
model2_test_new <- lm(Y2_test ~ X2_test, data=test_set)
summary(model2_test_new)

#######################################
# TRAIN lm() with all covariates
model2_new_all <- lm(after_24hr_comments ~ . , data=traindata)
summary(model2_new_all) #r2=0.3723
p_vals_model2_new_all <- summary(model2_new_all)$coefficients[,4]  
sig_p_vals_p_vals_model2_new_all <- rep(NA, length(p_vals_model2_new_all))
for (i in 1:length(p_vals_model2_new_all)) {
  if (p_vals_model2_new_all[i] < 0.05) {
    sig_p_vals_p_vals_model2_new_all[i] <- colnames(traindata)[i]
  }
}
sig_p_vals_p_vals_model2_new_all <- na.omit(sig_p_vals_p_vals_model2_new_all)
sig_p_vals_p_vals_model2_new_all

######################################################################################################
covs <- rep(NA, 281)
for (i in 1:281) {
  if (coef(model2)[i] != 0){
    covs[i] <- i
  }
}
covs <- na.omit(covs)
covs

covs_test <- rep(NA, 281)
for (i in 1:281) {
  if (coef(model2_test)[i] != 0){
    covs_test[i] <- i
  }
}
covs_test <- na.omit(covs_test)
covs_test

# find significant values
"""sig_covs <- rep(NA, 281)
for (i in 1:281) {
  if (coef(model2)[i] != 0){
    sig_covs[i] <- i
  }
}
sig_covs <- na.omit(sig_covs)
sig_covs

sig_covs_test <- rep(NA, 281)
for (i in 1:281) {
  if (coef(model2_test)[i] != 0){
    sig_covs_test[i] <- i
  }
}
sig_covs_test <- na.omit(sig_covs_test)
sig_covs_test"""

######################################################################################################

# FUNCTION: VALIDATE WEIGHTING UNDERSTIMATES MORE ERROR
evaluation_cont_w <- function(model, data){
  sum_squared <- 0
  for (i in 1:nrow(data)) {
    resid <- data$after_24hr_comments[i] - predict(model, data)[i]
    if(resid > 0){
      sum_squared <- sum_squared + (1.3 * (resid)^2)
    }
    else{
      sum_squared <- sum_squared + (resid)^2
    }
  }
  rmse <- sqrt(sum_squared / nrow(data))
  return(rmse)
}

# FUNCTION: VALIDATE UNWEIGHTED
evaluation_cont <- function(model, data){
  sum_squared <- 0
  for (i in 1:nrow(data)) {
    sum_squared <- sum_squared + (data$after_24hr_comments[i] - predict(model, data)[i])^2
  }
  rmse <- sqrt(sum_squared / nrow(data))
  return(rmse)
}

######################################################################################################

# glmnet(), train set, unweighted
Xtest2 = select(test_set, -c(after_24hr_comments)) %>% data.matrix()
Ytest2 = test_set$after_24hr_comments
predictions2 <- predict(model2, newx = Xtest2)
MSE3.2 <- mean((predictions2 - Ytest2)^2)
RMSE3.2 <- sqrt(MSE3.2)
RMSE3.2 # val=24.41738 --> test=24.51266
RMSE3.2_val <- 24.41738
(RMSE3.2 - RMSE3.2_val) / RMSE3.2_val # 0.003902221

# glmnet(), train set, weighted:
Xtest2 = select(test_set, -c(after_24hr_comments)) %>% data.matrix()
Ytest2 = test_set$after_24hr_comments
predictions2w <- predict(model2, newx = Xtest2)
residuals2w <- Ytest2 - predictions2w
sum_squared <- 0
for (i in 1:length(predictions2w)){
  if(predictions2w[i] > 0){
    sum_squared <- sum_squared + (1.3 * (predictions2w[i] - Ytest2[i])^2)
  }
  else{
    sum_squared <- sum_squared + (predictions2w[i] - Ytest2[i])^2
  }
}
RMSE3.2w <- sqrt(sum_squared / length(predictions2w))
RMSE3.2w # val=27.77638 --> test=27.85164
RMSE3.2w_val <- 27.77638
(RMSE3.2w - RMSE3.2w_val) / RMSE3.2w_val # 0.002709664

# glmnet(), test set, unweighted
Xtest2 = select(test_set, -c(after_24hr_comments)) %>% data.matrix()
Ytest2 = test_set$after_24hr_comments
predictions3 <- predict(model2_test, newx = Xtest2)
MSE3.2 <- mean((predictions3 - Ytest2)^2)
RMSE3.2 <- sqrt(MSE3.2)
RMSE3.2 # val=24.41738 --> test=23.29949
RMSE3.2_val <- 24.41738
(RMSE3.2 - RMSE3.2_val) / RMSE3.2_val # -0.04578261

# glmnet(), test set, weighted:
Xtest2 = select(test_set, -c(after_24hr_comments)) %>% data.matrix()
Ytest2 = test_set$after_24hr_comments
predictions2w <- predict(model2_test, newx = Xtest2)
residuals2w <- Ytest2 - predictions2w
sum_squared <- 0
for (i in 1:length(predictions2w)){
  if(predictions2w[i] > 0){
    sum_squared <- sum_squared + (1.3 * (predictions2w[i] - Ytest2[i])^2)
  }
  else{
    sum_squared <- sum_squared + (predictions2w[i] - Ytest2[i])^2
  }
}
RMSE3.2w <- sqrt(sum_squared / length(predictions2w))
RMSE3.2w # val=27.77638 --> test=26.51685
RMSE3.2w_val <- 27.77638
(RMSE3.2w - RMSE3.2w_val) / RMSE3.2w_val # -0.04534529


# lm(), train, unweighted
RMSE3.2_new <- evaluation_cont(model2_new, test_set)
RMSE3.2_new # 32.43471

# lm(), train, weighted:
RMSE3.2_test_new <- evaluation_cont_w(model2_new, test_set)
RMSE3.2_test_new # 35.83114

# lm(), test, unweighted
RMSE3.2_new <- evaluation_cont(model2_test_new, test_set)
RMSE3.2_new # 22.96092

# lm(), test, weighted:
RMSE3.2_test_new <- evaluation_cont_w(model2_test_new, test_set)
RMSE3.2_test_new # 25.56867

##########################

# lm() with ALL covariates (baseline), train set, unweighted
RMSE_allcovariates <- evaluation_cont(model2_new_all, test_set)
RMSE_allcovariates # 24.98147

# lm() with ALL covariates (baseline), train set, weighted
RMSE_allcovariates_W <- evaluation_cont_w(model2_new_all, test_set)
RMSE_allcovariates_W # 27.82302


################################################################################################

# visualize RMSE's
par(mfrow=c(1,2))   
barplot(c(RMSE3.2w, RMSE3.2w_val), names.arg = c("Actual","Predicted"),col=rainbow(2), ylim=c(20,30), 
        main="Weighted Test Set RMSE 
(130% error for underestimates)", xlab="", ylab="Root Mean Squared Error (w)")
barplot(c(RMSE3.2, RMSE3.2_val), names.arg = c("Actual","Predicted"),col=rainbow(2), ylim=c(20,30),
        main="Unweighted Test Set RMSE", xlab="", ylab="Root Mean Squared Error")
