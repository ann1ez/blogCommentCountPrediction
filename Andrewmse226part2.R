### MS&E 226 Project Part 2

# import libraries
set.seed(2025)
#install.packages("cvTools")
library(cvTools)
install.packages("glmnet", repos = "https://cran.us.r-project.org")
library(glmnet)


# import data
setwd("/Users/andrewhong/Desktop/College/MS&E 226/Project")
data <- read.csv("blogData_train.csv")

## cutting out 21,901 random rows in training set to get 80:20 train:test ratio of rows/instances
set.seed(2025)
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
library(readr)
validatedata_test <- read_csv("./BlogFeedback/blogData_test-2012.02.01.00_00.csv", col_names = FALSE, show_col_types = FALSE)
numbers <- seq(1, 31)
numbers <- sprintf("%02d", numbers)
for (i in 2:29) {
  path_name <- paste("./BlogFeedback/blogData_test-2012.02.", numbers[i], 
                     ".00_00.csv", sep = "")
  new_data <- read_csv(path_name, col_names = FALSE, show_col_types = FALSE)
  validatedata_test <- rbind(validatedata_test, new_data)
}
new_data <- read_csv("./BlogFeedback/blogData_test-2012.03.01.00_00.csv", 
                     col_names = FALSE, show_col_types = FALSE)
validatedata_test <- rbind(validatedata_test, new_data)
print(nrow(validatedata_test))
print(colnames(validatedata_test))
head(validatedata_test)

# name columns
basic_attributes <- c("total_before", "before_24hr", "from_48hr_to_24hr", "first_24hr", "change_between_two_days")
prefix <- c("avg", "sd", "min", "max", "med")
suffix <- c("comments", "links")
for (i in 1:5){
  for (j in 1:2) {
    for (k in 1:5) {
      index = 10 * (i-1) + 5 * (j-1) + k
      colnames(data)[index] <- paste(prefix[k], basic_attributes[i], suffix[j], sep = "_")
      colnames(validatedata_test)[index] <- paste(prefix[k], basic_attributes[i], suffix[j], sep = "_")
      colnames(traindata)[index] <- paste(prefix[k], basic_attributes[i], suffix[j], sep = "_")
    }
  }
}
for (i in 1:5){
  for (j in 1:2) {
    index = 50 + i + 5 * (j-1)
    colnames(data)[index] <- paste(basic_attributes[i], suffix[j], sep = "_")
    colnames(traindata)[index] <- paste(basic_attributes[i], suffix[j], sep = "_")
    colnames(validatedata_test)[index] <- paste(basic_attributes[i], suffix[j], sep = "_")
  }
}
colnames(data)[61] <- "time_length"
colnames(validatedata_test)[61] <- "time_length"
colnames(traindata)[61] <- "time_length"
colnames(data)[62] <- "post_length"
colnames(validatedata_test)[62] <- "post_length"
colnames(traindata)[62] <- "post_length"
for (i in 1: 200){
  index = toString(i)
  colnames(data)[i + 62] <- paste("freq_word_feature_", index, sep = "")
  colnames(validatedata_test)[i + 62] <- paste("freq_word_feature_", index, sep = "")
  colnames(traindata)[i + 62] <- paste("freq_word_feature_", index, sep = "")
}
days <- c("mon", "tues", "wed", "thurs", "fri", "sat", "sun")
for (i in 1:7) {
  colnames(data)[i + 262] <- paste("basetime", days[i], sep = "_")
  colnames(validatedata_test)[i + 262] <- paste("basetime", days[i], sep = "_")
  colnames(traindata)[i + 262] <- paste("basetime", days[i], sep = "_")
}
for (i in 1:7) {
  colnames(data)[i + 269] <- paste("publication", days[i], sep = "_")
  colnames(validatedata_test)[i + 269] <- paste("publication", days[i], sep = "_")
  colnames(traindata)[i + 269] <- paste("publication", days[i], sep = "_")
}
colnames(data)[277] <- "parent_pages_count"
colnames(data)[278] <- "min_parent_pages_comments"
colnames(data)[279] <- "max_parent_pages_comments"
colnames(data)[280] <- "avg_parent_pages_comments"
colnames(data)[281] <- "after_24hr_comments"
colnames(validatedata_test)[277] <- "parent_pages_count"
colnames(validatedata_test)[278] <- "min_parent_pages_comments"
colnames(validatedata_test)[279] <- "max_parent_pages_comments"
colnames(validatedata_test)[280] <- "avg_parent_pages_comments"
colnames(validatedata_test)[281] <- "after_24hr_comments"
colnames(traindata)[277] <- "parent_pages_count"
colnames(traindata)[278] <- "min_parent_pages_comments"
colnames(traindata)[279] <- "max_parent_pages_comments"
colnames(traindata)[280] <- "avg_parent_pages_comments"
colnames(traindata)[281] <- "after_24hr_comments"
head(data)
head(validatedata_test)
head(traindata)

# ANNIE: Next I also want to count the number of rows cumulatively across the 60 different test datasets to help determine if we want to change the train-test split.

"test_count <- 0
numbers <- seq(1, 31)
numbers <- sprintf("%02d", numbers)
for (i in 1:29) {
  path_name <- paste("./BlogFeedback/blogData_test-2012.02.", numbers[i], ".00_00.csv", sep = "")
  test_data <- read.csv(path_name)
  test_count <- test_count + nrow(test_data)
}
for (i in 1:25) {
  path_name <- paste("./BlogFeedback/blogData_test-2012.03.", numbers[i], ".00_00.csv", sep = "")
  test_data <- read.csv(path_name)
  test_count <- test_count + nrow(test_data)
}
for (i in 26:31) {
  path_name <- paste("./BlogFeedback/blogData_test-2012.03.", numbers[i], ".01_00.csv", sep = "")
  test_data <- read.csv(path_name)
  test_count <- test_count + nrow(test_data)
}"

# ANNIE: This means just from the way the data set was split from the repository, we would have 7624 rows for testing and 52397 rows for training. This creates about a 87\% \/ 13\% split between the training and testing data respectively, but typically a 80\% \/ 20\% split is used. Since we have 60021 instances in total, it would make sense for us to try to balance out the split so that there is more testing data, so later we can have more confidence in our generalized error calculation. The easiest way for us to achieve that 80\% \/ 20\% split is for us to randomly select 21901 rows from the initial training dataset to NOT include in our analysis. That way we would have 7624 rows for testing, 30496 rows for training, and 38120 instances in total. 
# ANNIE: With this new split and removal of some training data, we are still well above the minimum requirement for number of rows, and we do not have to worry at all about whether the train \/ test split is getting messed up by fact that some of this data relies on time covariates (i.e. 24 hrs before, 48hrs before) because we are not transitioning training data into testing data.


"## create validation set from train set
set.seed(2025)
sample <- sample(seq_len(nrow(traindata)), size = 24397)
traindata <- data2[sample,]
validatedata_train <- data2[-sample,]
ncol(validatedata_train)
ncol(traindata)
nrow(validatedata_train)
nrow(traindata)

# converting NaN/Inf to NA
traindata[is.na(traindata) | traindata == "Inf"] <- NA  # Replace NaN & Inf with NA
"


#######################################################################################################





## creating continuous models

#model 1: everything standard lm
model1 <- lm(after_24hr_comments ~ . , data=traindata)
summary(model1) #r2=0.3489
# In my first model, I simply create a linear model of all covariates given equally as a baseline. I deleted collinear columns created from new engineered columns I created to mitigate collinearity in the data.

#model 2: taking away some covariates bag of 200 words
dropCols = c(63:276)
model2 <- lm(after_24hr_comments ~ . , data=traindata[-dropCols])
summary(model2) #r2=0.3408 | 0.3404
# In my second model, I decided to remove columns that represented the 200 bag of words featured for 200 most frequent words of the blog post text to see if that data would cause overfitting on the specific training sets, causing increased prediction error on the test set or validation set.


#model 4: higher order terms on pre-24hr covariate measurements, degree=3
dropCols = c(11,12,13,14,15,16,17,18,19,20,52,57)
model4 <- lm(after_24hr_comments ~ I(avg_before_24hr_comments)^3 + 
               I(sd_before_24hr_comments)^3 + I(min_before_24hr_comments)^3 + 
               I(max_before_24hr_comments)^3 + I(med_before_24hr_comments)^3 + 
               I(avg_before_24hr_links)^3 + I(sd_before_24hr_links)^3 + 
               I(min_before_24hr_links)^3 + I(max_before_24hr_links)^3 + 
               I(med_before_24hr_links)^3 + I(before_24hr_comments)^3 + 
               I(before_24hr_links)^3 + ., data=traindata)
summary(model4) #r2=0.3489


#model 5: only terms on pre-24hr covariate measurements
model5 <- lm(traindata$after_24hr_comments ~ traindata$avg_before_24hr_comments + 
               traindata$sd_before_24hr_comments + traindata$min_before_24hr_comments + 
               traindata$max_before_24hr_comments + traindata$med_before_24hr_comments + 
               traindata$avg_before_24hr_links + traindata$sd_before_24hr_links + 
               traindata$min_before_24hr_links + traindata$max_before_24hr_links + 
               traindata$med_before_24hr_links + traindata$before_24hr_comments + 
               traindata$before_24hr_links, data=traindata)
summary(model5) #r2=0.2993

#model 6: log only terms on pre-24hr covariate measurements
traindata_temp6 = traindata + 1e-09
model6 <- lm(data=traindata_temp6, log(after_24hr_comments) ~ log(avg_before_24hr_comments) + 
               log(sd_before_24hr_comments) + log(min_before_24hr_comments) + 
               log(max_before_24hr_comments) + log(med_before_24hr_comments) + 
               log(avg_before_24hr_links) + log(sd_before_24hr_links) + 
               log(min_before_24hr_links) + log(max_before_24hr_links) + 
               log(med_before_24hr_links) + log(before_24hr_comments) + 
               log(before_24hr_links))
summary(model6) #r2=0.3544

#model 7: using centering
traindata_temp7 <- traindata
for (i in 1:280){
  traindata_temp7[,i] <- traindata_temp7[,i] - mean(traindata_temp7[,i])
}
head(traindata_temp7)
model7 <- lm(after_24hr_comments ~ . , data=traindata_temp7)
summary(model7) #r2=0.3489, intercept=6.493

#model 8: model1 + interaction terms: parent_pages_count-max_parent_pages_comments and basetime_Sun-before_24hr_links
model8 <- lm(after_24hr_comments ~ . + parent_pages_count:max_parent_pages_comments +
               basetime_sun:before_24hr_links + 
               , data=traindata)
summary(model8) #r2=0.3489


#model 9: combination of model6 and model1 (all columns but add log of 24hr-pre-basetime)
dropCols = c(63:262)
traindata_temp9 = traindata + 1e-09
model9 <- lm(log(after_24hr_comments) ~ . + log(avg_before_24hr_comments) + 
               log(sd_before_24hr_comments) + log(min_before_24hr_comments) + 
               log(max_before_24hr_comments) + log(med_before_24hr_comments) + 
               log(avg_before_24hr_links) + log(sd_before_24hr_links) + 
               log(min_before_24hr_links) + log(max_before_24hr_links) + 
               log(med_before_24hr_links) + log(before_24hr_comments) + 
               log(before_24hr_links), data=traindata_temp9[-dropCols])
summary(model9) #r2=0.4135


# LASSO model 10: lasso of transfornations n siht
dropCols = c(63:262)
traindata_temp10 = traindata + 1e-09
model10 <- lm()
summary(model10) #r2=


################# TESTING MODELS NORMAL WEIGHTING #################### ##########


library(cvTools)

# FUNCTION: VALIDATE UNWEIGHTED
evaluation_cont <- function(model, data){
  sum_squared <- 0
  for (i in 1:nrow(data)) {
    sum_squared <- sum_squared + (data$after_24hr_comments[i] - predict(model, data)[i])^2
  }
  rmse <- sqrt(sum_squared / nrow(data))
  return(rmse)
}

#model 1 k-fold CV:
RMSE1.1 <- cvFit(model1, traindata, y=traindata$after_24hr_comments, K=5, seed=2025)
RMSE1.1 # 31.01574

#model 2 k-fold CV:
RMSE1.2 <- cvFit(model2, traindata, y=traindata$after_24hr_comments, K=5, seed=2025)
RMSE1.2 # 31.01574

#model 3 k-fold CV: ERROR BC LASSO NOT WORKING
RMSE1.3 <- cvFit(model3, traindata, y=traindata$after_24hr_comments, K=5, seed=2025)
RMSE1.3 # 

#model 4 k-fold CV: ERROR BC DIFFERNT LENGTHS VARIABLES AVG_TOTAL_BEFORE_COMMENTS
RMSE1.4 <- cvFit(model4, traindata, y=traindata$after_24hr_comments, K=5, seed=2025)
RMSE1.4 # 

#model 5 k-fold CV:
RMSE1.5 <- cvFit(model5, traindata, y=traindata$after_24hr_comments, K=5, seed=2025)
RMSE1.5 # 46.35825

#model 6 k-fold CV:
RMSE1.6 <- cvFit(model5, traindata, y=traindata$after_24hr_comments, K=5, seed=2025)
RMSE1.6 # 46.35825

#model 7 k-fold CV:
RMSE1.7 <- cvFit(model5, traindata, y=traindata$after_24hr_comments, K=5, seed=2025)
RMSE1.7 # 46.35825

#model 8 k-fold CV:
RMSE1.8 <- cvFit(model5, traindata, y=traindata$after_24hr_comments, K=5, seed=2025)
RMSE1.8 # 46.35825

#model 9 k-fold CV:
RMSE1.9 <- cvFit(model5, traindata, y=traindata$after_24hr_comments, K=5, seed=2025)
RMSE1. # 46.35825

#install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)

#model 1 k-fold LOOCV
ctrl <- trainControl(method = "LOOCV")
model4.1 <- train(traindata$after_24hr_comments ~ ., data=traindata[-dropCols], method = "lm", trControl = ctrl)
print(model4.1)
RMSE4.1 # 31.81966

#model 2 k-fold LOOCV:
RMSE4.2 <- cvFit(model2, traindata, y=traindata$after_24hr_comments, K=5, seed=2025)
RMSE4.2 # 

#model 3 k-fold LOOCV:
RMSE4.3 <- cvFit(model3, traindata, y=traindata$after_24hr_comments, K=5, seed=2025)
RMSE4.3 # 

#model 4 k-fold LOOCV:
RMSE4.4 <- cvFit(model4, traindata, y=traindata$after_24hr_comments, K=5, seed=2025)
RMSE4.4 # 

#model 5 k-fold LOOCV:
RMSE4.5 <- cvFit(model5, traindata, y=traindata$after_24hr_comments, K=5, seed=2025)
RMSE4.5 # 

#model 1 validation-train set:
RMSE2.1 <- evaluation_cont(model1, validatedata_train)
RMSE2.1 # 28.26032

#model 2 validation-train set:
RMSE2.2 <- evaluation_cont(model2, validatedata_train)
RMSE2.2 # 19.77577

#model 3 validation-train set:
RMSE2.3 <- evaluation_cont(model3, validatedata_train)
RMSE2.3 # TODO

#model 4 validation-train set:
RMSE2.4 <- evaluation_cont(model4, validatedata_train)
RMSE2.4 # 19.77577 (sus)

#model 5 validation-train set:
RMSE2.5 <- evaluation_cont(model5, validatedata_train)
RMSE2.5 # 29.71252

#model 6 validation-train set:
RMSE2.6 <- evaluation_cont(model6, validatedata_train)
RMSE2.6 # NaN

#model 7 validation-train set:
RMSE2.7 <- evaluation_cont(model7, validatedata_train)
RMSE2.7 # 19.85921

#model 8 validation-train set:
RMSE2.8 <- evaluation_cont(model8, validatedata_train)
RMSE2.8 # 19.77537

#model 9 validation-train set:
RMSE2.9 <- evaluation_cont(model9, validatedata_train)
RMSE2.9 # NaN???

## IMPORTANTEST:
#model 1 validation-test set:
RMSE3.1 <- evaluation_cont(model1, validatedata_test)
RMSE3.1 # 24.93532

#model 2 validation-test set:
RMSE3.2 <- evaluation_cont(model2, validatedata_test)
RMSE3.2 # 24.75317

#model 3 validation-test set:
#RMSE3.3 <- evaluation_cont(model3, validatedata_test)
#RMSE3.3 # TODO

#model 4 validation-test set:
RMSE3.4 <- evaluation_cont(model4, validatedata_test)
RMSE3.4 # 24.93532

#model 5 validation-test set:
RMSE3.5 <- evaluation_cont(model5, validatedata_test)
RMSE3.5 # 34.2279

#model 6 validation-test set:
validatedata_test_temp6 <- validatedata_test + 1e-09
RMSE3.6 <- evaluation_cont(model6, validatedata_test_temp6)
RMSE3.6 # 36.98585

#model 7 validation-test set:
RMSE3.7 <- evaluation_cont(model7, validatedata_test)
RMSE3.7 # 26.3101

#model 8 validation-test set:
RMSE3.8 <- evaluation_cont(model8, validatedata_test)
RMSE3.8 # 24.9193

#model 9 validation-test set:
validatedata_test_temp9 <- validatedata_test + 1e-09
RMSE3.9 <- evaluation_cont(model9, validatedata_test_temp9)
RMSE3.9 # 36.33881

### VALIDATION BUT WITH PENALTY ON UNDERESTIMATION ###

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

#model 1 validation-train set weighted:
RMSE2.1w <- evaluation_cont_w(model1, validatedata_train)
RMSE2.1w # 28.26032

#model 2 validation-train set weighted:
RMSE2.2w <- evaluation_cont_w(model2, validatedata_train)
RMSE2.2w # 

#model 3 validation-train set weighted:
RMSE2.3w <- evaluation_cont_w(model3, validatedata_train)
RMSE2.3w # 

#model 4 validation-train set weighted:
RMSE2.4w <- evaluation_cont_w(model4, validatedata_train)
RMSE2.4w # 

#model 5 validation-train set weighted:
RMSE2.5w <- evaluation_cont_w(model5, validatedata_train)
RMSE2.5w # 


## IMPORTANTEST:
#model 1 validation-test set weighted:
RMSE3.1w <- evaluation_cont_w(model1, validatedata_test)
RMSE3.1w # 27.83455

#model 2 validation-test set weighted:
RMSE3.2w <- evaluation_cont_w(model2, validatedata_test)
RMSE3.2w # 27.65683

#model 3 validation-test set weighted:
#RMSE3.3w <- evaluation_cont_w(model3, validatedata_test)
#RMSE3.3w # TODO

#model 4 validation-test set weighted:
RMSE3.4w <- evaluation_cont_w(model4, validatedata_test)
RMSE3.4w # 27.83456

#model 5 validation-test set weighted:
RMSE3.5w <- evaluation_cont_w(model5, validatedata_test)
RMSE3.5w # 38.28275

#model 6 validation-test set weighted:
validatedata_test_temp6w <- validatedata_test + 1e-09
RMSE3.6w <- evaluation_cont_w(model6, validatedata_test_temp6w)
RMSE3.6w # 42.17036

#model 7 validation-test set weighted:
RMSE3.7w <- evaluation_cont_w(model7, validatedata_test)
RMSE3.7w # 27.78359

#model 8 validation-test set weighted:
RMSE3.8w <- evaluation_cont_w(model8, validatedata_test)
RMSE3.8w # 27.81803

#model 9 validation-test set weighted:
validatedata_test_temp9w <- validatedata_test + 1e-09
RMSE3.9w <- evaluation_cont_w(model9, validatedata_test_temp9w)
RMSE3.9w # 41.43257



############### VISUALIZATIONS ############### 
library("ggplot2")

# FUNCTION: CREATE VECTOR OF RESIDS
vectorize_resids <- function(model, data){
  sum_squared <- 0
  residuals <- rep(NA, nrow(data))
  for (i in 1:nrow(data)) {
    resid <- data$after_24hr_comments[i] - predict(model, data)[i]
    residuals[i] <- resid
  }
  return(residuals)
}
resids1 <- vectorize_resids(model1, validatedata_test)
resids2 <- vectorize_resids(model2, validatedata_test)
resids3 <- vectorize_resids(model4, validatedata_test)
resids4 <- vectorize_resids(model5, validatedata_test)
resids5 <- vectorize_resids(model6, validatedata_test_temp6w)
resids6 <- vectorize_resids(model7, validatedata_test)
resids7 <- vectorize_resids(model8, validatedata_test)
resids8 <- vectorize_resids(model9, validatedata_test_temp9w)
mean_resid1 <- mean(resids1)
mean_resid2 <- mean(resids2)
mean_resid3 <- mean(resids3)
mean_resid4 <- mean(resids4)
mean_resid5 <- mean(resids5)
mean_resid6 <- mean(resids6)
mean_resid7 <- mean(resids7)
mean_resid8 <- mean(resids8)

par(mfrow=c(2,4))   
plot(resids1, cex=0.2, ylim=c(-50,50), main="Model 1 Residuals", xlab="Instance in Validate Set", ylab="Residual Value")
legend("topleft", legend="mean residual", col="red", pch=19, cex = 0.6)
points(length(resids1)/2, mean_resid1, pch=19, cex=2, fill="red", col="red")
plot(resids2, cex=0.2, ylim=c(-50,50), main="Model 2 Residuals", xlab="Instance in Validate Set", ylab="Residual Value")
legend("topleft", legend="mean residual", col="red", pch=19, cex = 0.6)
points(length(resids2)/2, mean_resid2, pch=19, cex=2, fill="red", col="red")
plot(resids3, cex=0.2, ylim=c(-50,50), main="Model 3 Residuals", xlab="Instance in Validate Set", ylab="Residual Value")
legend("topleft", legend="mean residual", col="red", pch=19, cex = 0.6)
points(length(resids3)/2, mean_resid3, pch=19, cex=2, fill="red", col="red")
plot(resids4, cex=0.2, ylim=c(-50,50), main="Model 4 Residuals", xlab="Instance in Validate Set", ylab="Residual Value")
legend("topleft", legend="mean residual", col="red", pch=19, cex = 0.6)
points(length(resids4)/2, mean_resid4, pch=19, cex=2, fill="red", col="red")
plot(resids5, cex=0.2, ylim=c(-50,50), main="Model 5 Residuals", xlab="Instance in Validate Set", ylab="Residual Value")
legend("topleft", legend="mean residual", col="red", pch=19, cex = 0.6)
points(length(resids5)/2, mean_resid5, pch=19, cex=2, fill="red", col="red")
plot(resids6, cex=0.2, ylim=c(-50,50), main="Model 6 Residuals", xlab="Instance in Validate Set", ylab="Residual Value")
legend("topleft", legend="mean residual", col="red", pch=19, cex = 0.6)
points(length(resids6)/2, mean_resid6, pch=19, cex=2, fill="red", col="red")
plot(resids7, cex=0.2, ylim=c(-50,50), main="Model 7 Residuals", xlab="Instance in Validate Set", ylab="Residual Value")
legend("topleft", legend="mean residual", col="red", pch=19, cex = 0.6)
points(length(resids7)/2, mean_resid7, pch=19, cex=2, fill="red", col="red")
plot(resids8, cex=0.2, ylim=c(-50,50), main="Model 8 Residuals", xlab="Instance in Validate Set", ylab="Residual Value")
legend("topleft", legend="mean residual", col="red", pch=19, cex = 0.6)
points(length(resids8)/2, mean_resid8, pch=19, cex=2, fill="red", col="red")



r_squareds <- c(0.349, 0.340, 0.349, 0.299, 0.354, 0.349, 0.349, 0.414)
rmse_unweighted <- c(RMSE3.1, RMSE3.2, RMSE3.4, RMSE3.5, RMSE3.6, RMSE3.7, RMSE3.8, RMSE3.9)
rmse_weighted <- c(RMSE3.1w, RMSE3.2w, RMSE3.4w, RMSE3.5w, RMSE3.6w, RMSE3.7w, RMSE3.8w, RMSE3.9w)
rmse_unweighted
rmse_weighted
table(rmse_weighted)
table(rmse_unweighted)
models <- c(1,2,3,4,5,6,7,8)
RMSE.df <- data.frame(models, table(rmse_weighted), table(rmse_unweighted))
RMSE.df
RMSE.df1 <- data.matrix(rmse_weighted, rmse_unweighted)
RMSE.df1

par(mfrow=c(1,2))   
barplot(rmse_weighted, names.arg = models,col=rainbow(length(rmse_weighted)), ylim=c(20,45), 
        main="Weighted RMSE per Model 
(130% error for underestimates)", xlab="Model", ylab="Root Mean Squared Error")
barplot(rmse_unweighted, names.arg = models,col=rainbow(length(rmse_weighted)), ylim=c(20,45),
        main="Unweighted RMSE per Model", xlab="Model", ylab="Root Mean Squared Error")


par(mfrow=c(1,2))   
barplot(rmse_weighted, names.arg = models,col=rainbow(length(rmse_weighted)), ylim=c(25,45), 
        main="Weighted RMSE per Model 
(130% error for underestimates)", xlab="Model", ylab="Root Mean Squared Error")
barplot(r_squareds, names.arg = models,col=rainbow(length(rmse_weighted)), ylim=c(0.3,0.43),
        main="R-Squared per Model 
(On Training Set)", xlab="Model", ylab="R-Squared Value on Training Data")



# FUNCTION: CREATE VECTOR OF PREDICTEDS
vectorize_true_predicted <- function(model, data){
  sum_squared <- 0
  predicted <- rep(NA, nrow(data))
  for (i in 1:nrow(data)) {
    pred <- predict(model, data)[i]
    predicted[i] <- pred
  }
  return(predicted
}
head(validatedata_test[,281])
resids1 <- vectorize_resids(model1, validatedata_test)
resids2 <- vectorize_resids(model2, validatedata_test)
resids4 <- vectorize_resids(model4, validatedata_test)
resids5 <- vectorize_resids(model5, validatedata_test)
resids6 <- vectorize_resids(model6, validatedata_test_temp6w)
resids7 <- vectorize_resids(model7, validatedata_test)
resids8 <- vectorize_resids(model8, validatedata_test)
resids9 <- vectorize_resids(model9, validatedata_test_temp9w)
mean_resid1 <- mean(resids1)
mean_resid2 <- mean(resids2)
mean_resid4 <- mean(resids4)
mean_resid5 <- mean(resids5)
mean_resid6 <- mean(resids6)
mean_resid7 <- mean(resids7)
mean_resid8 <- mean(resids8)
mean_resid9 <- mean(resids9)
mean_resid9




