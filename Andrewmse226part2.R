### MS&E 226 Project Part 2

# import libraries
set.seed(2025)
install.packages("cvTools")
library(cvTools)
install.packages("glmnet", repos = "https://cran.us.r-project.org")
library(glmnet)
library(dplyr)


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

bestLambda <- 0.5
Xtrain2 = select(traindata, -c(after_24hr_comments)) %>% data.matrix()
Ytrain2 = traindata$after_24hr_comments
model2 <- glmnet(x = Xtrain2, y = Ytrain2, alpha = 1, lambda = bestLambda)
model2$dev.ratio #r2=0.3363457

#model 3: higher order terms on pre-24hr covariate measurements, degree=3
dropCols = c(11,12,13,14,15,16,17,18,19,20,52,57)
model3 <- lm(after_24hr_comments ~ I(avg_before_24hr_comments)^3 + 
               I(sd_before_24hr_comments)^3 + I(min_before_24hr_comments)^3 + 
               I(max_before_24hr_comments)^3 + I(med_before_24hr_comments)^3 + 
               I(avg_before_24hr_links)^3 + I(sd_before_24hr_links)^3 + 
               I(min_before_24hr_links)^3 + I(max_before_24hr_links)^3 + 
               I(med_before_24hr_links)^3 + I(before_24hr_comments)^3 + 
               I(before_24hr_links)^3 + ., data=traindata)
summary(model3) #r2=0.3489


#model 4: log transformation terms on pre-24hr covariate measurements
traindata_temp4 = traindata + 1e-09
model4 <- lm(data=traindata_temp4, log(after_24hr_comments) ~ log(avg_before_24hr_comments) + 
               log(sd_before_24hr_comments) + log(min_before_24hr_comments) + 
               log(max_before_24hr_comments) + log(med_before_24hr_comments) + 
               log(avg_before_24hr_links) + log(sd_before_24hr_links) + 
               log(min_before_24hr_links) + log(max_before_24hr_links) + 
               log(med_before_24hr_links) + log(before_24hr_comments) + 
               log(before_24hr_links))
summary(model4) #r2=0.3544

"#model 5: using centering
traindata_temp5 <- traindata
for (i in 1:280){
  traindata_temp5[,i] <- traindata_temp5[,i] - mean(traindata_temp5[,i])
}
model5 <- lm(after_24hr_comments ~ . , data=traindata_temp5)
summary(model5) #r2=0.3489, intercept=6.493"

# model 5: interaction terms:
model5 <- lm(after_24hr_comments ~ . + 
               parent_pages_count:max_parent_pages_comments +
               parent_pages_count:avg_parent_pages_comments +
               basetime_sun:before_24hr_links + 
               time_length:total_before_comments +
               time_length:before_24hr_comments +
               time_length:from_48hr_to_24hr_comments +
               time_length:first_24hr_comments +
               med_before_24hr_comments:before_24hr_comments +
               med_before_24hr_comments:from_48hr_to_24hr_comments +
               max_before_24hr_comments:before_24hr_comments +
               max_before_24hr_comments:from_48hr_to_24hr_comments +
               publication_sun:basetime_sun +
               avg_total_before_comments:avg_total_before_links, data=traindata)
summary(model5) #r2=0.3942

"#FAKE model 9: combination of modelX and modelY (all columns but add log of 24hr-pre-basetime)
dropCols = c(63:262)
traindata_temp9 = traindata + 1e-09
model9 <- lm(log(after_24hr_comments) ~ . + log(avg_before_24hr_comments) + 
               log(sd_before_24hr_comments) + log(min_before_24hr_comments) + 
               log(max_before_24hr_comments) + log(med_before_24hr_comments) + 
               log(avg_before_24hr_links) + log(sd_before_24hr_links) + 
               log(min_before_24hr_links) + log(max_before_24hr_links) + 
               log(med_before_24hr_links) + log(before_24hr_comments) + 
               log(before_24hr_links), data=traindata_temp9[-dropCols])
summary(model9) #r2=0.4135"


# LASSO model 6: lasso of transfornations n siht
bestLambda <- 2.8
traindata_temp7 <- traindata
traindata_temp7$avg_before_24hr_comments_3 = (traindata_temp7$avg_before_24hr_comments)^3
traindata_temp7$sd_before_24hr_comments_3 = (traindata_temp7$sd_before_24hr_comments)^3
traindata_temp7$min_before_24hr_comments_3 = (traindata_temp7$min_before_24hr_comments)^3
traindata_temp7$max_before_24hr_comments_3 = (traindata_temp7$max_before_24hr_comments)^3
traindata_temp7$med_before_24hr_comments_3 = (traindata_temp7$med_before_24hr_comments)^3
traindata_temp7$avg_before_24hr_links_3 = (traindata_temp7$avg_before_24hr_links)^3
traindata_temp7$sd_before_24hr_links_3 = (traindata_temp7$sd_before_24hr_links)^3
traindata_temp7$min_before_24hr_links_3 = (traindata_temp7$max_before_24hr_links)^3
traindata_temp7$med_before_24hr_links_3 = (traindata_temp7$med_before_24hr_links)^3
traindata_temp7$before_24hr_comments_3 = (traindata_temp7$before_24hr_comments)^3
traindata_temp7$before_24hr_links_3 = (traindata_temp7$before_24hr_links)^3
traindata_temp7$parent_pages_countmax_parent_pages_comments = traindata_temp7$parent_pages_count*traindata_temp7$max_parent_pages_comments
traindata_temp7$parent_pages_countavg_parent_pages_comments = traindata_temp7$parent_pages_count*traindata_temp7$avg_parent_pages_comments
traindata_temp7$basetime_sunbefore_24hr_links = traindata_temp7$basetime_sun*traindata_temp7$before_24hr_links
traindata_temp7$time_lengthtotal_before_comments = traindata_temp7$time_length*traindata_temp7$total_before_comments
traindata_temp7$time_lengthbefore_24hr_comments = traindata_temp7$time_length*traindata_temp7$before_24hr_comments
traindata_temp7$time_lengthfrom_48hr_to_24hr_comments = traindata_temp7$time_length*traindata_temp7$from_48hr_to_24hr_comments
traindata_temp7$time_lengthfirst_24hr_comments = traindata_temp7$time_length*traindata_temp7$first_24hr_comments
traindata_temp7$med_before_24hr_commentsbefore_24hr_comments = traindata_temp7$med_before_24hr_comments*traindata_temp7$before_24hr_comments
traindata_temp7$med_before_24hr_commentsfrom_48hr_to_24hr_comments = traindata_temp7$med_before_24hr_comments*traindata_temp7$from_48hr_to_24hr_comments
traindata_temp7$max_before_24hr_commentsbefore_24hr_comments = traindata_temp7$max_before_24hr_comments*traindata_temp7$before_24hr_comments
traindata_temp7$max_before_24hr_commentsfrom_48hr_to_24hr_comments = traindata_temp7$max_before_24hr_comments*traindata_temp7$from_48hr_to_24hr_comments
traindata_temp7$publication_sunbasetime_sun = traindata_temp7$publication_sun*traindata_temp7$basetime_sun
traindata_temp7$avg_total_before_commentsavg_total_before_links = traindata_temp7$avg_total_before_comments*traindata_temp7$avg_total_before_links

validatedata_test_temp7 <- validatedata_test
validatedata_test_temp7$avg_before_24hr_comments_3 = (validatedata_test_temp7$avg_before_24hr_comments)^3
validatedata_test_temp7$sd_before_24hr_comments_3 = (validatedata_test_temp7$sd_before_24hr_comments)^3
validatedata_test_temp7$min_before_24hr_comments_3 = (validatedata_test_temp7$min_before_24hr_comments)^3
validatedata_test_temp7$max_before_24hr_comments_3 = (validatedata_test_temp7$max_before_24hr_comments)^3
validatedata_test_temp7$med_before_24hr_comments_3 = (validatedata_test_temp7$med_before_24hr_comments)^3
validatedata_test_temp7$avg_before_24hr_links_3 = (validatedata_test_temp7$avg_before_24hr_links)^3
validatedata_test_temp7$sd_before_24hr_links_3 = (validatedata_test_temp7$sd_before_24hr_links)^3
validatedata_test_temp7$min_before_24hr_links_3 = (validatedata_test_temp7$max_before_24hr_links)^3
validatedata_test_temp7$med_before_24hr_links_3 = (validatedata_test_temp7$med_before_24hr_links)^3
validatedata_test_temp7$before_24hr_comments_3 = (validatedata_test_temp7$before_24hr_comments)^3
validatedata_test_temp7$before_24hr_links_3 = (validatedata_test_temp7$before_24hr_links)^3
validatedata_test_temp7$parent_pages_countmax_parent_pages_comments = validatedata_test_temp7$parent_pages_count*validatedata_test_temp7$max_parent_pages_comments
validatedata_test_temp7$parent_pages_countavg_parent_pages_comments = validatedata_test_temp7$parent_pages_count*validatedata_test_temp7$avg_parent_pages_comments
validatedata_test_temp7$basetime_sunbefore_24hr_links = validatedata_test_temp7$basetime_sun*validatedata_test_temp7$before_24hr_links
validatedata_test_temp7$time_lengthtotal_before_comments = validatedata_test_temp7$time_length*validatedata_test_temp7$total_before_comments
validatedata_test_temp7$time_lengthbefore_24hr_comments = validatedata_test_temp7$time_length*validatedata_test_temp7$before_24hr_comments
validatedata_test_temp7$time_lengthfrom_48hr_to_24hr_comments = validatedata_test_temp7$time_length*validatedata_test_temp7$from_48hr_to_24hr_comments
validatedata_test_temp7$time_lengthfirst_24hr_comments = validatedata_test_temp7$time_length*validatedata_test_temp7$first_24hr_comments
validatedata_test_temp7$med_before_24hr_commentsbefore_24hr_comments = validatedata_test_temp7$med_before_24hr_comments*validatedata_test_temp7$before_24hr_comments
validatedata_test_temp7$med_before_24hr_commentsfrom_48hr_to_24hr_comments = validatedata_test_temp7$med_before_24hr_comments*validatedata_test_temp7$from_48hr_to_24hr_comments
validatedata_test_temp7$max_before_24hr_commentsbefore_24hr_comments = validatedata_test_temp7$max_before_24hr_comments*validatedata_test_temp7$before_24hr_comments
validatedata_test_temp7$max_before_24hr_commentsfrom_48hr_to_24hr_comments = validatedata_test_temp7$max_before_24hr_comments*validatedata_test_temp7$from_48hr_to_24hr_comments
validatedata_test_temp7$publication_sunbasetime_sun = validatedata_test_temp7$publication_sun*validatedata_test_temp7$basetime_sun
validatedata_test_temp7$avg_total_before_commentsavg_total_before_links = validatedata_test_temp7$avg_total_before_comments*validatedata_test_temp7$avg_total_before_links


Xtrain = select(traindata_temp7, -c(after_24hr_comments)) %>% data.matrix()
Ytrain = traindata_temp7$after_24hr_comments
model6 <- glmnet(x = Xtrain, y = Ytrain, alpha = 1, lambda = bestLambda)
model6$dev.ratio #r2=0.3352306


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

#model 1 validation-test set:
RMSE3.1 <- evaluation_cont(model1, validatedata_test)
RMSE3.1 # 24.93532

#model 2 validation-test set:
Xtest2 = select(validatedata_test, -c(after_24hr_comments)) %>% data.matrix()
Ytest2 = validatedata_test$after_24hr_comments
predictions3 <- predict(model2, newx = Xtest2)
MSE3.2 <- mean((predictions3 - Ytest2)^2)
RMSE3.2 <- sqrt(MSE3.2)
RMSE3.2 # 24.41738

#model 3 validation-test set:
RMSE3.3 <- evaluation_cont(model3, validatedata_test)
RMSE3.3 # 24.93532

#model 4 validation-test set:
validatedata_test_temp4 <- validatedata_test + 1e-09
RMSE3.4 <- evaluation_cont(model4, validatedata_test_temp4)
RMSE3.4 # 36.98585

#model 5 validation-test set:
RMSE3.5 <- evaluation_cont(model5, validatedata_test)
RMSE3.5 # 34.30146

#model 6 validation-test set:
Xtest7 = select(validatedata_test_temp7, -c(after_24hr_comments)) %>% data.matrix()
Ytest7 = validatedata_test_temp7$after_24hr_comments
predictions7 <- predict(model7, newx = Xtest7)
MSE3.7 <- mean((predictions7 - Ytest7)^2)
RMSE3.6 <- sqrt(MSE3.7)
RMSE3.6 # 25.08764


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

#model 1 validation-test set weighted:
RMSE3.1w <- evaluation_cont_w(model1, validatedata_test)
RMSE3.1w # 27.83455

#model 2 validation-test set weighted:
Xtest2 = select(validatedata_test, -c(after_24hr_comments)) %>% data.matrix()
Ytest2 = validatedata_test$after_24hr_comments
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
RMSE3.2w # 27.77638

#model 3 validation-test set weighted:
RMSE3.3w <- evaluation_cont_w(model3, validatedata_test)
RMSE3.3w # 27.83456

#model 4 validation-test set weighted:
validatedata_test_temp4w <- validatedata_test + 1e-09
RMSE3.4w <- evaluation_cont_w(model4, validatedata_test_temp4w)
RMSE3.4w # 42.17036

#model 5 validation-test set weighted:
RMSE3.5w <- evaluation_cont_w(model5, validatedata_test)
RMSE3.5w # 36.57172

#model 6 validation-test set:
Xtest7 = select(validatedata_test_temp7, -c(after_24hr_comments)) %>% data.matrix()
Ytest7 = validatedata_test_temp7$after_24hr_comments
predictions7w <- predict(model7, newx = Xtest7)
residuals7w <- Ytest7 - predictions7w
sum_squared <- 0
for (i in 1:length(predictions7w)){
  if(predictions7w[i] > 0){
    sum_squared <- sum_squared + (1.3 * (predictions7w[i] - Ytest7[i])^2)
  }
  else{
    sum_squared <- sum_squared + (predictions7w[i] - Ytest7[i])^2
  }
}
RMSE3.6w <- sqrt(sum_squared / length(predictions7w))
RMSE3.6w # 28.59279



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
resids2 <- residuals2w
resids3 <- vectorize_resids(model3, validatedata_test)
resids4 <- vectorize_resids(model4, validatedata_test_temp4w)
resids5 <- vectorize_resids(model5, validatedata_test)
resids6 <- residuals7w
mean_resid1 <- mean(resids1)
mean_resid2 <- mean(resids2)
mean_resid3 <- mean(resids3)
mean_resid4 <- mean(resids4)
mean_resid5 <- mean(resids5)
mean_resid6 <- mean(resids6)

par(mfrow=c(2,3))   
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



r_squareds <- c(0.349, 0.336, 0.349, 0.354, 0.394, 0.335)
rmse_unweighted <- c(RMSE3.1, RMSE3.2, RMSE3.3, RMSE3.4, RMSE3.5, RMSE3.6)
rmse_weighted <- c(RMSE3.1w, RMSE3.2w, RMSE3.3w, RMSE3.4w, RMSE3.5w, RMSE3.6w)
rmse_unweighted
rmse_weighted
models <- c(1,2,3,4,5,6)
RMSE.df <- data.frame(models, table(rmse_weighted), table(rmse_unweighted))
RMSE.df
RMSE.df1 <- data.matrix(rmse_weighted, rmse_unweighted)
RMSE.df1
length(rmse_weighted)

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




