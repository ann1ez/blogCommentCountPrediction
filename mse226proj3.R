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

# model 2: taking away some covariates bag of 200 words
bestLambda <- 0.5
X2 = select(traindata, -c(after_24hr_comments)) %>% data.matrix()
Y2 = traindata$after_24hr_comments
model2 <- glmnet(x = X2, y = Y2, alpha = 1, lambda=bestLambda)
#coef(model2)
model2$dev.ratio #r2=0.3354415 --> test=0.3398696

########################################################################

## P-VALUE PER COEFFICIENT FINDING
coefs <- rep(NA, 281)
coefs_ones <- rep(NA, 281)
for (i in 1:281) {
  coefs[i] <- coef(model2)[i]
  if(coef(model2)[i] != 0){
    coefs_binary[i] <- i
  }
  else{
    coefs_binary[i] <- NA
  }
}
coefs
coefs_binary

"""X_lm <- train_data[,-281]
Y_lm <- train_data[,281]
traindata_new <- train_data[, c(1,6,7,8,11,22,26,52,53,55,56,58,62,70,78,155,159,172,218,227,281)]
traindata_new <- train_data[, c(0,5,6,7,10,21,25,51,52,54,55,57,61,69,77,154,158,171,217,226,281)]
head(traindata_new)
model2_lm <- lm(after_24hr_comments ~ . , data=traindata_new)
summary(model2_lm)

# trying to extract p-value per coefficient
p_values <- summary(model2)$lambda[which(model2$lambda == bestLambda)] * coef_lambda[, "s.e."] * sqrt(model2$df.null)
coef_lambda <- coef(model2, s = 0.5)
(coef_lambda)
p_values <- coef_lambda[, "pvalue"]

# 4.773366e-02 vs -3.226e-01
### chatgpt "how to get pvalues"
# Extract the coefficient estimates and their standard errors
coef_est <- coef(model2, s = bestLambda)
coef_se <- coef(summary(model2))[,4]
se_coef <- sqrt(diag(vcov(model2, s = bestLambda)))/sqrt(nrow(X2))
# Calculate the t-values and p-values
t_values <- coef_est/coef_se
p_values <- 2*pt(-abs(t_values), df = model2$df.residual)
# Print the p-values
print(p_values)"""

#########################################################################################

####################################################################################

### BOOTSTRAPPING: from lecture notes
df = data.frame(X2,Y2) 
coef.boot = function(X, Y, indices) {
  fm = glmnet(x = X, y = Y, alpha = 1, lambda=bestLambda)
  #fm = glmnet(data = data[indices,], alpha = 1, lambda=bestLambda)
  #fm = lm(data = data[indices,], Y ~ 1 + X)
  return(coef(fm)) 
} 
boot.out = boot(df, coef.boot, 50)
boot.out


## BOOTSTRAPPING: from gpt

# Generate some sample data
set.seed(123)
n <- 100
p <- 20
X <- matrix(rnorm(n*p), ncol=p)
beta <- rnorm(p, 0, 1)
y <- X %*% beta + rnorm(n)

# Define a function to fit the glmnet lasso model
glmnet_fit <- function(data, indices) {
  x <- data$X[indices, ]
  y <- data$y[indices]
  fit <- glmnet(x, y, alpha = 1) # alpha = 1 for lasso
  return(fit)
}

# Run a bootstrap with 1000 replications
boot_results <- boot(data = list(X = X2, y = Y2), statistic = glmnet_fit, R = 50000)

# Calculate the bootstrap standard errors for the coefficients
boot_se <- apply(boot_results$t, 2, sd)

# Extract the coefficients from the original fit
coef_original <- coef(fit_original)[-1] # exclude the intercept

# Calculate the bootstrap confidence intervals for the coefficients
boot_ci <- t(sapply(1:length(coef_original), function(i) quantile(boot_results$t[,i], c(0.025, 0.975))))

# Combine the coefficients, standard errors, and confidence intervals into a data frame
results_df <- data.frame(Coefficient = names(coef_original),
                         Estimate = coef_original,
                         Std_Error = boot_se,
                         Lower_CI = boot_ci[,1],
                         Upper_CI = boot_ci[,2])

# Print the results
print(results_df)

####################################################################################

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

#model 2 rmse test set:
Xtest2 = select(test_set, -c(after_24hr_comments)) %>% data.matrix()
Ytest2 = test_set$after_24hr_comments
predictions3 <- predict(model2, newx = Xtest2)
MSE3.2 <- mean((predictions3 - Ytest2)^2)
RMSE3.2 <- sqrt(MSE3.2)
RMSE3.2 # val=24.41738 --> test=24.51266
RMSE3.2_val <- 24.41738
(RMSE3.2 - RMSE3.2_val) / RMSE3.2_val # 0.003902221

#model 2 rmse test set weighted:
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

# visualize RMSE's
par(mfrow=c(1,2))   
barplot(c(RMSE3.2w, RMSE3.2w_val), names.arg = c("Actual","Predicted"),col=rainbow(2), ylim=c(20,30), 
        main="Weighted Test Set RMSE 
(130% error for underestimates)", xlab="", ylab="Root Mean Squared Error (w)")
barplot(c(RMSE3.2, RMSE3.2_val), names.arg = c("Actual","Predicted"),col=rainbow(2), ylim=c(20,30),
        main="Unweighted Test Set RMSE", xlab="", ylab="Root Mean Squared Error")





