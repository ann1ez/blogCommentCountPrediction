### MS&E 226 Project Part 3

# import libraries
set.seed(2025)
#install.packages("cvTools")
library(cvTools)
install.packages("glmnet", repos = "https://cran.us.r-project.org")
library(glmnet)
library(readr)
library(stats)


# import data
setwd("/Users/andrewhong/Desktop/College/MS&E 226/Project")
train_data <- read.csv("blogData_train.csv")

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
      colnames(train_data)[index] <- paste(prefix[k], basic_attributes[i], suffix[j], sep = "_")
    }
  }
}
for (i in 1:5){
  for (j in 1:2) {
    index = 50 + i + 5 * (j-1)
    colnames(train_data)[index] <- paste(basic_attributes[i], suffix[j], sep = "_")
    colnames(test_set)[index] <- paste(basic_attributes[i], suffix[j], sep = "_")
  }
}
colnames(test_set)[61] <- "time_length"
colnames(train_data)[61] <- "time_length"
colnames(test_set)[62] <- "post_length"
colnames(train_data)[62] <- "post_length"
for (i in 1: 200){
  index = toString(i)
  colnames(test_set)[i + 62] <- paste("freq_word_feature_", index, sep = "")
  colnames(train_data)[i + 62] <- paste("freq_word_feature_", index, sep = "")
}
days <- c("mon", "tues", "wed", "thurs", "fri", "sat", "sun")
for (i in 1:7) {
  colnames(test_set)[i + 262] <- paste("basetime", days[i], sep = "_")
  colnames(train_data)[i + 262] <- paste("basetime", days[i], sep = "_")
}
for (i in 1:7) {
  colnames(test_set)[i + 269] <- paste("publication", days[i], sep = "_")
  colnames(train_data)[i + 269] <- paste("publication", days[i], sep = "_")
}
colnames(test_set)[277] <- "parent_pages_count"
colnames(test_set)[278] <- "min_parent_pages_comments"
colnames(test_set)[279] <- "max_parent_pages_comments"
colnames(test_set)[280] <- "avg_parent_pages_comments"
colnames(test_set)[281] <- "after_24hr_comments"
colnames(train_data)[277] <- "parent_pages_count"
colnames(train_data)[278] <- "min_parent_pages_comments"
colnames(train_data)[279] <- "max_parent_pages_comments"
colnames(train_data)[280] <- "avg_parent_pages_comments"
colnames(train_data)[281] <- "after_24hr_comments"
head(test_set)
head(train_data)

######################################################################

# model 2: taking away some covariates bag of 200 words
bestLambda <- 0.5
X2 = select(traindata, -c(after_24hr_comments)) %>% data.matrix()
Y2 = traindata$after_24hr_comments
model2 <- glmnet(x = X2, y = Y2, alpha = 1, lambda=bestLambda <- 0.5)
coef(model2)
model2$dev.ratio #r2=0.3354415 --> test=0.3398696

# trying to extract p-value per coefficient
p_values <- summary(model2)$lambda[which(model2$lambda == bestLambda)] * coef_lambda[, "s.e."] * sqrt(model2$df.null)
coef_lambda <- coef(model2, s = 0.5)
head(coef_lambda)
p_values <- coef_lambda[, "pvalue"]


### chatgpt "how to get pvalues"
# Extract the coefficient estimates and their standard errors
coef_est <- coef(model2, s = bestLambda)
coef_se <- coef(summary(model2))[,4]
se_coef <- sqrt(diag(vcov(model2, s = bestLambda)))/sqrt(nrow(X2))
# Calculate the t-values and p-values
t_values <- coef_est/coef_se
p_values <- 2*pt(-abs(t_values), df = model2$df.residual)
# Print the p-values
print(p_values)




"""> coef(model2)
281 x 1 sparse Matrix of class "dgCMatrix"
s0
(Intercept)                           5.003474e+00
avg_total_before_comments             .           
sd_total_before_comments              .           
min_total_before_comments             .           
max_total_before_comments             .           
med_total_before_comments             2.135071e-02
avg_total_before_links                8.641994e-02
sd_total_before_links                 1.329349e-05
min_total_before_links                .           
max_total_before_links                .           
med_total_before_links                1.893794e-01
avg_before_24hr_comments              .           
sd_before_24hr_comments               4.268987e-02
min_before_24hr_comments              .           
max_before_24hr_comments              .           
med_before_24hr_comments              .           
avg_before_24hr_links                 .           
sd_before_24hr_links                  .           
min_before_24hr_links                 .           
max_before_24hr_links                 .           
med_before_24hr_links                 .           
avg_from_48hr_to_24hr_comments        1.904090e+00
sd_from_48hr_to_24hr_comments         .           
min_from_48hr_to_24hr_comments        .           
max_from_48hr_to_24hr_comments        .           
med_from_48hr_to_24hr_comments        5.633866e-01
avg_from_48hr_to_24hr_links           .           
sd_from_48hr_to_24hr_links            .           
min_from_48hr_to_24hr_links           .           
max_from_48hr_to_24hr_links           .           
med_from_48hr_to_24hr_links           .           
avg_first_24hr_comments               .           
sd_first_24hr_comments                .           
min_first_24hr_comments               .           
max_first_24hr_comments               .           
med_first_24hr_comments               .           
avg_first_24hr_links                  .           
sd_first_24hr_links                   .           
min_first_24hr_links                  .           
max_first_24hr_links                  .           
med_first_24hr_links                  .           
avg_change_between_two_days_comments  .           
sd_change_between_two_days_comments   .           
min_change_between_two_days_comments  .           
max_change_between_two_days_comments  .           
med_change_between_two_days_comments  .           
avg_change_between_two_days_links     .           
sd_change_between_two_days_links      .           
min_change_between_two_days_links     .           
max_change_between_two_days_links     .           
med_change_between_two_days_links     .           
total_before_comments                -7.547240e-04
before_24hr_comments                  8.317145e-02
from_48hr_to_24hr_comments            .           
first_24hr_comments                  -2.663214e-02
change_between_two_days_comments      7.944931e-02
total_before_links                    .           
before_24hr_links                     2.288211e-01
from_48hr_to_24hr_links               .           
first_24hr_links                      .           
change_between_two_days_links         .           
time_length                          -1.544445e-01
post_length                           .           
freq_word_feature_1                   .           
freq_word_feature_2                   .           
freq_word_feature_3                   .           
freq_word_feature_4                   .           
freq_word_feature_5                   .           
freq_word_feature_6                   .           
freq_word_feature_7                   8.584102e-01
freq_word_feature_8                   .           
freq_word_feature_9                   .           
freq_word_feature_10                  .           
freq_word_feature_11                  .           
freq_word_feature_12                  .           
freq_word_feature_13                  .           
freq_word_feature_14                  .           
freq_word_feature_15                  2.596480e+00
freq_word_feature_16                  .           
freq_word_feature_17                  .           
freq_word_feature_18                  .           
freq_word_feature_19                  .           
freq_word_feature_20                  .           
freq_word_feature_21                  .           
freq_word_feature_22                  .           
freq_word_feature_23                  .           
freq_word_feature_24                  .           
freq_word_feature_25                  .           
freq_word_feature_26                  .           
freq_word_feature_27                  .           
freq_word_feature_28                  .           
freq_word_feature_29                  .           
freq_word_feature_30                  .           
freq_word_feature_31                  .           
freq_word_feature_32                  .           
freq_word_feature_33                  .           
freq_word_feature_34                  .           
freq_word_feature_35                  .           
freq_word_feature_36                  .           
freq_word_feature_37                  .           
freq_word_feature_38                  .           
freq_word_feature_39                  .           
freq_word_feature_40                  .           
freq_word_feature_41                  .           
freq_word_feature_42                  .           
freq_word_feature_43                  .           
freq_word_feature_44                  .           
freq_word_feature_45                  .           
freq_word_feature_46                  .           
freq_word_feature_47                  .           
freq_word_feature_48                  .           
freq_word_feature_49                  .           
freq_word_feature_50                  .           
freq_word_feature_51                  .           
freq_word_feature_52                  .           
freq_word_feature_53                  .           
freq_word_feature_54                  .           
freq_word_feature_55                  .           
freq_word_feature_56                  .           
freq_word_feature_57                  .           
freq_word_feature_58                  .           
freq_word_feature_59                  .           
freq_word_feature_60                  .           
freq_word_feature_61                  .           
freq_word_feature_62                  .           
freq_word_feature_63                  .           
freq_word_feature_64                  .           
freq_word_feature_65                  .           
freq_word_feature_66                  .           
freq_word_feature_67                  .           
freq_word_feature_68                  .           
freq_word_feature_69                  .           
freq_word_feature_70                  .           
freq_word_feature_71                  .           
freq_word_feature_72                  .           
freq_word_feature_73                  .           
freq_word_feature_74                  .           
freq_word_feature_75                  1.774653e+00
freq_word_feature_76                  .           
freq_word_feature_77                  .           
freq_word_feature_78                  .           
freq_word_feature_79                  .           
freq_word_feature_80                  .           
freq_word_feature_81                  .           
freq_word_feature_82                  .           
freq_word_feature_83                  .           
freq_word_feature_84                  .           
freq_word_feature_85                 -4.790082e-01
freq_word_feature_86                  .           
freq_word_feature_87                  .           
freq_word_feature_88                  .           
freq_word_feature_89                  .           
freq_word_feature_90                  .           
freq_word_feature_91                  .           
freq_word_feature_92                  7.477337e-01
freq_word_feature_93                  .           
freq_word_feature_94                  .           
freq_word_feature_95                  .           
freq_word_feature_96                  .           
freq_word_feature_97                  .           
freq_word_feature_98                  .           
freq_word_feature_99                  .           
freq_word_feature_100                 .           
freq_word_feature_101                 .           
freq_word_feature_102                 .           
freq_word_feature_103                 .           
freq_word_feature_104                 .           
freq_word_feature_105                 .           
freq_word_feature_106                 .           
freq_word_feature_107                 .           
freq_word_feature_108                 .           
freq_word_feature_109                 2.551869e+01
freq_word_feature_110                 .           
freq_word_feature_111                 .           
freq_word_feature_112                 .           
freq_word_feature_113                 .           
freq_word_feature_114                 .           
freq_word_feature_115                 .           
freq_word_feature_116                 .           
freq_word_feature_117                 .           
freq_word_feature_118                 .           
freq_word_feature_119                 .           
freq_word_feature_120                 .           
freq_word_feature_121                 .           
freq_word_feature_122                 .           
freq_word_feature_123                 .           
freq_word_feature_124                 .           
freq_word_feature_125                 .           
freq_word_feature_126                 .           
freq_word_feature_127                 .           
freq_word_feature_128                 .           
freq_word_feature_129                 .           
freq_word_feature_130                 .           
freq_word_feature_131                 .           
freq_word_feature_132                 .           
freq_word_feature_133                 .           
freq_word_feature_134                 .           
freq_word_feature_135                 .           
freq_word_feature_136                 .           
freq_word_feature_137                 .           
freq_word_feature_138                 .           
freq_word_feature_139                 .           
freq_word_feature_140                 .           
freq_word_feature_141                 .           
freq_word_feature_142                 .           
freq_word_feature_143                 .           
freq_word_feature_144                 .           
freq_word_feature_145                 .           
freq_word_feature_146                 .           
freq_word_feature_147                 .           
freq_word_feature_148                 .           
freq_word_feature_149                 .           
freq_word_feature_150                 .           
freq_word_feature_151                 .           
freq_word_feature_152                 .           
freq_word_feature_153                 .           
freq_word_feature_154                 .           
freq_word_feature_155                -8.889213e+00
freq_word_feature_156                 .           
freq_word_feature_157                 .           
freq_word_feature_158                 .           
freq_word_feature_159                 .           
freq_word_feature_160                 .           
freq_word_feature_161                 .           
freq_word_feature_162                 .           
freq_word_feature_163                 .           
freq_word_feature_164                 6.533699e-01
freq_word_feature_165                 .           
freq_word_feature_166                 .           
freq_word_feature_167                 .           
freq_word_feature_168                 2.926835e-02
freq_word_feature_169                 .           
freq_word_feature_170                 .           
freq_word_feature_171                 .           
freq_word_feature_172                 .           
freq_word_feature_173                 .           
freq_word_feature_174                 .           
freq_word_feature_175                 .           
freq_word_feature_176                 .           
freq_word_feature_177                 .           
freq_word_feature_178                 .           
freq_word_feature_179                 .           
freq_word_feature_180                 .           
freq_word_feature_181                 .           
freq_word_feature_182                 .           
freq_word_feature_183                 .           
freq_word_feature_184                 .           
freq_word_feature_185                 .           
freq_word_feature_186                 4.773366e-02
freq_word_feature_187                 .           
freq_word_feature_188                 .           
freq_word_feature_189                 .           
freq_word_feature_190                 .           
freq_word_feature_191                 .           
freq_word_feature_192                 .           
freq_word_feature_193                 .           
freq_word_feature_194                 .           
freq_word_feature_195                 .           
freq_word_feature_196                 .           
freq_word_feature_197                 .           
freq_word_feature_198                 .           
freq_word_feature_199                 4.367142e-02
freq_word_feature_200                 .           
basetime_mon                          .           
basetime_tues                         .           
basetime_wed                          .           
basetime_thurs                        .           
basetime_fri                          .           
basetime_sat                          .           
basetime_sun                          .           
publication_mon                       .           
publication_tues                      .           
publication_wed                       .           
publication_thurs                     .           
publication_fri                       .           
publication_sat                       .           
publication_sun                       .           
parent_pages_count                    .           
min_parent_pages_comments             .           
max_parent_pages_comments             .           
avg_parent_pages_comments             . """

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
(130% error for underestimates)", xlab="Type", ylab="Root Mean Squared Error (w)")
barplot(c(RMSE3.2, RMSE3.2_val), names.arg = c("Actual","Predicted"),col=rainbow(2), ylim=c(20,30),
        main="Unweighted Test Set RMSE", xlab="Type", ylab="Root Mean Squared Error")





