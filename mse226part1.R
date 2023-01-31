### MS&E 226 Project Part 1


# loading in data + adding colnames
setwd("/Users/andrewhong/Desktop/College/MS&E 226/Project")
data <- read.csv("blogData_train.csv")
basic_attributes <- c("total_before", "before_24hr", "from_48hr_to_24hr", "first_24hr", "change_between_two_days")
prefix <- c("avg", "sd", "min", "max", "med")
suffix <- c("comments", "links")
for (i in 1:5){
  for (j in 1:2) {
    for (k in 1:5) {
      index = 10 * (i-1) + 5 * (j-1) + k
      colnames(data)[index] <- paste(prefix[k], basic_attributes[i], suffix[j], sep = "_")
    }
  }
}
for (i in 1:5){
  for (j in 1:2) {
    index = 50 + i + 5 * (j-1)
    colnames(data)[index] <- paste(basic_attributes[i], suffix[j], sep = "_")
  }
}
colnames(data)[61] <- "time_length"
colnames(data)[62] <- "post_length"
for (i in 1: 200){
  index = toString(i)
  colnames(data)[i + 62] <- paste("freq_word_feature_", index, sep = "")
}
days <- c("mon", "tues", "wed", "thurs", "fri", "sat", "sun")
for (i in 1:7) {
  colnames(data)[i + 262] <- paste("basetime", days[i], sep = "_")
}
for (i in 1:7) {
  colnames(data)[i + 269] <- paste("publication", days[i], sep = "_")
}
colnames(data)[277] <- "parent_pages_count"
colnames(data)[278] <- "min_parent_pages_comments"
colnames(data)[279] <- "max_parent_pages_comments"
colnames(data)[280] <- "avg_parent_pages_comments"
colnames(data)[281] <- "after_24hr_comments"
print(data)

# ANNIE: Next I also want to count the number of rows cumulatively across the 60 different test datasets to help determine if we want to change the train-test split.

test_count <- 0
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
}

# ANNIE: This means just from the way the data set was split from the repository, we would have 7624 rows for testing and 52397 rows for training. This creates about a 87\% \/ 13\% split between the training and testing data respectively, but typically a 80\% \/ 20\% split is used. Since we have 60021 instances in total, it would make sense for us to try to balance out the split so that there is more testing data, so later we can have more confidence in our generalized error calculation. The easiest way for us to achieve that 80\% \/ 20\% split is for us to randomly select 21901 rows from the initial training dataset to NOT include in our analysis. That way we would have 7624 rows for testing, 30496 rows for training, and 38120 instances in total. 
# ANNIE: With this new split and removal of some training data, we are still well above the minimum requirement for number of rows, and we do not have to worry at all about whether the train \/ test split is getting messed up by fact that some of this data relies on time covariates (i.e. 24 hrs before, 48hrs before) because we are not transitioning training data into testing data.

## cutting out 21,901 random rows in training set to get 80:20 train:test ratio of rows/instances
set.seed(2)
rowsTrainSet <- nrow(data) # 52,396 rows in training set
rowsTestSet <- 7624 # rows in testing set
# 7624 / 0.25 = 30495 training set rows needed given number of test set rows, so randomly throw out 21901 training set rows
randomRows <- sample(1:rowsTrainSet, 21901, replace = F)
length(unique(randomRows)) # check
data2 <- data[-randomRows,]
nrow(data2)


## andrew exploratory analysis

# random regressions
#- parents vs target

# random regressions w/interactions

# 200 word analysis: binary var + regression

# transform -1's --> 1's in 200 word binary variables
for (i in 63:262) { 
  tempVec <- data2[,i]
  for (j in 1:length(tempVec)) {
    if(tempVec[j] == -1){
      tempVec[j] <- 1
    }
  }
}

# create new continuous var: sum of 200-words occurrences
data2$sum_occurences_freq_word <- rep(0, nrow(data2))
for (column in 63:262) {
  for (row in 1:nrow(data2)) {
    if (is.na(data2[row, column])){
      data2[row, column] <- 0
    }
    data2$sum_occurences_freq_word[row] <- data2$sum_occurences_freq_word[row] + data2[row, column]
  }
}
max(data2$sum_occurences_freq_word)

# create new binary var: occurrence of 200 words or not
data2$occurence_freq_word <- rep(0, nrow(data2))
for (i in 1:nrow(data2)) {
  if (data2$sum_occurences_freq_word[i] > 0){
    data2$occurence_freq_word[i] <- 1
  }
}
sum(data2$occurence_freq_word) # check

# create new binary variable: publication on weekend or not?
data2$publication_weekend <- rep(0, nrow(data2))
for (i in 1:nrow(data2)) {
  if (data2$publication_sat[i] > 0 | data2$publication_sat[i] > 0){
    data2$publication_weekend[i] <- 1
  }
}
sum(data2$publication_weekend)

# create new binary variable: basetime on weekend or not?
data2$basetime_weekend <- rep(0, nrow(data2))
for (i in 1:nrow(data2)) {
  if (data2$basetime_sat[i] > 0 | data2$basetime_sun[i] > 0){
    data2$basetime_weekend[i] <- 1
  }
}
sum(data2$basetime_weekend)

# regression: occurrence_freq_word (binary) x target/# comments 24hr after basetime
reg1 <- lm(data2$after_24hr_comments ~ 1 + data2$occurence_freq_word) # 4.042 + 3.690b, r2=0.002
summary(reg1)
plot(data2$occurence_freq_word, data2$after_24hr_comments, main="Existence of 1+ Frequent 200 Words v.s. 
     Comments 24hr Post-Basetime", xlab="Existence of 1+ 200 Frequent Words", ylab="Comments 24hr Post-Basetime")
abline(reg1$coefficients[1], reg1$coefficients[2], col="red", lwd=3)


# regression: sum_occurrence_freq_word (continuous) x target/# comments 24hr after basetime
reg2 <- lm(data2$after_24hr_comments ~ 1 + data2$sum_occurences_freq_word) # 3.605 + 0.320b, r2=0.006
summary(reg2)
plot(data2$sum_occurences_freq_word, data2$after_24hr_comments, main="Sum of Prevelance of Frequent 200 Words 
     v.s. Comments 24hr Post-Basetime", xlab="Sum of Prevelance of 1+ 200 Frequent Words", ylab="Comments 24hr Post-Basetime")
abline(reg2$coefficients[1], reg2$coefficients[2], col="red", lwd=3)


# regression: parent_pages_count x target/# comments 24hr after basetime
reg3 <- lm(data2$after_24hr_comments ~ 1 + data2$parent_pages_count) # 6.7457 = 0.2897b, r2=4.494e-05
summary(reg3)
plot(data2$sum_occurences_freq_word, data2$after_24hr_comments, main="No. Parent Pages vs 
     Comments 24hr Post-Basetime", xlab="Number of Linked Parent Pages", ylab="Comments 24hr Post-Basetime")
abline(reg3$coefficients[1], reg3$coefficients[2], col="red", lwd=3)


# regression: max_parent_pages_comments x target/# comments 24hr after basetime
reg4 <- lm(data2$after_24hr_comments ~ 1 + data2$max_parent_pages_comments) # 6.717 - 0.002b, r2=3.481e-06
summary(reg4)
plot(data2$max_parent_pages_comments, data2$after_24hr_comments, main="Max No. Comments on a Parent Pages vs 
     Comments 24hr Post-Basetime", xlab="Max Comment on a Linked Parent Page", ylab="Comments 24hr Post-Basetime")
abline(reg4$coefficients[1], reg4$coefficients[2], col="red", lwd=3)


# regression w/higher order term: max_parent_pages_comments x target/# comments 24hr after basetime
reg5 <- lm(data2$after_24hr_comments ~ 1 + data2$max_parent_pages_comments + I(data2$max_parent_pages_comments^2)) # 6.718 - 3.277e-03b + 7.638e-07b^2, r2=3.576e-06
summary(reg5)
plot(data2$max_parent_pages_comments, data2$after_24hr_comments, main="Max No. Comments on a Parent Pages vs 
     Comments 24hr Post-Basetime", xlab="Max Comment on a Linked Parent Page", ylab="Comments 24hr Post-Basetime")
abline(reg5$coefficients[1], reg5$coefficients[2] + reg5$coefficients[3]^2, col="red", lwd=3)



# regression: (publication_weekend) x target/# comments 24hr after basetime
reg6.1 <- lm(data2$after_24hr_comments ~ 1 + data2$publication_weekend) #r2=3.247e-05
summary(reg6.1)
plot(data2$publication_weekend, data2$after_24hr_comments)

# regression w/interaction: (basetime_weekend) x target/# comments 24hr after basetime
reg6.2 <- lm(data2$after_24hr_comments ~ 1 + data2$basetime_weekend) #r2=2.999e-05
summary(reg6.2)
plot(data2$publication_weekend, data2$after_24hr_comments)

# regression w/interaction: (publication_weekend & basetime_weekend) x target/# comments 24hr after basetime
reg6 <- lm(data2$after_24hr_comments ~ 1 + data2$publication_weekend + data2$basetime_weekend 
           + data2$basetime_weekend:data2$publication_weekend) #r2=0.003, 7.274 - 4.689b1 - 2.137b2 + 16.356b1:b2
summary(reg6)
plot(data2$basetime_weekend, data2$after_24hr_comments, col = data2$publication_weekend+5, cex=0.9, pch=16,
     main="Basetime Day of Week vs 
     Comments 24hr Post-Basetime", xlab="Weekday (0) vs Weekend (1) of Basetime", ylab="Comments 24hr Post-Basetime")
abline(reg6$coefficients[1], reg6$coefficients[3], col=5, lwd=2)
abline(reg6$coefficients[1] + reg6$coefficients[2], reg6$coefficients[2] + reg6$coefficients[4], col=6, lwd=2)
legend("topright", title = "Publication Day", legend=c("weekday", "weekend"), col=unique(data2$publication_weekend+5), lwd = 4, cex = 0.8)

plot(data2$publication_weekend, data2$after_24hr_comments, col = data2$basetime_weekend+5, cex=0.9, pch=16, ylim=c(0,10),
     main="No. Comments 24hr Pre-Basetime vs 
     Comments 24hr Post-Basetime", xlab="Weekday (0) vs Weekend (1) of Publication", ylab="Comments 24hr Post-Basetime")
abline(reg6$coefficients[1], reg6$coefficients[2], col="red", lwd=3)
legend("topright", title = "Basetime Day", legend=c("weekday", "weekend"), col=unique(data2$basetime_weekend+5), lwd = 4, cex = 0.8)



# regression w/3 terms: (publication_weekend & #comments24hrbeforebasetime) x target/# comments 24hr after basetime
reg8 <- lm(data2$after_24hr_comments ~ 1 + data2$publication_weekend + data2$before_24hr_comments) # 1.930 + 1.245b1 + 0.308b2, r2=0.213
summary(reg8)
plot(data2$publication_weekend, data2$before_24hr_comments)



# regression :  #comments24hrbeforebasetime x target/# comments 24hr after basetime
reg9 <- lm(data2$after_24hr_comments ~ 1 + data2$before_24hr_comments) # 2.047 + 0.310b, r2=0.213
summary(reg9)
plot(data2$before_24hr_comments, data2$after_24hr_comments, main="No. Comments 24hr Pre-Basetime vs 
     Comments 24hr Post-Basetime", xlab="Comments 24hr Post-Basetime", ylab="Comments 24hr Post-Basetime")
abline(reg9$coefficients[1], reg9$coefficients[2], col="red", lwd=3)

# regression :  #comments24hrbeforebasetime x target/# comments 24hr after basetime
reg9.1 <- lm(data2$after_24hr_comments ~ 1 + data2$before_24hr_comments 
           + I(data2$before_24hr_comments^2)) # 2.047 + 0.310b, r2=0.213
summary(reg9.1)
plot(data2$before_24hr_comments, data2$after_24hr_comments, main="No. Comments 24hr Pre-Basetime vs 
     Comments 24hr Post-Basetime 
     (quadratic, higher order term)", xlab="Comments 24hr Post-Basetime", ylab="Comments 24hr Post-Basetime")
abline(reg9.1$coefficients[1], reg9$coefficients[2] + reg9$coefficients[2]^2, col="red", lwd=3)



### break
# regression w/interaction: (timeBtwnPostandBasetime & #comments24hrsAfterPosting) x target/# comments 24hr after basetime
reg7 <- lm(data2$after_24hr_comments ~ 1 + data2$time_length + data2$first_24hr_comments)  
           #+ data2$time_length:data2$first_24hr_comments) # 7.670 -0.160b1 + 0.325b2 - 0.005b1:b2 r2=0.173
summary(reg7)
plot(data2$publication_weekend, data2$after_24hr_comments)

reg10 <- lm(data2$after_24hr_comments ~ 1 + data2$time_length) # 16.326 - 0.276b, r2=0.022
summary(reg10)
plot(data2$time_length, data2$after_24hr_comments)

reg11 <- lm(data2$after_24hr_comments ~ 1 + data2$first_24hr_comments) # 2.440 + 0.123b, r2=0.093
summary(reg11)
plot(data2$first_24hr_comments, data2$after_24hr_comments)
### break

"after_24hr_comments <- rep(0, length(data2$avg_total_before_comments))
publication_weekend <- rep(0, length(data2$avg_total_before_comments))
for(i in 1:length(data2$publication_weekend)){
  after_24hr_comments[i] = data2$after_24hr_comments[i] + 1
  publication_weekend[i] = data2$publication_weekend[i] + 1
}
head(after_24hr_comments)
head(publication_weekend)

reg10 <- lm(log(after_24hr_comments) ~ 1 + log(publication_weekend))
summary(reg10)
plot(log(publication_weekend), log(after_24hr_comments))"

# regression w/higher order term: (TODO: choose wtvr works w quadratic) x target/# comments 24hr after basetime




data2$max_parent_pages_comments











