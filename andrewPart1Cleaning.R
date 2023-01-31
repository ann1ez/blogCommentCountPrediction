### MS&E 226 Project Part 1: Andrew's Cleaning

## cutting out 21,901 random rows in training set to get 80:20 train:test ratio of rows/instances
set.seed(2)
rowsTrainSet <- nrow(data) # 52,396 rows in training set
rowsTestSet <- 7624 # rows in testing set
# 7624 / 0.25 = 30495 training set rows needed given number of test set rows, so randomly throw out 21901 training set rows
randomRows <- sample(1:rowsTrainSet, 21901, replace = F)
length(unique(randomRows)) # check
data2 <- data[-randomRows,]
nrow(data2)

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
