# first thing to do is: importing test and train dataframes.
# make sure that all datatypes are correct, most of them are factors. 
# according to the student.names.txt document

setwd("~/Desktop/R/Assignment_6")

library(readr)
test <- read_csv("predict.students.data.csv", 
          col_types = cols(Activities = col_factor(levels = c("yes", "no")), 
                           Address = col_factor(levels = c("U","R")), 
                           Choice_of_school = col_factor(levels = c("home", "reputation", "course", "other")), 
                           Cohab_status = col_factor(levels = c("T", "A")), 
                           Day_alcohol = col_factor(levels = c("1", "2", "3", "4", "5")), 
                           Educ_support = col_factor(levels = c("yes", "no")), 
                           Education_father = col_factor(levels = c("0","1", "2", "3", "4")), 
                           Education_mother = col_factor(levels = c("0","1", "2", "3", "4")), 
                           Employment_father = col_factor(levels = c("teacher", "health", "services", "at_home", "other")), 
                           Employment_mother = col_factor(levels = c("teacher", "health", "services", "at_home", "other")), 
                           Failures = col_factor(levels = c("0", "1", "2", "3", "4")), 
                           Family_educ_support = col_factor(levels = c("yes", "no")), 
                           Family_relationships = col_factor(levels = c("1", "2", "3", "4", "5")), 
                           Family_size = col_factor(levels = c("LE3", "GT3")), 
                           Freetime = col_factor(levels = c("1", "2", "3", "4", "5")), 
                           Gender = col_factor(levels = c("F", "M")), 
                           Go_out = col_factor(levels = c("1", "2", "3", "4", "5")), 
                           Guardian = col_factor(levels = c("mother", "father", "other")), 
                           Health = col_factor(levels = c("1", "2", "3", "4", "5")), 
                           Higher = col_factor(levels = c("yes", "no")), 
                           Internet = col_factor(levels = c("yes", "no")), 
                           Nursery = col_factor(levels = c("yes", "no")), 
                           Paid = col_factor(levels = c("yes", "no")), 
                           Primary_school = col_factor(levels = c("JH", "KD")), 
                           Romantic = col_factor(levels = c("yes", "no")), 
                           Studytime = col_factor(levels = c("1", "2", "3", "4")), 
                           Traveltime = col_factor(levels = c("1", "2", "3", "4")), 
                           Weekend_alcohol = col_factor(levels = c("1", "2", "3", "4", "5"))))
View(test) 

library(readr)
train <- read_csv("students.data.csv", 
                 col_types = cols(Activities = col_factor(levels = c("yes", "no")), 
                                  Address = col_factor(levels = c("U","R")), 
                                  Choice_of_school = col_factor(levels = c("home", "reputation", "course", "other")), 
                                  Cohab_status = col_factor(levels = c("T", "A")), 
                                  Day_alcohol = col_factor(levels = c("1", "2", "3", "4", "5")), 
                                  Educ_support = col_factor(levels = c("yes", "no")), 
                                  Education_father = col_factor(levels = c("0","1", "2", "3", "4")), 
                                  Education_mother = col_factor(levels = c("0","1", "2", "3", "4")), 
                                  Employment_father = col_factor(levels = c("teacher", "health", "services", "at_home", "other")), 
                                  Employment_mother = col_factor(levels = c("teacher", "health", "services", "at_home", "other")), 
                                  Failures = col_factor(levels = c("0", "1", "2", "3", "4")), 
                                  Family_educ_support = col_factor(levels = c("yes", "no")), 
                                  Family_relationships = col_factor(levels = c("1", "2", "3", "4", "5")), 
                                  Family_size = col_factor(levels = c("LE3", "GT3")), 
                                  Freetime = col_factor(levels = c("1", "2", "3", "4", "5")), 
                                  Gender = col_factor(levels = c("F", "M")), 
                                  Go_out = col_factor(levels = c("1", "2", "3", "4", "5")), 
                                  Guardian = col_factor(levels = c("mother", "father", "other")), 
                                  Health = col_factor(levels = c("1", "2", "3", "4", "5")), 
                                  Higher = col_factor(levels = c("yes", "no")), 
                                  Internet = col_factor(levels = c("yes", "no")), 
                                  Nursery = col_factor(levels = c("yes", "no")), 
                                  Paid = col_factor(levels = c("yes", "no")), 
                                  Primary_school = col_factor(levels = c("JH", "KD")), 
                                  Romantic = col_factor(levels = c("yes", "no")), 
                                  Studytime = col_factor(levels = c("1", "2", "3", "4")), 
                                  Traveltime = col_factor(levels = c("1", "2", "3", "4")), 
                                  Weekend_alcohol = col_factor(levels = c("1", "2", "3", "4", "5"))))
View(train)


# visually explore dataframe test
install.packages("ggplot2")
library(ggplot2)
ggplot(train, aes(x = Day_alcohol, fill = factor(Pass))) + 
  stat_count(width = 0.5) + 
  xlab("Alcohol consumption") + 
  ylab("Total Count") + 
  labs(fill = "Passed")

# install the randomForest algorithm
install.packages("randomForest")
library(randomForest)

# set up dataframes for randomforest

# select the 30 variables of the train dataset, not the identifier! 
rf.train.1 <- train[1:295, 1:30]
rf.label <- as.factor(train$Pass)

# execute randomForest 
set.seed(1234)
rf.1 <- randomForest(x= rf.train.1, y = rf.label, importance = TRUE)
rf.1
# gives error rate of 29.15%

# visually analyze which variables are important
varImpPlot(rf.1)

# make new randomforest for Failures, Absences, Age, Go_out, Health
# because these variabels show up to be important
rf.train.2 <- train[1:295, c("Failures", "Absences", "Age", "Go_out", "Health")]
rf.2 <- randomForest(x=rf.train.2, y = rf.label, importance = TRUE)
rf.2 
# gives error rate of 34.24%

rf.train.3 <- train[1:295, c("Failures", "Absences", "Age", "Go_out", "Educ_support")]
rf.3 <- randomForest(x=rf.train.3, y = rf.label, importance = TRUE)
rf.3
# gives error rate of 29.14%

rf.train.4 <- train[1:295, c("Failures", "Absences", "Age", "Go_out", "Educ_support","Guardian","Higher","Family_relationships")]
rf.4 <- randomForest(x=rf.train.4, y = rf.label, importance = TRUE)
rf.4
# gives error rate of 27.8%

rf.train.5 <- train[1:295, c("Failures", "Absences", "Age", "Go_out", "Educ_support","Guardian","Higher","Family_relationships", "Choice_of_school", "Employment_mother" )]
rf.5 <- randomForest(x=rf.train.5, y = rf.label, importance = TRUE)
rf.5
# gives error rate of 29.49%

# use cross validation to see what the accuracy is
install.packages("caret")
library(caret)

set.seed(4358)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)

# test accuracy for rf.1 and rf.4 because they appear to be te most accurate

set.seed(5342)
rf.1.cv.1 <- train(x = rf.train.1, y = rf.label, method = "rf", tuneLength = 3, trControl = ctrl.1)
rf.1.cv.1 
# shows that the value mtry = 30 was used for the model and gives an accuracy of 71,45%

set.seed(43491)
rf.4.cv.1 <- train(x = rf.train.4, y = rf.label, method = "rf", tuneLength = 3, trControl = ctrl.1)
rf.4.cv.1
# shows that the value mtry = 2 was used for the model and gives an accuracy of 71,56%



# CONCLUSION 
# the best accuracy we can achieve via RandomForest is rf.4, which is 71,56%



# add a "Pass" column to the test dataframe and subset my test records and features
test.pass <- data.frame(Pass = rep("None", nrow(test)), test[,])
test.submit.df <- test.pass[ c("Failures", "Absences", "Age", "Go_out", "Educ_support","Guardian","Higher","Family_relationships","Identifier")]

# make predictions
rf.4.preds <- predict(rf.4, test.submit.df)
table(rf.4.preds)


# calculate the F1 score for my randomforest predictions
precision <- posPredValue(rf.4.preds, rf.label  , positive="TRUE")
recall <- sensitivity(rf.4.preds, rf.label  , positive="TRUE")

F1 <- (2 * precision * recall) / (precision + recall)
# F1 score is 0.7073

# write out a .csv file for submission
submit.df <- data.frame(test.submit.df[c("Identifier")], Pass = rf.4.preds)
write.csv(submit.df, file= "5874491.csv", row.names = FALSE)

# write out a .txt file for the prediction
# manually done




