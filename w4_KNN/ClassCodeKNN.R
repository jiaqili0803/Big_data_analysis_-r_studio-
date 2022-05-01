#Reading
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
#Delete column number 1
wbcd <- wbcd[,-1]
str(wbcd)
#Factorize
wbcd$diagnosis <- as.factor(wbcd$diagnosis)
#Model matrix for creating dummy variables
wbcd_dummy <- as.data.frame(model.matrix(~ . -1, data = wbcd))

str(wbcd_dummy)
#Deleting the excess dummy
wbcd_dummy$diagnosisB <- NULL
#Function for normalizing
normalize <- function(x) {
return ((x - min(x)) / (max(x) - min(x)))
}
#Normalizing everything
wbcd_n <- as.data.frame(lapply(wbcd_dummy, normalize))
summary(wbcd_n)
str(wbcd_n)




#Break into test and train
set.seed(12345)
test_set <- sample(1:nrow(wbcd_n), 100) # 20% of data

# X 
#test dataset WITHOUT the Y value (KNN need separate the X and y values)
wbcd_test <- wbcd_n[test_set, -1] # exclude the 1st column
#train without Y
wbcd_train <- wbcd_n[-test_set, -1]

# y
# Label for test, essentially just the Y value
test_label <- wbcd_n[test_set, 1]
#Label for train
train_label <- wbcd_n[-test_set, 1]



library(class)


#KNN command, outputs prediction for the test data
# usually choose odd number, k is usually the square root of number of rows
# The number K is typically chosen as the square root of N, the total number of points in the training data set. 
test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = train_label, k = 21)
summary(test_pred)

library(caret)
#Confusion Matrix
confusionMatrix(as.factor(test_label), as.factor(test_pred), positive = "1")
# M == 1(HAVE cancer), so 0 should be not have cancer, 
# from confusionMatrix, have 1 wrong prediction, who is cancered but predicted as not cancered.

