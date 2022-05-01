wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE) 
str(wbcd) # Lets check what we have in this dataset
wbcd <- wbcd[-1]
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), 
                         labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n[c("radius_mean", "area_mean", "smoothness_mean")])
wbcd_train <- wbcd_n[101:569, ]
wbcd_test <- wbcd_n[1:100, ]
wbcd_train_labels <- wbcd[101:569, 1]
wbcd_test_labels <- wbcd[1:100, 1]
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, 
           prop.chisq=FALSE)

#True Positives (TP): These are cases in which we predicted yes (they have the disease), and they do have the disease.
#True Negatives (TN): We predicted no, and they don't have the disease.
#False Positives (FP): We predicted yes, but they don't actually have the disease. (Also known as a "Type I error.")
#False Negatives (FN): We predicted no, but they actually do have the disease. (Also known as a "Type II error.")

# Accuracy: Overall, how often is the classifier correct?
(66 + 29) / (66 + 29 + 1 + 4)

# Error rate: Overall, how often is it wrong?
(1 + 4) / (66 + 29 + 1 + 4)

# error rate = 1 - accuracy
1 - 0.95

## Beyond accuracy: other performance measures ----

# Kappa statistic: This is essentially a measure of how well the classifier performed as compared to how well it would have performed simply by chance.
pr_a <- (0.66 + 0.29)
pr_a

pr_e <- 0.67*0.70 + 0.33*0.30
pr_e

k <- (pr_a - pr_e) / (1 - pr_e)
k

# Sensitivity: When it's actually yes, how often does it predict yes?
sens <- 29 / (29 + 4)
sens

# Specificity: When it's actually no, how often does it predict no?
spec <- 66 / (66 + 1)
spec

# Precision: When it predicts yes, how often is it correct?
prec <- 66 / (66 + 4)
prec

#Additional Code
#How to randomize rows
wbcd_random <- wbcd[sample(nrow(wbcd)), ]

#Using Caret
library(caret)

# example using the caret package

confusionMatrix(wbcd_test_pred, wbcd_test_labels, positive = "Malignant")


#Using Z-Score Normalization
wbcd_z <- as.data.frame(scale(wbcd[-1]))
wbcd_train <- wbcd_z[101:569, ]; wbcd_test <- wbcd_z[1:100, ]  
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, 
           prop.chisq=FALSE)
confusionMatrix(wbcd_test_pred, wbcd_test_labels, positive = "Malignant")

# Partioning Data Randomly
in_train <- createDataPartition(wbcd$diagnosis, p = 0.7, list = FALSE)
wbcd_train <- wbcd_n[in_train, ]
wbcd_test <- wbcd_n[-in_train, ]
wbcd_train_labels <- wbcd[in_train, 1]
wbcd_test_labels <- wbcd[-in_train, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, 
           prop.chisq=FALSE)
confusionMatrix(wbcd_test_pred, wbcd_test_labels, positive = "Malignant")
