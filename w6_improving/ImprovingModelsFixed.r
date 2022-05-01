##### Improving Model Performance -------------------

# load the credit dataset
credit <- read.csv("credit.csv")
library(caret)
library(C50)

## Creating a simple tuned model ----
# automated parameter tuning of C5.0 decision tree 
set.seed(300)
test_set <- sample(1:nrow(credit), 0.3*nrow(credit))
credit_train <- credit[-test_set,]
credit_test <- credit[test_set,]
m1 <- train(default ~ ., data = credit_train, method = "C5.0")


# summary of tuning results
m1

# apply the best C5.0 candidate model to make predictions
p <- predict(m1, credit_test)
table(p, credit_test$default)


## Customizing the tuning process ----
# use trainControl() to alter resampling strategy
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")

# use expand.grid() to create grid of tuning parameters
grid <- expand.grid(.model = "tree",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    .winnow = "FALSE")

# look at the result of expand.grid()
grid

# customize train() with the control list and grid of parameters 
set.seed(300)
m <- train(default ~ ., data = credit_train, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
m


## Bagging ----
# Using the ipred bagged decision trees
library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit_train, nbagg = 25)

# estimate performance of ipred bagged trees
library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit_train, method = "treebag",
      trControl = ctrl)


## Random Forests ----
# random forest with default settings
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data = credit_train)
rf

library(caret)
# auto-tune a random forest
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

set.seed(300)
m_rf <- train(default ~ ., data = credit_train, method = "rf",
              metric = "Kappa", trControl = ctrl,
              tuneGrid = grid_rf)
m_rf

# auto-tune a boosted C5.0 decision tree
grid_c50 <- expand.grid(.model = "tree",
                        .trials = c(10, 20, 30, 40),
                        .winnow = "FALSE")

set.seed(300)
m_c50 <- train(default ~ ., data = credit_train, method = "C5.0",
                metric = "Kappa", trControl = ctrl,
               tuneGrid = grid_c50)
m_c50

## Making some mistakes more costly than others
# create a cost matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost

# apply the cost matrix to the tree
credit_cost <- C5.0(as.factor(default) ~ ., data = credit_train,
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)

summary(credit_cost_pred)

confusionMatrix(as.factor(credit_test$default), as.factor(credit_cost_pred), positive = "yes")

