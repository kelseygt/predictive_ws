######################
#                    #  
#  LOADING PACKAGES  #
#                    #
######################


library(rsample)  # data splitting 
library(dplyr)    # data transformation
library(rpart)    # partitioning
library(ROSE)     # sampling
library(caret)    # naive bayes
library(ranger)   # random forest


######################
#                    #  
#      DATA PREP     #
#                    #
######################


# Loading data
ws <- read.csv(file.choose(), header = T)
str(ws)
ws <- ws[-c(1:3, 6, 14, 20)]

# Converting CIP / withdraw flag to factor
ws$X2.Digit.CIP <- as.factor(ws$X2.Digit.CIP)
ws$W.Flag <- as.factor(ws$W.Flag)

# Checking how many missing value for each variable
sapply(ws, function(x) sum(is.na(x)))


######################
#                    #  
#     NAIVE BAYES    #
#                    #
######################


# Training and testing sets (70% / 30%)
split <- initial_split(ws, prop = .7, strata = "W.Flag")
train <- training(split)
test  <- testing(split)

# Distribution of complete withdraw rates across data sets
table(ws$W.Flag) %>% prop.table()
table(train$W.Flag) %>% prop.table()
table(test$W.Flag) %>% prop.table()

# Creating objects x = the predictor variables, and y = the response variable
y = train$W.Flag
x = subset(train, select = -c(W.Flag))

# Training and testing
model = caret::train(x, y, method = 'nb', laplace = 1, na.action = na.pass, trControl = trainControl(method = 'cv'))
predict <- predict(model, newdata = test)
confusionMatrix(predict, test$W.Flag)
var_perf <- varImp(model, scale = F)
plot(var_perf)
# Acc: 0.9703, Kappa: 0 (lol)


######################
#                    #  
#  SAMPLING METHODS  #
#                    #
######################


# Oversampling, undersampling, both, and ROSE
ws_oversampled <- ovun.sample(W.Flag ~ ., data = train, method = "over")$data
table(ws_oversampled$W.Flag)

ws_undersampled <- ovun.sample(W.Flag ~ ., data = train, method = "under")$data
table(ws_undersampled$W.Flag)

ws_both <- ovun.sample(W.Flag ~ ., data = train, method = "both")$data
table(ws_both$W.Flag)

ws_rose <- ROSE(W.Flag ~ ., data = train)$data
table(ws_rose$W.Flag)


######################
#                    #  
#    4 NB MODELS     #
#                    #
######################


# Oversampling
y_os = ws_oversampled$W.Flag
x_os = subset(ws_oversampled, select = -c(W.Flag))
model_os = caret::train(x_os, y_os, method = 'nb', laplace = 1, na.action = na.pass, trControl = trainControl(method = 'cv'))
predict_os <- predict(model_os, newdata = test)
confusionMatrix(predict_os, test$W.Flag)
var_perf_os <- varImp(model_os, scale = F)
plot(var_perf_os)
# Acc: 0.685, Kappa: 0.0557

# Undersampling
y_us = ws_undersampled$W.Flag
x_us = subset(ws_undersampled, select = -c(W.Flag))
model_us = caret::train(x_us, y_us, method = 'nb', laplace = 1, na.action = na.pass, trControl = trainControl(method = 'cv'))
predict_us <- predict(model_us, newdata = test)
confusionMatrix(predict_us, test$W.Flag)
var_perf_us <- varImp(model_us, scale = F)
plot(var_perf_us)
# Acc: 0.6867, Kappa: 0.0585

# Both
y_both = ws_both$W.Flag
x_both = subset(ws_both, select = -c(W.Flag))
model_both = caret::train(x_both, y_both, method = 'nb', laplace = 1, na.action = na.pass, trControl = trainControl(method = 'cv'))
predict_both <- predict(model_both, newdata = test)
confusionMatrix(predict_both, test$W.Flag)
var_perf_both <- varImp(model_both, scale = F)
plot(var_perf_both)
# Acc: 0.6847, Kappa: 0.0558

# ROSE
y_rose = ws_rose$W.Flag
x_rose = subset(ws_rose, select = -c(W.Flag))
model_rose = caret::train(x_rose, y_rose, method = 'nb', laplace = 1, na.action = na.pass, trControl = trainControl(method = 'cv'))
predict_rose <- predict(model_rose, newdata = test)
confusionMatrix(predict_rose, test$W.Flag)
var_perf_rose <- varImp(model_rose, scale = F)
plot(var_perf_rose)
# Acc: 0.8599, Kappa: 0.0834


######################
#                    #  
#   RANDOM FOREST    #
#                    #
######################


rf <- ranger(W.Flag ~ ., data = train, write.forest = T, importance = "permutation", num.trees = 2000, mtry = 3)
rf
rf_pred <- predict(rf, data = test)
head(rf_pred$predictions)
#rf_predictions <- ifelse(rf_pred$predictions > 0.5, 1, 0)
confusionMatrix(as.factor(rf_pred$predictions), as.factor(test$W.Flag))
importance(rf)
# Acc = 0.9721, Kappa = 0.0044

rf_os <- ranger(W.Flag ~ ., data = ws_oversampled, write.forest = T)
rf_os
rf_pred_os <- predict(rf_os, data = test)
confusionMatrix(as.factor(rf_pred_os$predictions), as.factor(test$W.Flag))
# Acc = 0.965, Kappa = .0408

rf_us <- ranger(W.Flag ~ ., data = ws_undersampled, write.forest = T)
rf_us
rf_pred_us <- predict(rf_us, data = test)
confusionMatrix(as.factor(rf_pred_us$predictions), as.factor(test$W.Flag))
# Acc = 0.9721, Kappa = 0.0589

rf_rose <- ranger(W.Flag ~ ., data = ws_rose, write.forest = T)
rf_rose
rf_pred_rose <- predict(rf_rose, data = test)
confusionMatrix(as.factor(rf_pred_rose$predictions), as.factor(test$W.Flag))
# Acc = 0.9569, Kappa = .0645


######################
#                    #  
#    LOGISTIC REG    #
#                    #
######################


# Haven't finished. Need to modify data set to remove College of Academic Affairs
log_reg <- glm(W.Flag ~ ., data = train, family = "binomial")
pred_log <- predict(log_reg, newdata = test, type = "response")
pred_log
pred_y <- as.factor(ifelse(as.numeric(pred_log$predictions > 0.5, 1, 0)))


######################
#                    #  
#   DECISION TREES   #
#                    #
######################

# In progress. Worth running? Probably not.
test$W.Flag <- as.character(test$W.Flag)
test$W.Flag[test$W.Flag == 1] <- "Y"
test$W.Flag[test$W.Flag == 0] <- "N"
test$W.Flag <- as.factor(test$W.Flag)

train$W.Flag <- as.character(train$W.Flag)
train$W.Flag[train$W.Flag == 1] <- "Y"
train$W.Flag[train$W.Flag == 0] <- "N"
train$W.Flag <- as.factor(train$W.Flag)

trctrl <- trainControl(method = "cv", n = 4, classProbs = TRUE)
dt <- train(W.Flag ~ ., data = train, method = "rpart", metric = "ROC", parms  = list(split = "gini"), trControl = trctrl)
dt_pred <- predict(dt, newdata = test, type = "prob")
dt_pred$Y


######################
#                    #  
#  NB PART/FULL TIME #
#                    #
######################


# Creating two data sets (part-time and full-time)
pt <- ws[ws$Time.Status == "Part-Time", ]
ft <- ws[ws$Time.Status == "Full-Time", ]

# Part-time model
split_pt <- initial_split(pt, prop = .7, strata = "W.Flag")
train_pt <- training(split_pt)
test_pt  <- testing(split_pt)

# Creating objects x = the predictor variables, and y = the response variable
y_pt = train_pt$W.Flag
x_pt = subset(train_pt, select = -c(W.Flag))

# Training and testing
model_pt = caret::train(x_pt, y_pt, method = 'nb', laplace = 1, na.action = na.pass, trControl = trainControl(method = 'cv'))
predict_pt <- predict(model_pt, newdata = test_pt)
confusionMatrix(predict_pt, test_pt$W.Flag)
var_perf <- varImp(model_pt, scale = F)
plot(var_perf)
# Acc: 0.9503, Kappa: NEGATIVE? lollolololool

# Full-time model
split_ft <- initial_split(ft, prop = .7, strata = "W.Flag")
train_ft <- training(split_ft)
test_ft  <- testing(split_ft)

# Creating objects x = the predictor variables, and y = the response variable
y_ft = train_ft$W.Flag
x_ft = subset(train_ft, select = -c(W.Flag))

# Training and testing
model_ft = caret::train(x_ft, y_ft, method = 'nb', laplace = 1, na.action = na.pass, trControl = trainControl(method = 'cv'))
predict_ft <- predict(model_ft, newdata = test_ft)
confusionMatrix(predict_ft, test_ft$W.Flag)
var_perf <- varImp(model_ft, scale = F)
plot(var_perf)
# Acc: 0.9817, Kappa: 0.0119