# install.packages("treemap")
# install.packages("GGally")
# install.packages("corrplot")
library(tidyverse)
library(caret)
library(GGally)
library(treemap)
library(MASS)
library(corrplot)
library(caTools)

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(caret)
library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(rpart)
library(randomForest)
library(ggpubr)


getwd()
df1 <- read.csv("/Users/blosherbrar/Desktop/Professional/University/HEC Montreal/Fall 2020/3 - Statistical Learning (60603A)/Customer_Intelligence_Assignment/retention_data_F20/train_student.csv")
#df1 <- subset(df1, df1$promo == "False") #only considering data in which promo is false
#df2 <- read.csv("/Users/blosherbrar/Desktop/Professional/University/HEC Montreal/Fall 2020/3 - Statistical Learning (60603A)/Customer_Intelligence_Assignment/retention_data_F20/score_student_withID.csv")

# df1$churn_in_12 = ifelse((df1$churn_in_12 == "True"),
#                          1,
#                          0
# )
##################################### General info about Dataset ######################################################
# creating a seperate df for churn == true
df1_churn_true <- subset(df1, df1$churn_in_12 == "True")
# 
# print(nrow(df1))
# print(ncol(df1))
# print(df1[1:5,])
# print(colnames(df1))
# 
# print(nrow(df2))
# print(ncol(df2))
# print(df2[1:5,])
# print(colnames(df2))
# 
# # x <- paste(colnames(df1))
# # x
# # write.csv(x,"df1_col_names.csv")
# 
# # find out how many missing values are in data
# df1_na_count <-sapply(df1, function(y) sum(length(which(is.na(y)))))
# df1_na_count <- data.frame(df1_na_count)
# print(df1_na_count)
# 
# df1_na_count_percentage <-sapply(df1, function(y) round(sum(length(which(is.na(y))))/(sum(length(which(is.na(y))))+sum(length(which(!is.na(y))))),4))
# df1_na_count_percentage <- data.frame(df1_na_count_percentage)
# print(df1_na_count_percentage)

##################################### Data exploration of missing values + Replacing missing values ######################################################

#phone_price
# print(summary(df1$phone_price))
# print(unique(df1$plan_type))
# print(length(df1$plan_type == "buy"))
# 
# rent_plan = 0
# buy_plan = 0
# bring_plan = 0
# for (type in df1$plan_type){
#   if (type == "rent"){
#     rent_plan = rent_plan + 1
#   } 
#   else if (type == "buy"){
#     buy_plan = buy_plan + 1
#   }
#   else if (type == "bring"){
#     bring_plan = bring_plan + 1
#   }
# }
# print(paste(c(rent_plan, buy_plan, bring_plan), c("Rent", "Buy","Bring"), sep = " "))

### replace phone_price == N/A with phone_price == 0
df1$phone_price = ifelse(is.na(df1$phone_price),
                         0,
                         df1$phone_price
)
# print(summary(df1$phone_price))

# voice_minutes
# print(unique(df1$unlimited_voice))

# true_unlimited_voice = 0
# false_unlimited_voice = 0
# for (type in df1$unlimited_voice){
#   if (type == "True"){
#     true_unlimited_voice = true_unlimited_voice + 1
#   } 
#   else if (type == "False"){
#     false_unlimited_voice = false_unlimited_voice + 1
#   }
# }
# print(paste(c(true_unlimited_voice, false_unlimited_voice), c("True", "False"), sep = " "))
# 
# ### must replace voice_minutes w/ unlimited
# print(summary(df1$voice_minutes))

df1$voice_minutes = ifelse(is.na(df1$voice_minutes),
                           ave(df1$voice_minutes, FUN = function(x) max(x, na.rm = TRUE)),
                           df1$voice_minutes
)

# ggplot(df1, aes(x = voice_minutes))+
#   geom_histogram()
# 
# ggplot(df1, 
#        aes(x = voice_minutes, 
#            fill = churn_in_12)) + 
#   geom_bar(position = "fill")
# # time_since_complaints - missing values
# print(summary(df1$total_complaints))
# 
# zero_time_since_complaints = 0
# more_than_zero_time_since_complaints = 0
# for (num in df1$total_complaints){
#   if (num == 0){
#     zero_time_since_complaints = zero_time_since_complaints + 1
#   } 
#   else if (num > 0 ){
#     more_than_zero_time_since_complaints = more_than_zero_time_since_complaints + 1
#   }
# }
# print(paste(c(zero_time_since_complaints, more_than_zero_time_since_complaints), c("Zero complaints", "More than zero Complaints"), sep = " "))
# ### replace time_since_complaints == N/A to max(time_since_complaints) + 10
# 
# ggplot(df1, aes(x = time_since_complaints)) +
#   geom_histogram()
# 
# ggplot(df1,
#        aes(x = churn_in_12, 
#            y = time_since_complaints)) +
#   geom_bar(stat = "identity")
# 
# ggplot(df1, 
#        aes(x = churn_in_12, 
#            y = time_since_complaints)) +
#   geom_boxplot()
# 
# print(summary(df1$time_since_complaints))

df1$time_since_complaints = ifelse(is.na(df1$time_since_complaints),
                                   ave(df1$time_since_complaints, FUN = function(x) max(x, na.rm = TRUE))+10,
                                   df1$time_since_complaints
)
# 
# ggplot(df1, 
#        aes(x = churn_in_12, 
#            y = time_since_complaints)) +
#   geom_boxplot()
# 
# ggplot(df1, aes(x = time_since_complaints)) +
#   geom_histogram()
# 
# # time_since_technical_problems
# zero_times_technical_problems = 0
# more_than_zero_times_technical_problems = 0
# for (num in df1$total_technical_problems){
#   if (num == 0){
#     zero_times_technical_problems = zero_times_technical_problems + 1
#   } 
#   else if (num > 0 ){
#     more_than_zero_times_technical_problems = more_than_zero_times_technical_problems + 1
#   }
# }
# print(paste(c(zero_times_technical_problems, more_than_zero_times_technical_problems), c("Zero technical problems", "More than zero technical problems"), sep = " "))
# 
# ### replace time_since_technical_problems == N/A to max(time_since_technical_problems) + 10
# print(summary(df1$total_technical_problems))
# print(summary(df1$time_since_technical_problems))
# 
# ggplot(df1, aes(x = time_since_technical_problems)) +
#   geom_histogram()
# 
# ggplot(df1,
#        aes(x = churn_in_12, 
#            y = time_since_technical_problems)) +
#   geom_bar(stat = "identity")
# 
# ggplot(df1, 
#        aes(x = churn_in_12, 
#            y = time_since_technical_problems)) +
#   geom_boxplot()

df1$time_since_technical_problems = ifelse(is.na(df1$time_since_technical_problems),
                                   ave(df1$time_since_technical_problems, FUN = function(x) max(x, na.rm = TRUE))+10,
                                   df1$time_since_technical_problems
)
# 
# ggplot(df1, 
#        aes(x = churn_in_12, 
#            y = time_since_complaints)) +
#   geom_boxplot()
# 
# ggplot(df1, aes(x = time_since_complaints)) +
#   geom_histogram()
# 
# ggplot(df1, 
#        aes(x = time_since_complaints, 
#            fill = churn_in_12)) + 
#   geom_bar(position = "fill")
# 
# #time_since_data_overage_fees
# 
# print(summary(df1$total_data_overage_fees))
# print(summary(df1$time_since_data_overage))
# 
# ggplot(df1, aes(x = time_since_data_overage)) +
#   geom_histogram()
# 
# ggplot(df1, aes(x = total_data_overage_fees)) +
#   geom_histogram()
# 
# ggplot(df1, 
#        aes(x = churn_in_12, 
#            y = time_since_data_overage)) +
#   geom_boxplot()
# 
# zero_total_data_overage_fees = 0
# more_than_zero_total_data_overage_fees = 0
# for (num in df1$total_data_overage_fees){
#   if (num == 0){
#     zero_total_data_overage_fees = zero_total_data_overage_fees + 1
#   } 
#   else if (num > 0 ){
#     more_than_zero_total_data_overage_fees = more_than_zero_total_data_overage_fees + 1
#   }
# }
# print(paste(c(zero_total_data_overage_fees, more_than_zero_total_data_overage_fees), c("Zero data overage", "More than zero data overage"), sep = " "))
# ## create new column w/ max(time_since_data_overage) + 10

df1$time_since_data_overage = ifelse(is.na(df1$time_since_data_overage),
                                     ave(df1$time_since_data_overage, FUN = function(x) max(x, na.rm = TRUE))+10,
                                     df1$time_since_data_overage
)
# 
# ggplot(df1, 
#        aes(x = churn_in_12, 
#            y = time_since_data_overage)) +
#   geom_boxplot()
# 
# ggplot(df1, aes(x = time_since_data_overage)) +
#   geom_histogram()
# 
# ggplot(df1, 
#        aes(x = time_since_data_overage, 
#            fill = churn_in_12)) + 
#   geom_bar(position = "fill")
# 
# #time_since_voice_overage_fees
# 
# print(summary(df1$total_voice_overage_fees))
# print(summary(df1$time_since_voice_overage))
# 
# ggplot(df1, aes(x = time_since_voice_overage)) +
#   geom_histogram()
# 
# ggplot(df1, aes(x = total_voice_overage_fees)) +
#   geom_histogram()
# 
# ggplot(df1, 
#        aes(x = churn_in_12, 
#            y = time_since_voice_overage)) +
#   geom_boxplot()
# 
# zero_total_voice_overage_fees = 0
# more_than_zero_total_voice_overage_fees = 0
# for (num in df1$total_voice_overage_fees){
#   if (num == 0){
#     zero_total_voice_overage_fees = zero_total_voice_overage_fees + 1
#   } 
#   else if (num > 0 ){
#     more_than_zero_total_voice_overage_fees = more_than_zero_total_voice_overage_fees + 1
#   }
# }
# print(paste(c(zero_total_voice_overage_fees, more_than_zero_total_voice_overage_fees), c("Zero voice overage", "More than zero voice overage"), sep = " "))
# ## create new column w/ max(time_since_voice_overage) + 10

df1$time_since_voice_overage = ifelse(is.na(df1$time_since_voice_overage),
                                      ave(df1$time_since_voice_overage, FUN = function(x) max(x, na.rm = TRUE))+10,
                                      df1$time_since_voice_overage
)
# 
# ggplot(df1, 
#        aes(x = churn_in_12, 
#            y = time_since_voice_overage)) +
#   geom_boxplot()
# 
# ggplot(df1, aes(x = time_since_voice_overage)) +
#   geom_histogram()
# 
# ggplot(df1, 
#        aes(x = time_since_voice_overage, 
#            fill = churn_in_12)) + 
#   geom_bar(position = "fill")
# 
# #time_since_overage
# 
# print(summary(df1$total_overage_fees))
# print(summary(df1$time_since_overage))
# 
# ggplot(df1, aes(x = time_since_overage)) +
#   geom_histogram()
# 
# ggplot(df1, 
#        aes(x = time_since_overage, 
#            fill = churn_in_12)) + 
#   geom_bar(position = "fill")
# 
# ggplot(df1, aes(x = total_overage_fees)) +
#   geom_histogram()
# 
# ggplot(df1, 
#        aes(x = churn_in_12, 
#            y = time_since_overage)) +
#   geom_boxplot()
# 
# zero_total_overage_fees = 0
# more_than_zero_total_overage_fees = 0
# for (num in df1$total_overage_fees){
#   if (num == 0){
#     zero_total_overage_fees = zero_total_overage_fees + 1
#   } 
#   else if (num > 0 ){
#     more_than_zero_total_overage_fees = more_than_zero_total_overage_fees + 1
#   }
# }
# print(paste(c(zero_total_overage_fees, more_than_zero_total_overage_fees), c("Zero voice overage", "More than zero voice overage"), sep = " "))
# ## create new column w/ max(time_since_overage) + 10

df1$time_since_overage = ifelse(is.na(df1$time_since_overage),
                                ave(df1$time_since_overage, FUN = function(x) max(x, na.rm = TRUE))+10,
                                df1$time_since_overage
)
# 
# ggplot(df1, 
#        aes(x = churn_in_12, 
#            y = time_since_overage)) +
#   geom_boxplot()
# 
# ggplot(df1, aes(x = time_since_overage)) +
#   geom_histogram()
# 
# ggplot(df1, 
#        aes(x = time_since_overage, 
#            fill = churn_in_12)) + 
#   geom_bar(position = "fill")
##################################### Feature scaling #####################################
df1 <- df1[,c(2:4,6:34,36)]
##################################### First Logistic Regression ######################################################

# Split into training + validation #
# set.seed(123)
# split = sample.split(df1$churn_in_12, SplitRatio = 0.8)
# training_set = subset(df1, split == TRUE)
# validation_set = subset(df1, split == FALSE)
# classifier1 = glm(formula = churn_in_12 ~ .,
#                  family = binomial,
#                  data = training_set
#                  )
# 
# # Predict Logistic Regression in Validation #
# prob_pred_1 <- predict(classifier1,
#                        type = 'response',
#                        newdata = validation_set[-33]
#                        )
# y_pred_1 <- ifelse(prob_pred_1 >= .50,
#                    1,
#                    0
#                    )
# # Make Confusion Matrix for Logistic Regression #
# cm_1 <- table(validation_set[,33], y_pred_1)
# cm_1
# accuracy1 <- (1 - sum(diag(cm_1))/nrow(df1))
# accuracy1
##################################### Convert Categorical + Feature scaling ######################################################

#true & false

##gender
df1$gender = ifelse((df1$gender == "Man"),
                         0,
                         1
)
##work phone
df1$workphone = ifelse((df1$workphone == "True"),
                    1,
                    0
)
##unlimited voice
df1$unlimited_voice = ifelse((df1$unlimited_voice == "True"),
                       1,
                       0
)
##promo
df1$promo = ifelse((df1$promo == "True"),
                             1,
                             0
)
df1$churn_in_12 = ifelse((df1$churn_in_12 == "True"),
                   1,
                   0
)

#multi-class classification

##plan type
print(unique(df1$plan_type))
df1$plan_type = as.factor(factor(df1$plan_type,
                                levels = c("buy", "rent", "bring"),
                                labels = c(1, 2, 3)
))
##################################### Re-do Training/Validation Split ######################################################


df1[, c(4:26,30,32)] = scale(df1[, c(4:26,30,32)]) #feature scaling 
set.seed(123)
split = sample.split(df1$churn_in_12, SplitRatio = 0.8)
training_set = subset(df1, split == TRUE)
validation_set = subset(df1, split == FALSE)
##################################### Second Logistic Regression ######################################################

classifier2 = glm(formula = churn_in_12 ~ .,
                  family = binomial,
                  data = df1
)

summary(classifier2)
# Predict Logistic Regression in Training #
prob_pred_training_2 <- predict(classifier2,
                       type = 'response',
                       newdata = df1[-33]
)
y_pred_training_2 <- factor(ifelse(prob_pred_training_2 >= .50,
                   1,
                   0
))
y_actual_training_2 <- factor(ifelse(df1$churn_in_12 == 1,
                            1,
                            0
))
confusionMatrix(data = y_pred_training_2, reference = y_actual_training_2)
roc <- roc(training_set$churn_in_12, prob_pred_training_2, plot= TRUE, print.auc=TRUE)
roc

# Predict Logistic Regression in Validation #
# prob_pred_validation_2 <- predict(classifier2,
#                        type = 'response',
#                        newdata = validation_set[-32]
# )
# y_pred_validation_2 <- factor(ifelse(prob_pred_validation_2 >= .50,
#                    1,
#                    0
# ))
# y_actual_validation_2 <- factor(ifelse(validation_set$churn_in_12 == 1,
#                               1,
#                               0
# ))
# 
# confusionMatrix(data = y_pred_validation_2, reference = y_actual_validation_2)
# roc <- roc(validation_set$churn_in_12, prob_pred_validation_2, plot= TRUE, print.auc=TRUE)
# roc
# Finding Optimal Cutoff and adjust class of prediction #
pred <- prediction(prob_pred_training_2, y_actual_training_2)
perf <- performance(pred, "spec", "sens")
cutoffs <- data.frame(cut=perf@alpha.values[[1]], specificity=perf@x.values[[1]], 
                      sensitivity= perf@y.values[[1]])
opt_cutoff <- cutoffs[which.min(abs(cutoffs$specificity-cutoffs$sensitivity)),]
opt_cutoff
ggplot(data = cutoffs) +
  geom_line(aes(x = cut, y = specificity, color ="red"), size = 1.5)+
  geom_line(aes(x = cut, y = sensitivity, color = "blue"), size = 1.5) +
  labs(x = "cutoff", y ="value") +
  scale_color_discrete(name = "", labels = c("Specificity", "Sensitivity"))+
  geom_vline(aes(xintercept = opt_cutoff$cut))+
  geom_text(aes(x= 0.55, y= 0.75),label="opt_cutoff = 0.3",hjust=1, size=4)

# Make Confusion Matrix for Logistic Regression #
cm_2 <- table(validation_set[,32], y_pred_2)
cm_2
accuracy2 <- (1 - sum(diag(cm_2))/nrow(df1))

summary(classifier2)

##################################### Second Logistic Regression - Entire Training Set ######################################################

classifier4 = glm(formula = churn_in_12 ~ .,
                  family = binomial,
                  data = df1
)

##################################### Third Logistic Regression ######################################################

classifier3 <- glm(formula = churn_in_12 ~
                     plan_type+
                     age +
                     base_monthly_rate_phone +
                     phone_balance +
                     time_since_complaints +
                     time_since_technical_problems+
                     phone_price +
                     period_id,
                   family = binomial,
                   data = training_set
)
summary(classifier3)
vif(classifier2)
vif(classifier3)

# Predict Logistic Regression in Training #
prob_pred_train_3 <- predict(classifier3,
                       type = 'response',
                       newdata = training_set[-32]
)
roc <- roc(training_set$churn_in_12, prob_pred_train_3, plot= TRUE, print.auc=TRUE)
roc
# Predict Logistic Regression in Validation #
prob_pred_3 <- predict(classifier3,
                       type = 'response',
                       newdata = validation_set[-32]
)
y_pred_3 <- ifelse(prob_pred_3 >= .50,
                   1,
                   0
)
# Make Confusion Matrix for Logistic Regression #
cm_3 <- table(validation_set[,32], y_pred_3)
cm_3
accuracy3 <- (1 - sum(diag(cm_3))/nrow(df1))
accuracy3
accuracy2
accuracy1


roc_prob_pred_3 <- roc(validation_set$churn_in_12, prob_pred_3, plot= TRUE, print.auc=TRUE)
roc_prob_pred_3
roc_prob_pred_2 <- roc(validation_set$churn_in_12, prob_pred_2, plot= TRUE, print.auc=TRUE)
roc_prob_pred_2
##################################### Finding Optimal Cutoff and adjust class of prediction ######################################################
y_pred_2 <- factor(ifelse(prob_pred_2 >= 0.5, "Yes", "No"))
train_actual_2 <- factor(ifelse(training_set$churn_in_12 == 1, "Yes", "No"))
test_pred <- factor(ifelse(test_prob >= 0.5, "Yes", "No"))
test_actual <- factor(ifelse(test$Churn == 1, "Yes", "No"))

##################################### Test Set ######################################################
df2 <- read.csv("/Users/blosherbrar/Desktop/Professional/University/HEC Montreal/Fall 2020/3 - Statistical Learning (60603A)/Customer_Intelligence_Assignment/retention_data_F20/score_student_withID.csv")
#df2 <- subset(df2, df2$promo == "False") #only considering data in which promo is false
df3 <- read.csv("/Users/blosherbrar/Desktop/Professional/University/HEC Montreal/Fall 2020/3 - Statistical Learning (60603A)/Customer_Intelligence_Assignment/retention_data_F20/score_student_withID.csv")
#df3 <- subset(df3, df3$promo == "False") #only considering data in which promo is false
##################################### Test Set Replacing missing values + useful columns #####################################


### replace phone_price == N/A with phone_price == 0
df2$phone_price = ifelse(is.na(df2$phone_price),
                         0,
                         df2$phone_price
)

# ### must replace voice_minutes w/ unlimited

df2$voice_minutes = ifelse(is.na(df2$voice_minutes),
                           ave(df2$voice_minutes, FUN = function(x) max(x, na.rm = TRUE)),
                           df2$voice_minutes
)
# ### replace time_since_complaints == N/A to max(time_since_complaints) + 10

df2$time_since_complaints = ifelse(is.na(df2$time_since_complaints),
                                   ave(df2$time_since_complaints, FUN = function(x) max(x, na.rm = TRUE))+10,
                                   df2$time_since_complaints
)

# ### replace time_since_technical_problems == N/A to max(time_since_technical_problems) + 10

df2$time_since_technical_problems = ifelse(is.na(df2$time_since_technical_problems),
                                           ave(df2$time_since_technical_problems, FUN = function(x) max(x, na.rm = TRUE))+10,
                                           df2$time_since_technical_problems
)

# ## create new column w/ max(time_since_data_overage) + 10

df2$time_since_data_overage = ifelse(is.na(df2$time_since_data_overage),
                                     ave(df2$time_since_data_overage, FUN = function(x) max(x, na.rm = TRUE))+10,
                                     df2$time_since_data_overage
)

# ## create new column w/ max(time_since_voice_overage) + 10

df2$time_since_voice_overage = ifelse(is.na(df2$time_since_voice_overage),
                                      ave(df2$time_since_voice_overage, FUN = function(x) max(x, na.rm = TRUE))+10,
                                      df2$time_since_voice_overage
)

# ## create new column w/ max(time_since_overage) + 10

df2$time_since_overage = ifelse(is.na(df2$time_since_overage),
                                ave(df2$time_since_overage, FUN = function(x) max(x, na.rm = TRUE))+10,
                                df2$time_since_overage
)

# df2_na_count <-sapply(df2, function(y) sum(length(which(is.na(y)))))
# df2_na_count <- data.frame(df2_na_count)
# print(df2_na_count)

df2 <- df2[,c(2:4,6:34)] #select only useful columns

##################################### Test Set First Logistic Regression ######################################################
# 
# # Split into training + validation #
# 
# # Predict Logistic Regression in Validation #
# test_prob_pred_1 <- predict(classifier1,
#                        type = 'response',
#                        newdata = df2
# )
# test_y_pred_1 <- ifelse(test_prob_pred_1 >= .50,
#                    1,
#                    0
# )
# # export result #
# test_y_pred_1 = data.frame(test_y_pred_1)
# 
# export_data = subset(df3, select = unique_family)
# 
# export_data$pred1 = test_y_pred_1
# 
# export_data = export_data[!(export_data$pred1 == 0),1]
# 
# write.csv(export_data,"Desktop/Third_Prediction.csv", row.names = FALSE )

##################################### Test Set Second Logistic Regression ######################################################

# Convert Categorical + Feature scaling #

#true & false
##gender
df2$gender = ifelse((df2$gender == "Man"),
                    0,
                    1
)
##work phone
df2$workphone = ifelse((df2$workphone == "True"),
                       1,
                       0
)
##unlimited voice
df2$unlimited_voice = ifelse((df2$unlimited_voice == "True"),
                             1,
                             0
)
df2$promo = ifelse((df2$promo == "True"),
                             1,
                             0
)

#multi-class classification
##plan type
print(unique(df2$plan_type))
df2$plan_type = as.factor(factor(df2$plan_type,
                                 levels = c("buy", "rent", "bring"),
                                 labels = c(1, 2, 3)
))


df2[, c(4:26,30,32)] = scale(df2[, c(4:26,30,32)])

# Predict Logistic Regression in Validation #
test_prob_pred_2 <- predict(classifier2,
                            type = 'response',
                            newdata = df2
)
test_y_pred_2 <- ifelse(test_prob_pred_2 >= .1493025,
                        1,
                        0
)
# export results #

test_y_pred_2 = data.frame(test_y_pred_2)

export_data = subset(df3, select = unique_family)

export_data$pred2 = test_y_pred_2

export_data = export_data[!(export_data$pred2 == 0),1]

write.csv(export_data,"Desktop/Seventeen_Prediction.csv", row.names = FALSE )








##################################### Test Set Forth Logistic Regression ######################################################

# Convert Categorical + Feature scaling #

#true & false
##gender
df2$gender = ifelse((df2$gender == "Man"),
                    0,
                    1
)
##work phone
df2$workphone = ifelse((df2$workphone == "True"),
                       1,
                       0
)
##unlimited voice
df2$unlimited_voice = ifelse((df2$unlimited_voice == "True"),
                             1,
                             0
)

#multi-class classification
##plan type
print(unique(df2$plan_type))
df2$plan_type = as.factor(factor(df2$plan_type,
                                 levels = c("buy", "rent", "bring"),
                                 labels = c(1, 2, 3)
))

# df2[, c(3:25,29,31)] = scale(df2[, c(3:25,29,31)])

# Predict Logistic Regression in Validation #
test_prob_pred_2 <- predict(classifier4,
                            type = 'response',
                            newdata = df2
)
test_y_pred_2 <- ifelse(test_prob_pred_2 >= .1346,
                        1,
                        0
)
# export results #

test_y_pred_2 = data.frame(test_y_pred_2)

export_data = subset(df3, select = unique_family)

export_data$pred2 = test_y_pred_2

export_data = export_data[!(export_data$pred2 == 0),1]

write.csv(export_data,"Desktop/Forth_Prediction.csv", row.names = FALSE )







