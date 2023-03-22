loans_train = read.csv('loan_train.csv')
head(loans_train)
summary(loans_train)

#I really wanted to look at the ratio of loan approvals by Gender, and run a hypothesis test to see if they were significantly different
Status = subset(loans_train$Status, loans_train$Gender != "")
Gender = subset(loans_train$Gender, loans_train$Gender != "")

loan_gender_table = table(Status, Gender)
prop.table(loan_gender_table, margin = 2)

avg_rej_rate = (loan_gender_table[1, 1] + loan_gender_table[1, 2]) /
  (loan_gender_table[1, 1] + loan_gender_table[1, 2] + loan_gender_table[2, 1] + loan_gender_table[2, 2]); avg_rej_rate

prop.test(x = c(loan_gender_table[1, 1], loan_gender_table[1, 2]), n = 
            c((loan_gender_table[1, 1] + loan_gender_table[2, 1]), (loan_gender_table[1, 2] + loan_gender_table[2, 2])),
            alternative = 'two.sided', conf.level = .95)
#Very high p-value, no difference in rejection based on gender


str(loans_train)
loans_train[c('Gender', 'Married', 'Dependents', 'Education', 'Self_Employed', 'Area', 'Status', 'Term', 'Credit_History')] =
  lapply(loans_train[c('Gender', 'Married', 'Dependents', 'Education', 'Self_Employed', 'Area', 'Status', 'Term', 'Credit_History')]
         , as.factor)

loans_train$Coapplicant_Income = as.integer(loans_train$Coapplicant_Income)

loans_train$Status = ifelse(loans_train$Status == 'Y', 1, 0)

#Removing rows where data is empty, removed about 110 rows
loans_train = subset(loans_train, loans_train$Gender != "")
loans_train = subset(loans_train, loans_train$Married != "")
loans_train = subset(loans_train, loans_train$Dependents != "")
loans_train = subset(loans_train, loans_train$Self_Employed != "")
loans_train = subset(loans_train, loans_train$Credit_History != "")
loans_train = subset(loans_train, loans_train$Term != "")

loans_train = na.omit(loans_train)
dim(loans_train)

cor(loans_train$Applicant_Income, loans_train$Loan_Amount)
cor((loans_train$Applicant_Income + loans_train$Coapplicant_Income), loans_train$Loan_Amount)

library(ggplot2)
ggplot(data = loans_train, aes(x = Status)) +
  geom_bar(stat = 'count', position = position_dodge()) +
  facet_grid(Area ~ Gender + Education + Self_Employed)

#Splitting the data

loans_index = sample(1:nrow(loans_train), size = round(.7 * nrow(loans_train)), replace = FALSE)
loans_samp = loans_train[loans_index, ]
loans_test = loans_train[-loans_index, ]


#Creating Models
loan_model_full = glm(Status ~ ., data = loans_samp, family = 'binomial')
summary(loan_model_full)

loan_model_reduced = glm(Status ~ Married + Credit_History + Area, data = loans_samp, family = 'binomial')
summary(loan_model_reduced)

#Our reduced model loses almost no information

prediction1 = predict(loan_model_reduced, newdata = loans_test[, -12], type = 'response')
prediction1

prediction1 = ifelse(prediction1 > .5, 0, 1)

library(pROC)

roc_curve = roc(loans_test$Status, prediction1, plot = TRUE, print.auc = TRUE, col = 'blue', lwd = 4, print.auc.y = .4,
                legacy.axes = TRUE, main = 'Loan Prediction ROC Curve')

library(caret)
confusionMatrix(table(prediction1, loans_test$Status))

#Getting bad accuracy, and inconsistent ROC curve

#Decision Tree
library(rpart)

tree_model = rpart(loans_samp$Status ~ ., data = loans_samp[, -12], method = 'class')
prediction2 = predict(tree_model, loans_test[, -12])

prediction2 = ifelse(prediction2[, 1] > prediction2[, 2], 0, 1)

confusionMatrix(table(prediction2, loans_test$Status))
#We're getting a much higher accuracy now, much better
