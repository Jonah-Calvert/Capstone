library(readr)
library(pROC)
library(car)
library(ggplot2)
library(ggcorrplot)
library(MuMIn)
library(glmnet)
library(dplyr)
data <- read_csv("cs-training.csv", 
                    col_types = cols(
                      AppNum  = col_integer(),
                      `NumberOfTime30-59DaysPastDueNotWorse` = col_integer(),
                      MonthlyIncome = col_integer(), 
                      NumberOfOpenCreditLinesAndLoans = col_integer(), 
                      NumberOfTimes90DaysLate = col_integer(),
                      NumberRealEstateLoansOrLines = col_integer(),
                      `NumberOfTime60-89DaysPastDueNotWorse` = col_integer(),
                      NumberOfDependents = col_integer()
                    ))
View(data)

#Fix Data
data <- data[data$`NumberOfTime30-59DaysPastDueNotWorse` <= 20, ]
data <- data[data$RevolvingUtilizationOfUnsecuredLines <= 1, ]
data$MonthlyIncome <- ifelse(is.na(data$MonthlyIncome), 0, data$MonthlyIncome)
data$NumberOfDependents <- ifelse(is.na(data$NumberOfDependents), 0, data$NumberOfDependents)
data <- data[data$NumberOfDependents <= 8, ]


#Linear Regression
options(na.action = "na.fail")
globalmodel <- lm(data$SeriousDlqin2yrs ~ data$RevolvingUtilizationOfUnsecuredLines 
               + data$age 
               + data$`NumberOfTime30-59DaysPastDueNotWorse` 
               + data$DebtRatio 
               + data$MonthlyIncome
               + data$NumberOfOpenCreditLinesAndLoans
               + data$NumberOfTimes90DaysLate
               + data$NumberRealEstateLoansOrLines
               + data$`NumberOfTime60-89DaysPastDueNotWorse`
               + data$NumberOfDependents)

summary(globalmodel)

predictions <- predict(globalmodel, type = "response")
roc_obj <- roc(data$SeriousDlqin2yrs, predictions)
plot(roc_obj, main="ROC Curve", col="#1c61b6", lwd=2, legacy.axes = TRUE)
auc_value <- auc(roc_obj)
text(0.5, 0.2, paste("AUC =", round(auc(roc_obj), 2)))
print(auc_value)

#Fiding Best Model
combinations <- dredge(globalmodel, evaluate = TRUE)
print(combinations)


#Logistic Regression
logistic_model <- glm(data$SeriousDlqin2yrs ~ data$RevolvingUtilizationOfUnsecuredLines 
                      + data$age 
                      + data$`NumberOfTime30-59DaysPastDueNotWorse` 
                      + data$DebtRatio 
                      + data$MonthlyIncome
                      + data$NumberOfOpenCreditLinesAndLoans
                      + data$NumberOfTimes90DaysLate
                      + data$NumberRealEstateLoansOrLines
                      + data$`NumberOfTime60-89DaysPastDueNotWorse`
                      + data$NumberOfDependents, family = binomial)
summary(logistic_model)

predictions <- predict(logistic_model, type = "response")
roc_obj <- roc(data$SeriousDlqin2yrs, predictions)
auc_value <- auc(roc_obj)
print(auc_value)
plot(roc_obj, main="ROC Curve", col="#1c61b6", lwd=2, legacy.axes = TRUE)


#Multicollinearity


# VIF
vif_values <- vif(logistic_model)
vif_df <- data.frame(variable = names(vif_values), vif = vif_values)
print(vif_df)
ggplot(vif_df, aes(x = variable, y = vif)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Variance Inflation Factor (VIF)",
       x = "Predictor Variable",
       y = "VIF Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Correlation Matrix
correlation_matrix <- cor(data)
print(correlation_matrix)

ggcorrplot(correlation_matrix, hc.order = TRUE, type = "lower", outline.col = "white", insig = "blank")

#Adding Variable
data$HasBeenLate <- ifelse(data$`NumberOfTime30-59DaysPastDueNotWorse` != 0 | data$`NumberOfTime60-89DaysPastDueNotWorse` != 0 | data$NumberOfTimes90DaysLate != 0, TRUE, FALSE)
data <- subset(data, select = -data$`NumberOfTime30-59DaysPastDueNotWorse`)
data <- subset(data, select = -`NumberOfTime60-89DaysPastDueNotWorse`)
data <- subset(data, select = -`NumberOfTimes90DaysLate`)


#Fixing Some Variable Names
Age <- data$age
ThirtyPast <- data$`NumberOfTime30-59DaysPastDueNotWorse`
SixtyPast <- data$`NumberOfTime60-89DaysPastDueNotWorse`
NinetyPast <- data$NumberOfTimes90DaysLate
DebtRatio <- data$DebtRatio
MonthlyIncome <- data$MonthlyIncome
CreditLines <- data$NumberOfOpenCreditLinesAndLoans
RealEstate <- data$NumberRealEstateLoansOrLines
Dependents <- data$NumberOfDependents
RevolvingCredit <- data$RevolvingUtilizationOfUnsecuredLines


betterlogisticmodel <- glm(data$SeriousDlqin2yrs ~ RevolvingCredit 
                      + Age
                      + DebtRatio
                      + MonthlyIncome
                      + CreditLines
                      + RealEstate
                      + Dependents
                      + data$HasBeenLate, family = binomial)
summary(logistic_model)


# VIF
vif_values <- vif(betterlogisticmodel)
vif_df <- data.frame(variable = names(vif_values), vif = vif_values)
print(vif_df)
ggplot(vif_df, aes(x = variable, y = vif)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Variance Inflation Factor (VIF)",
       x = "Predictor Variable",
       y = "VIF Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Correlation Matrix
correlation_matrix <- cor(data)
print(correlation_matrix)

ggcorrplot(correlation_matrix, hc.order = TRUE, type = "lower", outline.col = "white", insig = "blank")


#GRAPHS
#Ages
age_bins<- cut(data$age, breaks = c(20, 30, 40, 50, 60, 70, Inf), labels = c("21-30", "31-40", "41-50", "51-60", "61-70", "71+"))
barplot(height = table(age_bins),
        names.arg = levels(age_bins),
        xlab = "Age Group",
        ylab = "Count",
        main = "Age Distribution of Applicants",
)

#Dependents
barplot(table(data$NumberOfDependents),
        xlab = "Number of Dependents",
        ylab = "Count",
        main = "Distribution of Number of Dependents")


