# Load necessary libraries
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2) #for data visualization
library(grid) # for grids
library(gridExtra) # for arranging the grids
library(corrplot) # for Correlation plot


diabetes_data <- read.csv("diabetes/diabetes.csv")
head(diabetes_data)

summary(diabetes_data)

str(diabetes_data)

colnames(diabetes_data)

sum(is.na(diabetes_data))

min_glucose <- min(diabetes_data$Glucose)
print(paste("Minimum Glucose Value :",min_glucose))

max_glucose <- max(diabetes_data$Glucose)
print(paste("Maximum Glucose Value :",max_glucose))

range_Glucose<- range(diabetes_data$Glucose)
print(range_Glucose)

Mean_Glucose <- mean(diabetes_data$Glucose)
Mean_Insulin <- mean(diabetes_data$Insulin)
Mean_BMI <- mean(diabetes_data$BMI)
Mean_Pregnancies <- mean(diabetes_data$Pregnancies)

print(paste("Mean of Glucose:", Mean_Glucose))
print(paste("Mean of Insulin:", Mean_Insulin))
print(paste("Mean of BMI:", Mean_BMI))
print(paste("Mean of Pregnancies:", Mean_Pregnancies))

Median_Glucose <- median(diabetes_data$Glucose)
Median_Insulin <- median(diabetes_data$Insulin)
Median_BMI <- median(diabetes_data$BMI)
Median_Pregnencies <- median(diabetes_data$Pregnancies)

print(paste("Median of Glucose :",Median_Glucose))
print(paste("Median of Insulin  :",Median_Insulin))
print(paste("Median of BMI :",Median_BMI))
print(paste("Median of Pregnencies :",Median_Pregnencies))

# Check the distribution of the target variable
ggplot(diabetes_data, aes(x = factor(Outcome))) + 
  geom_bar(fill = "skyblue", alpha = 0.7) +
  labs(x = "Outcome", y = "Count", title = "Distribution of Outcome")

# Check the distribution of numeric variables
numeric_vars <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", 
                  "Insulin", "BMI", "DiabetesPedigreeFunction", "Age")


p1 <- ggplot(diabetes_data, aes(x=Pregnancies)) + ggtitle("Number of times pregnant") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="blue") + ylab("Percentage")
p2 <- ggplot(diabetes_data, aes(x=Glucose)) + ggtitle("Glucose") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 5, colour="black", fill="orange") + ylab("Percentage")
p3 <- ggplot(diabetes_data, aes(x=BloodPressure)) + ggtitle("Blood Pressure") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 2, colour="black", fill="green") + ylab("Percentage")
p4 <- ggplot(diabetes_data, aes(x=SkinThickness)) + ggtitle("Skin Thickness") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 2, colour="black", fill="pink") + ylab("Percentage")
p5 <- ggplot(diabetes_data, aes(x=Insulin)) + ggtitle("Insulin") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 20, colour="black", fill="red") + ylab("Percentage")
p6 <- ggplot(diabetes_data, aes(x=BMI)) + ggtitle("Body Mass Index") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="yellow") + ylab("Percentage")
p7 <- ggplot(diabetes_data, aes(x=DiabetesPedigreeFunction)) + ggtitle("Diabetes Pedigree Function") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="purple") + ylab("Percentage")
p8 <- ggplot(diabetes_data, aes(x=Age)) + ggtitle("Age") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=1, colour="black", fill="lightblue") + ylab("Percentage")
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2)
grid.rect(width = 1, height = 1, gp = gpar(lwd = 1, col = "black", fill = NA))

numeric.var <- sapply(diabetes_data, is.numeric)
corr.matrix <- cor(diabetes_data[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", order = "hclust", tl.col = "black", tl.srt=45, tl.cex=0.8, cl.cex=0.8)
box(which = "outer", lty = "solid")



set.seed(123)  # for reproducibility
train_index <- createDataPartition(diabetes_data$Outcome, p = 0.8, list = FALSE)
train_data <- diabetes_data[train_index, ]
test_data <- diabetes_data[-train_index, ]

# Train a logistic regression model
logit_model <- glm(Outcome ~ ., data = train_data, family = binomial)

# Make predictions on the test set
predicted <- predict(logit_model, newdata = test_data, type = "response")
predicted_class <- ifelse(predicted > 0.5, 1, 0)

# Evaluate the model
conf_matrix <- confusionMatrix(factor(predicted_class), factor(test_data$Outcome))
print(conf_matrix)


# Calculate sensitivity and specificity
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# Evaluate the model using performance metrics
accuracy <- (conf_matrix$overall["Accuracy"] * 100)
sensitivity <- (conf_matrix$byClass["Sensitivity"] * 100)
specificity <- (conf_matrix$byClass[
  "Specificity"] * 100)
precision <- (conf_matrix$byClass["Pos Pred Value"] * 100)
f1_score <- (conf_matrix$byClass["F1"] * 100)

# Print performance metrics
print(paste("Accuracy:", round(accuracy, 2), "%"))
print(paste("Sensitivity (True Positive Rate):", round(sensitivity, 2), "%"))
print(paste("Specificity (True Negative Rate):", round(specificity, 2), "%"))
print(paste("Precision (Positive Predictive Value):", round(precision, 2), "%"))
print(paste("F1 Score:", round(f1_score, 2), "%"))
