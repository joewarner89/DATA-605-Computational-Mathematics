### Preprocess data ! 
library(glmnet)
library(MLmetrics)
library(missForest)
library(class)
library(dplyr)
library(plyr)
library(data.table)
library(caret)
library(xgboost)
library(gbm)
library(BBmisc)
library(MASS)
library(DMwR)
library(corrplot)
lib <- data 
str(lib)

lib$readmitted <- case_when(lib$readmitted %in% c(">30","NO") ~ "0",
                              TRUE ~ "1")
install.packages("DMwR")

# remove duplicate 
## Removing duplicate patients encounter and only take the first observation to avoid bias
lib <- lib[!duplicated(lib$patient_nbr),]
# change year to 10 number
lib$age <- ifelse(lib$age == "[0-10)",  0, lib$age);
lib$age <- ifelse(lib$age == "[10-20)", 10, lib$age);
lib$age <- ifelse(lib$age == "[20-30)", 20, lib$age);
lib$age <- ifelse(lib$age == "[30-40)", 30, lib$age);
lib$age <- ifelse(lib$age == "[40-50)", 40, lib$age);
lib$age <- ifelse(lib$age == "[50-60)", 50, lib$age);
lib$age <- ifelse(lib$age == "[60-70)", 60, lib$age);
lib$age <- ifelse(lib$age == "[70-80)", 70, lib$age);
lib$age <- ifelse(lib$age == "[80-90)", 80, lib$age);
lib$age <- ifelse(lib$age == "[90-100)", 90, lib$age);

# Change categorical values 
lib$max_glu_serum <- ifelse(lib$max_glu_serum == "None",  0, lib$max_glu_serum);
lib$max_glu_serum <- ifelse(lib$max_glu_serum == "Norm",  100, lib$max_glu_serum);
lib$max_glu_serum <- ifelse(lib$max_glu_serum == ">200",  200, lib$max_glu_serum);
lib$max_glu_serum <- ifelse(lib$max_glu_serum == ">300",  300, lib$max_glu_serum);

lib$A1Cresult <- ifelse(lib$A1Cresult == "None",  0, lib$A1Cresult);
lib$A1Cresult <- ifelse(lib$A1Cresult == "Norm",  5, lib$A1Cresult);
lib$A1Cresult <- ifelse(lib$A1Cresult == ">7",    7, lib$A1Cresult);
lib$A1Cresult <- ifelse(lib$A1Cresult == ">8",    8, lib$A1Cresult);

lib$metformin <- ifelse(lib$metformin == 'No', 0 , lib$metformin);
lib$metformin <- ifelse(lib$metformin == 'Steady', 1 , lib$metformin);
lib$metformin <- ifelse(lib$metformin == 'Up', 2 , lib$metformin);
lib$metformin <- ifelse(lib$metformin == 'Down', 3 , lib$metformin);

lib$repaglinide <- ifelse(lib$repaglinide == 'No', 0 , lib$repaglinide);
lib$repaglinide <- ifelse(lib$repaglinide == 'Steady', 1 , lib$repaglinide);
lib$repaglinide <- ifelse(lib$repaglinide == 'Up', 2 , lib$repaglinide);
lib$repaglinide <- ifelse(lib$repaglinide == 'Down', 3 , lib$repaglinide);

lib$chlorpropamide <- ifelse(lib$chlorpropamide == 'No', 0 , lib$chlorpropamide);
lib$chlorpropamide <- ifelse(lib$chlorpropamide == 'Steady', 1 , lib$chlorpropamide);
lib$chlorpropamide <- ifelse(lib$chlorpropamide == 'Up', 2 , lib$chlorpropamide);
lib$chlorpropamide <- ifelse(lib$chlorpropamide == 'Down', 3 , lib$chlorpropamide);

lib$nateglinide <- ifelse(lib$nateglinide == 'No', 0 , lib$nateglinide);
lib$nateglinide <- ifelse(lib$nateglinide == 'Steady', 1 , lib$nateglinide);

lib$glimepiride <- ifelse(lib$glimepiride == 'No', 0 , lib$glimepiride);
lib$glimepiride <- ifelse(lib$glimepiride == 'Steady', 1 , lib$glimepiride);
lib$glimepiride <- ifelse(lib$glimepiride == 'Up', 2 , lib$glimepiride);

lib$pioglitazone <- ifelse(lib$pioglitazone == 'No', 0 , lib$pioglitazone);
lib$pioglitazone <- ifelse(lib$pioglitazone == 'Steady', 1 , lib$pioglitazone);
lib$pioglitazone <- ifelse(lib$pioglitazone == 'Up', 2 , lib$pioglitazone);

lib$glyburide <- ifelse(lib$glyburide == 'No', 0 , lib$glyburide);
lib$glyburide <- ifelse(lib$glyburide == 'Steady', 1 , lib$glyburide);
lib$glyburide <- ifelse(lib$glyburide == 'Up', 2 , lib$glyburide);
lib$glyburide <- ifelse(lib$glyburide == 'Down', 3 , lib$glyburide);

lib$acarbose <- ifelse(lib$acarbose == 'No', 0 , lib$acarbose);
lib$acarbose <- ifelse(lib$acarbose == 'Steady', 1 , lib$acarbose);

lib$insulin <- ifelse(lib$insulin == 'No', 0 , lib$insulin);
lib$insulin <- ifelse(lib$insulin == 'Steady', 1 , lib$insulin);
lib$insulin <- ifelse(lib$insulin == 'Up', 2 , lib$insulin);
lib$insulin <- ifelse(lib$insulin == 'Down', 3 , lib$insulin);

lib$glipizide.metformin <- ifelse(lib$glipizide.metformin == 'No', 0 , lib$glipizide.metformin);
lib$glipizide.metformin <- ifelse(lib$glipizide.metformin == 'Steady', 1 , lib$glipizide.metformin);

lib$change <- ifelse(lib$change == 'No', 0 , lib$change);
lib$change <- ifelse(lib$change == 'Ch', 1 , lib$change);

lib$diabetesMed <- ifelse(lib$diabetesMed == 'No', 0 , lib$diabetesMed);
lib$diabetesMed <- ifelse(lib$diabetesMed == 'Yes', 1 , lib$diabetesMed);
#See Variable with low variances 
nearZeroVar(lib, names = T, freqCut = 19, uniqueCut = 10)
lib$number_outpatient + lib$number_emergency + lib$number_inp

# reajust data 
lib$visits = lib$number_outpatient + lib$number_emergency + lib$number_inp
readmitted = lib$readmitted
lib <- subset(lib, select =-c(readmitted))
lib$readmitted = readmitted

#This column has low variances 
keys <- nearZeroVar(lib, names = T, freqCut = 19, uniqueCut = 10)
keys

lib$num_med <- 0
lib$num_changes <- 0
for(key in keys){
  lib$num_med <- ifelse(lib[key] != 0, lib$num_med + 1, lib$num_med)
  lib$num_changes <- ifelse((lib[key] == 1 | lib[key] == 2 | lib[key] == 3), lib$num_changes + 1, lib$num_changes)
}

# drop key column 

  
lib <- subset(lib, select = 
         -c(metformin, repaglinide, nateglinide, chlorpropamide, glimepiride, glipizide, glyburide, pioglitazone, rosiglitazone, acarbose, miglitol, insulin, glyburide.metformin, tolazamide, metformin.pioglitazone, glipizide.metformin))
head(lib)


## Normalize, Remove Outliers, and Standardize Numerical Features
diabetes$number_inpatient <- log1p(diabetes$number_inpatient)
diabetes$number_outpatient <- log1p(diabetes$number_outpatient)
diabetes$number_emergency <- log1p(diabetes$number_emergency)

histogram(lib$number_inpatient)
histogram(lib$number_outpatient)
histogram(lib$number_emergency)



non_outliers = function(x, zs) {
  temp <- (x - mean(x))/sd(x)
  return(temp < zs)
}



lib <- lib[non_outliers(lib$number_inpatient, 3),]
lib <- lib[non_outliers(lib$number_outpatient, 3),]
lib <- lib[non_outliers(lib$number_emergency, 3),]
lib <- subset(lib, select = -c(number_emergency))



#Normalise skewed features and removing outliers using z-score

#Normalise skewed features and removing outliers using z-score

cols <- dplyr::select_if(lib, is.numeric)
temp <- scale(dplyr::select_if(lib, is.numeric))
for(col in colnames(cols)){
  lib[,col] <- temp[,col]
}
head(lib)

# see c
library(GGally)
ggpairs(lib, columns = c("time_in_hospital","num_lab_procedures","num_procedures",
                         "number_inpatient","number_outpatient","number_diagnoses",
                         "number_emergency","visits","num_med","num_changes","num_med"))

ggpairs(lib, columns = c("number_inpatient","num_lab_procedures","number_outpatient","number_diagnoses",
                         "num_procedures","visits"))
cor <- lib %>% dplyr::select(number_outpatient,number_inpatient,visits,number_emergency, num_med, num_changes)
lib$num_med
lib$num_med   <- as.numeric(lib$num_med)

lib$num_changes <- as.numeric(lib$num_changes)
class(num)

plot(cor)
library(psych)
corPlot(cor)

# Model development 

# set seed 
set.seed(123)

# creating  training and test 
train_indices <- sample(seq_len(nrow(lib)), 0.7* nrow(lib))
train_data <- lib[train_indices, ]
test_data <- lib[-train_indices, ]

# Fit the logistic regression 
model <- glm(readmitted == '1' ~  visits + number_inpatient + number_outpatient  , data = train_data)
summary(model)   

predicted_probs <- predict(model, newdata = test_data , type = "response")

# Convert probabilities to binary predictions
predicted_classes <- ifelse(predicted_probs > 0.1, "1", "0")
predicted_labels
# Convert probabilities to predicted classes
#predicted_classes <- ifelse(predicted_probs > 0.3, "1", ifelse(predicted_probs < 0.25, "NO", ">30"))
#print(data.frame(Probabilities = predicted_probs, Predicted_labels = predicted_labels))

# Display the confusion matrix
confusion_matrix <- table(predicted_classes, test_data$readmitted)
print(confusion_matrix)


# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

#Display the predicted labels
print(predicted_labels)
pred_model_1 <- test_data
pred_model_1$predictor <- predicted_labels
head(pred_model_1)

#evaluation 
##3 second model 
library(boot)
# Define the logistic regression model
set.seed(123)
logistic_model <- glm(readmitted == ">30" ~ visits + number_outpatient  , data = lib)
summary(logistic_model)
# Perform 10-fold cross-validation
cv_results <- cv.glm(lib, logistic_model, K = 10)

print(cv_results)

# Access accuracy
accuracy2 <- 1 - cv_results$delta[1]
cat("Accuracy:", accuracy2, "\n")


# Predict the probabilities of the positive class ("Wins")
predicted_probs2 <- predict(logistic_model , newdata = lib, type = "response")


# Convert probabilities to binary predictions
predicted_labels2 <- ifelse(predicted_probs2 > 0.5, "readmitted", ifelse(predicted_probs2 > 0.25, "Not readmitted", "readmitted after 30"))

# Display results
print(data.frame(Probabilities = predicted_probs2, Predicted_Labels = predicted_labels2))


# Display the confusion matrix
confusion_matrix2 <- table(predicted_labels2, lib$readmitted)
print(confusion_matrix2)

# Display the predicted labels
print(predicted_labels2)
pred_model2 <- lib
pred_model2$predictor <- predicted_labels
head(pred_model2)
#####################################################
logistic_model2 <-glm(readmitted==">30"  ~ age + visits + number_outpatient + max_glu_serum + insulin + diabetesMed , data = lib)
summary(logistic_model2)
# Perform 10-fold cross-validation
cv_results2 <- cv.glm(lib, logistic_model2, K = 10)

print(cv_results2)

# Access accuracy
accuracy3 <- 1 - cv_results2$delta[1]
cat("Accuracy:", accuracy3, "\n")


# Predict the probabilities of the positive class ("Wins")
predicted_probs3 <- predict(logistic_model2, newdata = lib, type = "response")


# Convert probabilities to binary predictions
predicted_labels3 <- ifelse(predicted_probs3 > 0.5, "readmitted", ifelse(predicted_probs3 < 0.25, "readmitted after 30", "Not readmitted"))
predicted_labels3 <- ifelse(predicted_probs3 > 0.5, "readmitted","Not readmitted")
# Display results
print(data.frame(Probabilities = predicted_probs3, Predicted_Labels = predicted_labels3))
at <- data.frame(Probabilities = predicted_probs3, Predicted_Labels = predicted_labels3) %>% arrange(desc(Probabilities))

# Display the predicted labels
print(predicted_labels3)
pred_model3 <- lib
pred_model3$predictor <- predicted_labels3
head(pred_model3)

# Display the confusion matrix
confusion_matrix3 <- table(predicted_labels3, lib$readmitted)
print(confusion_matrix3)


library(pROC)

# Create the ROC curve
roc_obj <- roc(lib$readmitted, predicted_probs3)
# Plot the ROC curve
plot(roc_obj, main = "ROC Curve for the Logistic Regression Model")
abline(0, 1, lty = 2, col = "gray")  # Add a reference line for a random classifier

# Calculate the AUC
auc_value <- auc(roc_obj)
cat("AUC:", auc_value, "\n")
