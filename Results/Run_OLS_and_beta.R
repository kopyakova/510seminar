setwd("/Users/annakopyakova/Desktop/Case Study 510/510seminar/Results")

#Load raw data
weather          <- read.delim("../Data/weather_data.csv", sep = ",", header = TRUE) # Not imputed yet
ovitrap_cleaned  <- read.delim("../Data/ovitrap_data_per_month_per_province_cleaned.csv", 
                               sep = ",", header = TRUE)
ovitrap_original <- read.delim("../Data/ovitrap_data_per_month_per_province.csv", sep = ",", header = TRUE)

#Run the methods 
source("../Source/main_analysis.R", echo = F)
ols  <- main_analysis(ovitrap_original, ovitrap_cleaned, weather, number_of_bootstraps = 1,
                      model_type = "linear_regression", two_stage = F, save_train_test = T)

beta <- main_analysis(ovitrap_original, ovitrap_cleaned, weather, number_of_bootstraps = 1,
                      model_type = "beta_regression", two_stage = F, save_train_test = T)

# Load training and testing data
training_final <- read.csv("../Data/training_data.csv")
training_final <- training_final[,!(names(training_final) == "X")]
training_final <- training_final[,!(names(training_final) == "adm")]
training_final <- training_final[,!(names(training_final) == "date")]
training_final <- training_final[ ,!(names(training_final) == "value_indicator")]

test_final <- read.csv("../Data/test_data.csv")
test_final <- test_final[,!(names(test_final) == "X")]
test_final <- test_final[,!(names(test_final) == "adm")]
test_final <- test_final[,!(names(test_final) == "date")]
test_final <- test_final[ ,!(names(test_final) == "value_indicator")]

true_train <- training_final$value
true_test  <- test_final$value
source("Auxiliary_functions_results.R", echo = F)
######### The OLS regression evaluation #########
# Get predictions
ols_pred_train <- ols$predictions_train
ols_pred_test  <- ols$predictions_test

# Evaluate results
ols_results <- evaluate_results(true_train = true_train, pred_train = ols_pred_train, 
                                   true_test  = true_test, pred_test = ols_pred_test, 
                                   method_name = "OLS")

# Evaluate residuls
ols_resid  <- evaluate_residuals(predicted_test = ols_pred_test, true_test = true_test, 
                                    method = "OLS")
# Print plots with the results
print(ols_results$plot)
print(ols_resid$resid_vs_pred)
print(ols_resid$resid_vs_actual)

######### The beta regression evaluation ######### 
# Get predictions
beta_pred_train <- beta$predictions_train
beta_pred_test  <- beta$predictions_test

# Evaluate results
beta_results <- evaluate_results(true_train = true_train, pred_train = beta_pred_train*100, 
                                    true_test  = true_test, pred_test = beta_pred_test*100, 
                                    method_name = "Beta")  

# Evaluate residuls
beta_resid  <- evaluate_residuals(predicted_test = beta_pred_test*100, true_test = true_test, 
                                    method = "Beta regression")

#Print plots with the results
print(beta_results$plot)
beta_resid$resid_vs_pred
beta_resid$resid_vs_actual

######### Base models - simple variable selection ######### 
ols_base  <- lm(value ~ ., data = training_final)
ols_base  <- step(ols_base , direction = "backward", trace = FALSE)

ols_base_pred_train <- ols_base $fitted.values
ols_base_pred_test  <- predict(ols_base , newdata = test_final)
ols_base_results    <- evaluate_results(true_train = true_train, pred_train = ols_base_pred_train, 
                                        true_test  = true_test, pred_test = ols_base_pred_test, 
                                        method_name = "OLS base simple selection")


######### Base Beta - simple variable selection #########
beta_base            <- beta_regression(training_final, T, test_final)
beta_base_pred_train <- beta_base$train_pred
beta_base_pred_test  <- beta_base$test_pred
beta_base_results    <- evaluate_results(true_train = true_train, pred_train = beta_base_pred_train*100, 
                                         true_test  = true_test, pred_test = beta_base_pred_test*100, 
                                         method_name = "Beta base simple selection")
