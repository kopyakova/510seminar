library(Metrics)
boot <- 100

training_final <- read.csv("training_imputed.csv")
training_final <- training_final[,-1]
training_final <- training_final[ ,!(names(training_final) == "latitude")]
training_final <- training_final[ ,!(names(training_final) == "longitude")]
training_final <- training_final[ ,!(names(training_final) == "value_indicator")]

test_final <- read.csv("testing_imputed.csv")
test_final <- test_final[,-1]
test_final <- test_final[ ,!(names(test_final) == "latitude")]
test_final <- test_final[ ,!(names(test_final) == "longitude")]
test_final <- test_final[ ,!(names(test_final) == "value_indicator")]

true_train <- training_final$value
true_test  <- test_final$value
#OLS regression with backard selection
# ols_bw <- main(ovitrap_original, ovitrap_cleaned, weather, number_of_bootstraps = boot, 
#             model_type = "linear_regression", two_stage = F)
ols_bw_pred_train <- ols_bw$stage2$predictions_train
ols_bw_pred_test  <- ols_bw$stage2$predictions_test

ols_bw_results <- evaluate_results(true_train = true_train, pred_train = ols_bw_pred_train, 
                                   true_test  = true_test, pred_test = ols_bw_pred_test, 
                                   method_name = "OLS")

print(ols_bw_results$plot)
ols_bw_resid  <- evaluate_residuals(predicted_test = ols_bw_pred_test, true_test = true_test, 
                                    method = "OLS")
ols_bw_resid$resid_vs_pred
ols_bw_resid$resid_vs_actual

######The beta regression with backward selection######
# beta_bw  <- main(ovitrap_original, ovitrap_cleaned, weather, number_of_bootstraps = boot, 
#               model_type = "beta_regression", two_stage = F)
beta_bw_pred_train <- beta_bw$stage2$predictions_train
beta_bw_pred_test <- beta_bw$stage2$predictions_test


beta_bw_results <- evaluate_results(true_train = true_train, pred_train = beta_bw_pred_train*100, 
                                    true_test  = true_test, pred_test = beta_bw_pred_test*100, 
                                    method_name = "Beta")  
print(beta_bw_results$plot)
beta_bw_resid  <- evaluate_residuals(predicted_test = beta_bw_pred_test*100, true_test = true_test, 
                                    method = "Beta regression")
beta_bw_resid$resid_vs_pred
beta_bw_resid$resid_vs_actual

######The beta regression with forward selection######
# beta_fw  <- main(ovitrap_original, ovitrap_cleaned, weather, number_of_bootstraps = boot, 
#                  model_type = "beta_regression", two_stage = F)
# beta_fw_pred_train <- beta_fw$stage2$predictions_train
# beta_fw_pred_test <- beta_fw$stage2$predictions_test
# 
# beta_fw_results <- evaluate_results(true_train = true_train, pred_train = beta_fw_pred_train*100, 
#                                    true_test  = true_test, pred_test = beta_fw_pred_test*100, 
#                                    method_name = "Beta with forward selection")  
# 
# beta_fw_results$plot


######Base models - no variable selection######
training_final <- training_final[, 3:30]
test_final     <- test_final[, 3:30]
ols_base <- lm(value ~ ., data = training_final)
ols_base_pred_train <- ols_base$fitted.values
ols_base_pred_test <- predict(ols_base, newdata = test_final)
ols_base_results <- evaluate_results(true_train = true_train, pred_train = ols_base_pred_train, 
                                     true_test  = true_test, pred_test = ols_base_pred_test, 
                                     method_name = "OLS base, no selection")

######Base models - simple variable selection######
ols_base_selection <- lm(value ~ ., data = training_final)
ols_base_selection <- step(ols_base_selection, direction = "backward", trace = FALSE)
ols_base_selection_pred_train <- ols_base_selection$fitted.values
ols_base_selection_pred_test <- predict(ols_base_selection, newdata = test_final)
ols_base_selection_results <- evaluate_results(true_train = true_train, pred_train = ols_base_selection_pred_train, 
                                               true_test  = true_test, pred_test = ols_base_selection_pred_test, 
                                               method_name = "OLS base simple selection")

######Base Beta - no selection######
beta_base <- beta_regression(training_final)
beta_base_pred_train <- beta_base$train_pred
beta_base_pred_test <- predict(beta_base$model, newdata = test_final)
beta_base_results <- evaluate_results(true_train = true_train, pred_train = beta_base_pred_train*100, 
                                     true_test  = true_test, pred_test = beta_base_pred_test*100, 
                                     method_name = "Beta base, no selection")

######Base Beta - simple variable selection######
beta_base_selection <- beta_regression(training_final, T, test_set = test_final)
beta_base_selection_pred_train <- beta_base_selection$train_pred
beta_base_selection_pred_test <-  beta_base_selection$test_pred
beta_base_selection_results <- evaluate_results(true_train = true_train, pred_train = beta_base_selection_pred_train*100, 
                                                true_test  = true_test, pred_test = beta_base_selection_pred_test*100, 
                                                method_name = "Beta base simple selection")

#########XGBOOST
xg_pred_test <- xg_boost_results$predictions
xg_results <- evaluate_results(true_train = true_test, pred_train = xg_pred_test, 
                                                true_test  = true_test, pred_test = xg_pred_test, 
                                                method_name = "XGBoost")
print(xg_results$plot)
xg_results_resid  <- evaluate_residuals(predicted_test = xg_pred_test, true_test = true_test, 
                                     method = "XGBoost")
xg_results_resid$resid_vs_pred
xg_results_resid$resid_vs_actual
###########
beta_regression <- function(data, var_select = F, test_set, type = "ML", link = "logit"){
  if (any(data$value==1)||any(data$value==0)){
    n.obs <- sum(!is.na(data$value))
    data$value <- ((data$value * (n.obs - 1) + 0.5) / n.obs )/100
  }
  
  if (var_select){
    selection <- betaselect(data[,!(names(data) == "value")], data$value, 
                              criterion="AIC", method = "backward")
    final_covariates <- selection$variable
    final_model  <- betareg(value ~ ., data = data[, c(final_covariates, "value")], link = link, type = type) 
    predictions_final_train <- predict(final_model, newdata = data[, c(final_covariates, "value")])
    predictions_final_test <- predict(final_model, newdata = test_set[, c(final_covariates, "value")])
    
  } else {
    final_model <- betareg(value ~ ., data = data, link = "logit") 
    predictions_final_train <- predict(final_model, newdata = data)
    predictions_final_train <- predict(final_model, newdata = data)
    predictions_final_test <- predict(final_model, newdata = test_set)
  }

  return(list("model" = final_model, "train_pred" = predictions_final_train,
              "test_pred" = predictions_final_test))
}



library(Metrics)
library(ggplot2)
evaluate_results <- function(true_train, pred_train, true_test, pred_test, method_name){
  erroe_train <- true_train - pred_train
  error_test  <- true_test - pred_test
  mrae_train <- median(abs(erroe_train))
  mrae_test  <- median(abs(error_test))
  
  rmse_train <- rmse(actual = true_train, predicted = pred_train)
  rmse_test  <- rmse(actual = true_test, predicted = pred_test)
  
  sd_error_train <- sqrt(var(erroe_train))
  sd_error_test  <- sqrt(var(error_test))
  data <- data.frame(cbind(pred_test, true_test))
  plot <- ggplot(data, aes(x = pred_test, y = true_test)) + 
          geom_point() + xlim(0, 100)+ ylim(0, 100)+ 
          geom_line(color='red',#data = data.frame(cbind("x" = c(0:100), "y" = c(0:100))),
                    aes(x = c(1:480), y = c(1:480)))+
          labs(y="Actual ovitrap index", x = paste0("Ovitrap index predicted with ", method_name))

  return_list <- list()
  return_list$rmse_train <- rmse_train
  return_list$rmse_test <- rmse_test
  
  return_list$mrae_train <- mrae_train
  return_list$mrae_test <- mrae_test
  
  return_list$sd_error_train <- sd_error_train
  return_list$sd_error_test <- sd_error_test
  
  return_list$plot <- plot
  return(return_list)
}

resid_vs_pred_plot <- function(residuals, predictions, method_name, actual = F){
  
  data <- data.frame(cbind(residuals, predictions))
  if (actual){
    plot <- ggplot(data, aes(x = predictions, y = residuals)) + 
      geom_point()+ ylim(-3, 3)+
      geom_hline(yintercept = 0, color='red')+
      labs(y="Standardized residuals", x = "Actual Ovitrap index")
  } else {
    plot <- ggplot(data, aes(x = predictions, y = residuals)) + 
      geom_point()+ ylim(-3, 3)+
      geom_hline(yintercept = 0, color='red')+
      labs(y="Standardized residuals", x = paste0("Ovitrap index predicted with ", method_name))
  }
  
  return(plot)
}

#Evaluate residuals
evaluate_residuals <- function(predicted_test, true_test, method){
  residuals <- true_test - predicted_test
  standardized_residuals <- scale(residuals)
  return_list <- list()
  return_list$hist <- hist(residuals)
  return_list$resid_vs_pred <- resid_vs_pred_plot(standardized_residuals, predicted_test,  method)
  return_list$resid_vs_actual <- resid_vs_pred_plot(standardized_residuals, true_test,  method,
                                                    actual = T)
  return(return_list)
}


######OLS regression with forward selection#######
ols_fw <- main(ovitrap_original, ovitrap_cleaned, weather, number_of_bootstraps = boot, 
               model_type = "linear_regression", two_stage = F)
ols_fw_pred_train <- ols_fw$stage2$predictions_train
ols_fw_pred_test <- ols_fw$stage2$predictions_test

ols_fw_results <- evaluate_results(true_train = true_train, pred_train = ols_fw_pred_train, 
                                   true_test  = true_test, pred_test = ols_fw_pred_test, 
                                   method_name = "OLS with forward selection")

