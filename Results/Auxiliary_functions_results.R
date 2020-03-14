######### EVALUATE RESULTS OF THE MODEL #########
#calculate RMSE, MdAE, SD of the errors and actual vs predicted plot
evaluate_results <- function(true_train, pred_train, true_test, pred_test, method_name){
  library(Metrics)
  library(ggplot2)
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
    geom_line(color='red',
              aes(x = c(1:480), y = c(1:480)))+
    labs(y="Actual ovitrap index", x = paste0("Ovitrap index predicted with ", method_name))
  
  return_list <- list()
  return_list$rmse_train <- rmse_train
  return_list$rmse_test  <- rmse_test
  
  return_list$mrae_train <- mrae_train
  return_list$mrae_test  <- mrae_test
  
  return_list$sd_error_train <- sd_error_train
  return_list$sd_error_test  <- sd_error_test
  
  return_list$plot <- plot
  return(return_list)
}

######### EVALUATE RESIDUALS #########
#Plot of residuals vs predicrted or actual values
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
  return_list$resid_vs_pred <- resid_vs_pred_plot(standardized_residuals, predicted_test,  method)
  return_list$resid_vs_actual <- resid_vs_pred_plot(standardized_residuals, true_test,  method,
                                                    actual = T)
  return(return_list)
}

######### Beta regression for the base model #########
beta_regression <- function(data, var_select = F, test_set){
  if (any(data$value==1)||any(data$value==0)){
    n.obs <- sum(!is.na(data$value))
    data$value <- ((data$value * (n.obs - 1) + 0.5) / n.obs )/100
  }
  
  if (var_select){
    selection <- betaselect(data[,!(names(data) == "value")], data$value, 
                            criterion="AIC", method = "backward")
    final_covariates <- selection$variable
    final_model  <- betareg(value ~ ., data = data[, c(final_covariates, "value")], link = "logit") 
    predictions_final_train <- predict(final_model, newdata = data[, c(final_covariates, "value")])
    predictions_final_test <- predict(final_model, newdata = test_set[, c(final_covariates, "value")])
    
  } else {
    final_model <- betareg(value ~ ., data = data) 
    predictions_final_train <- predict(final_model, newdata = data)
    predictions_final_train <- predict(final_model, newdata = data)
    predictions_final_test  <- predict(final_model, newdata = test_set)
  }
  
  return(list("model" = final_model, "train_pred" = predictions_final_train,
              "test_pred" = predictions_final_test))
}