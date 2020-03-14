#' Train an econometric prediction model of ovitrao indices based on weather
#'
#' @usage main_analysis(ovitrap_original, ovitrap_cleaned, weather, threshold_WHO = 0.1, 
#' threshold_selection = 0.5, number_of_bootstraps = 100, model_type = "linear_regression", 
#' two_stage = FALSE, chronologically = TRUE, save = F))
#'
#' @param ovitrap_original a numeric matrix or data frame, information about ovitrap indices, not cleaned 
#' @param ovitrap_cleaned a numeric matrix or data frame, information about ovitrap indices, cleaned 
#' @param weather a numeric matrix or data frame, information about weather
#' NOTE: DATA SETS MUST BE IN SPECIFIC FORMAT 
#' @param model_type type of madel to be evaluated. Possible values "linear_regression" and "beta_regression"
#' @param chronologically logical value, indicating if chronological order in the data set should be used
#' for splitting of training and testing sets
#' @param two_stage logical value, indicating if a two-stage model should be used
#' @param threshold_WHO fraction betweet 0 and 1, used to convert ovitrap index to indicator variable. (used in the two-stage model)
#' Parameters for the variable selection
#' @param number_of_bootstraps number of bootstrapped samples to make
#' @param threshold_selection fraction betweet 0 and 1, indicating proportion of bootstrapped samples
#' a variable must be selected in to be seelcted to the final model. Default value 0.5
#' @param save logical value, indicating if intermideary results should be saved. Full data set, 
#' imputed data, complete bootstrapped samples from stage 1, final predictors from stage 1,
#' complete bootstrapped samples from stage 2, final predictors from stage 2,
#' @param save_train_test should training and testing sets be saved as csv files
#' 
#' @return final_model 
#' @return final_covariates
#' @return predictions_train predictions for the training set
#' @return predictions_test predictions for the testing set
main_analysis <- function(ovitrap_original, ovitrap_cleaned, weather, threshold_WHO = 0.1, 
                              threshold_selection = 0.5, number_of_bootstraps = 100, 
                              model_type = "linear_regression", two_stage = FALSE,
                              chronologically = TRUE, save = F, save_train_test = T) {
  source("../Source/Auxiliary_functions.R", echo = F)
  library(VIM)          #used for knn imputation
  library(mice)         #used for mice imputation
  library(betareg)      #used for beta regression
  library(frmselection) #used for variable selection in beta regression
  library(doParallel)
  registerDoParallel(cores=4)
  # (1) Initial full data set with NA values
  full_data_NA <- fulldata(weather, ovitrap_cleaned, ovitrap_original)
  if(save){
    save(full_data_NA, file = "full_data_NA.RData")
  }

  # (2) Impute the full data set
  imputed_data      <- imputations(5, 5, full_data_NA, weather)
  if(save){
    save(imputed_data, file = "imputed_data.RData")
  }
  full_data_imputed <- imputed_data$completedata
  full_data_imputed <- full_data_imputed[order(full_data_imputed$date), ]

  # (3) Get full imputed weather data
  n_vars <- dim(weather)[2]
  weather_data_imputed <- imputed_data$imputed_weather
  weather_data_imputed <- weather_data_imputed[, 1:(n_vars)]
 
  # (4) Add value lags to the test sets 
  df_lag1    <- make_lags(data = full_data_imputed, weather_data = weather_data_imputed, 
                          id_index = "adm", date_index = "date", num_lags = 1)
  df_lagged  <- make_lags(data = df_lag1, weather_data = weather_data_imputed, 
                          id_index = "adm", date_index = "date", num_lags = 2)
  
  # (5) Add indicators
  if (two_stage){
    df_lagged <- add_value_indicator(df_lagged, cutoff = threshold_WHO)
  }
  
  
  # (6) Split full data set in a training, validation and test set  
  split_sets    <- split_train_test(df = df_lagged, train = 0.8, validate = NA, 
                                    chronologically = chronologically, 
                                    remove_NA = TRUE, remove_adm_date = F) # keep adm and date for bootstrap 
  training_final <- split_sets$train
  test_final     <- split_sets$test

  if (save_train_test){
    write.csv(training_final, file = "training_data.csv")
    write.csv(test_final, file = "test_data.csv")
  }
  
  # (7) Set the imputed values in training set to NA - find a match base of date and adm
  if (chronologically){
    training_set_na <- training_final[,1:ncol(full_data_imputed)] # drop lags and inicator
    rows            <- nrow(training_set_na)
    full_data_NA_2  <- full_data_NA[order(full_data_NA$date), ]
    subsss          <- full_data_NA_2[1:rows, ]
    indices         <- is.na(subsss)
    training_set_na[indices] <- NA # Note: can throw an error if there are extra columns in the data set
  } else {
    training_set_na <- training_final[, 1:ncol(full_data_imputed)] # drop lags and inicator
    full_data_NA_2  <- full_data_NA[split_sets$train_index, ]      #select elements that in trainin set
    indices         <- is.na(full_data_NA_2)
    training_set_na[indices] <- NA
  }
  
  # (8) Call first stage
  if (two_stage){
    output_1   <- first_stage(training_set_na = training_set_na, training_set = training_final, 
                              test_set = test_final, validation_set = NA, weather = weather_data_imputed,
                              number_of_bootstraps = number_of_bootstraps, log_transf = FALSE,    
                              threshold_WHO = threshold_WHO, threshold_selection = threshold_selection,
                              save = save)
    print(unique(test_final$adm))
    training_set_na <- output_1$training_set_risk_na
    training_final  <- output_1$training_set_risk
    test_final      <- output_1$test_set_risk
    print(unique(test_final$adm))
  }
  
  # (9) Call second stage
  output_2 <- train_regressions(training_set_na = training_set_na, training_set = training_final,
                           test_set = test_final, weather = weather_data_imputed, model_type = model_type,
                           number_of_bootstraps = number_of_bootstraps, threshold_WHO = threshold_WHO,
                           threshold_selection = threshold_selection, save = save)
  
  # (10) Prepare outputs
  return_list <- list()
  if (two_stage){
    return_list$stage1 <- output_1
    return_list$stage2 <- output_2
  } else {
    return_list <- output_2
  }
  return(return_list)
}

######### FIRST STAGE OF THE TWO STAGE MODEL #########
first_stage <- function(training_set_na, validation_set, training_set, test_set, weather, 
                        number_of_bootstraps = 20, threshold_WHO = 0.1, threshold_selection = 0.5, 
                        log_transf = FALSE, save = F) {
  
  set.seed(510)
  line_search <- function(predicted_frac, actual_frac){
    search_space <- seq(0.1, 0.9, 0.01)
    diff <- Inf
    for (i in search_space){
      predicted_index <- ifelse(predicted_frac >= i, 1, 0)
      actual_index    <- ifelse(actual_frac >= i, 1, 0)
      
      diff_new        <- sum(predicted_index != actual_index)
      print(diff_new)
      if (diff_new < diff){
        diff <- diff_new
        best_threshold <- i
      } else if (diff_new == 0){
        best_threshold <- i
        break()
      }
    }
    return(best_threshold)
  }
  
  # (1) Apply bootstrap on the training set
  bootstrap_samples <- bootstrap_samples(number_of_bootstraps = number_of_bootstraps,
                                         training_set = training_set_na)

  # (2) Apply imputation methods and add lagged values
  complete_samples  <- complete_samples(number_of_bootstraps = number_of_bootstraps,
                                        threshold_WHO = threshold_WHO,
                                        bootstrap_samples = bootstrap_samples, log_transf = log_transf,
                                        weather = weather)
  if(save){
    save(complete_samples, file = "complete_samples_stage1.RData")
  }

  # Remove unnecessry columns
  temp <- complete_samples[[1]]
  temp <- temp[ ,!(names(temp) == "adm")]
  temp <- temp[ ,!(names(temp) == "date")]
  temp <- temp[ ,!(names(temp) == "value")]
  temp <- temp[ ,!(names(temp) == "value_indicator")]
  
  only_covariates      <- colnames(temp)
  number_of_covariates <- length(only_covariates) 
  names_covariates     <- only_covariates
  
  # (3) Estimate a model for each complete sample, apply variable selection and return list of 
  # final variables
  final_covariates <- estimation_and_selection_process(number_of_bootstraps = number_of_bootstraps,
                                                       threshold_selection = threshold_selection,
                                                       number_of_covariates = number_of_covariates,
                                                       names_covariates = names_covariates,
                                                       complete_samples = complete_samples,
                                                       model_type = "logit")
  if(save){
    save(final_covariates, file = "final_covariates_stage1.RData")
  }
  
  # (4) Estimate the model with imputed training set from stage 0
  logitMod <- glm(value_indicator ~ . , data = training_set[, c(final_covariates, "value_indicator")], 
                  family = binomial(link = 'logit'))
  predictions_logitMod_train <- predict(logitMod, 
                                        newdata = training_set[, c(final_covariates, "value_indicator")], 
                                        type = "response")
  
  predictions_logitMod_test <- predict(logitMod, 
                                       newdata = test_set[, c(final_covariates, "value_indicator")], 
                                       type = "response")
  
  # (5) Determine threshold 
  provinces       <- unique(training_set$adm)
  predicted_index <- ifelse(predictions_logitMod_train >= threshold_WHO, 1, 0)
  true_index      <- training_set$value_indicator
  
  # Get a fraction of once per province based on the predicted index
  predicted_frac <- aggregate(predicted_index ~ ., data = cbind(training_set$adm, predicted_index), mean)[2]
  
  # Get a fraction of once per province based on the actual index
  actual_frac    <- aggregate(true_index ~ ., data = cbind(training_set$adm, true_index), mean)[2]
  
  threshold      <- line_search(predicted_frac, actual_frac)

  provinces_at_risk <- provinces[predicted_frac > threshold]
  
  if(length(provinces_at_risk) == length(provinces)){
    training_set_risk    <- training_set
    training_set_risk_na <- training_set_na
    test_set_risk        <- test_set
  } else {
    training_set_risk    <- training_set[training_set$adm %in% provinces_at_risk,]
    training_set_risk_na <- training_set_na[training_set_na$adm %in% provinces_at_risk,]
    test_set_risk        <- test_set[test_set$adm %in% provinces_at_risk,]
  }

  # OUTPUT
  # (1) Final model
  # (2) Selected variables
  # (3) Predictions for the training set 
  # (4) Predictions for the testing set 
  # (5) Provinces at risk 
  # (6) Selected threshold
  # (7) traingin set of risky provinces
  # (8) traingin set with NAs of risky provinces
  # (9) testing set of risky provinces

  return(list("final_logit" = logitMod, 
              "final_covariates" = final_covariates,
              "predictions_train" = predictions_logitMod_train, 
              "predictions_test" = predictions_logitMod_test,
              "risky_provinces" = provinces_at_risk,
              "threshold_risky_provinces" = threshold,
              "training_set_risk" = training_set_risk,
              "training_set_risk_na" = training_set_risk_na,
              "test_set_risk" = test_set_risk))
}

######### MODEL TRAINING #########
train_regressions <- function(number_of_bootstraps = 20, threshold_WHO = 0.1, 
                              threshold_selection = 0.5, training_set_na, training_set, test_set, 
                              weather, model_type = "linear_regression", save = F) { 
  set.seed(510)
  # (1) Apply bootstrap on the training set
  bootstrap_samples <- bootstrap_samples(number_of_bootstraps = number_of_bootstraps,
                                         training_set = training_set_na)

  # (2) Apply imputation methods and add lagged values
  complete_samples <- complete_samples(number_of_bootstraps = number_of_bootstraps,
                                       threshold_WHO = threshold_WHO,
                                       bootstrap_samples = bootstrap_samples, log_transf = F,
                                       weather = weather)
  if(save){
    save(complete_samples, file = "complete_boot_samples_stage2.RData")
  }

  # (3) Get information about covariates
  temp <- complete_samples[[1]]#[,-c(1,2,3, ncol(complete_samples[[1]]))]  #DO maybe fix it 
  temp <- temp[ ,!(names(temp) == "adm")]
  temp <- temp[ ,!(names(temp) == "date")]
  temp <- temp[ ,!(names(temp) == "value")]
  temp <- temp[ ,!(names(temp) == "value_indicator")]
  only_covariates <- colnames(temp)
  
  number_of_covariates <- length(only_covariates) 
  names_covariates     <- only_covariates
  
  # (4) Select the variables in all imputed data sets and return a final model
  final_covariates <- estimation_and_selection_process(number_of_bootstraps = number_of_bootstraps, 
                                                       threshold_selection = threshold_selection, 
                                                       number_of_covariates = number_of_covariates, 
                                                       names_covariates = names_covariates, 
                                                       complete_samples = complete_samples,
                                                       model_type = model_type)
  if(save){
    save(final_covariates, file = "final_covariates_stage2.RData")
  }

  # (5) Return the final model
  
  if (model_type == "linear_regression"){
    final_model <- lm(value ~ ., data = training_set[, c(final_covariates, "value")]) 
    
    predictions_final_train <- predict(final_model, newdata = training_set[, c(final_covariates, "value")])
    predictions_final_test  <- predict(final_model, newdata = test_set[, c(final_covariates, "value")])
    
  } else if (model_type == "beta_regression") {
    # Transform according to Smithson and Verkuilen 2006, if 0 and 1.
    if (any(training_set$value==1)||any(training_set$value==0)){
      n.obs              <- sum(!is.na(training_set$value))
      training_set$value <- ((training_set$value * (n.obs - 1) + 0.5) / n.obs )/100
    }
    final_model <- betareg(value ~ ., data = training_set[, c(final_covariates, "value")], link = "logit") 
    
    predictions_final_train <- predict(final_model, newdata = training_set[, c(final_covariates, "value")])
    predictions_final_test  <- predict(final_model, newdata = test_set[, c(final_covariates, "value")])
  }
  
  # OUTPUT
  # (1) Final model
  # (2) Selected variables
  # (3) Predictions for the training set 
  # (4) Predictions for the testing set 
  return(list("final_lm" = final_model,
              "final_covariates" = final_covariates,
              "predictions_test" = predictions_final_test, 
              "predictions_train" = predictions_final_train))
}  
