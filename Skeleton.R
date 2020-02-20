main <- function(df){
  # (1) Initial full data set with NA values - df
  # (2) Impute total df
  # (4) Split full data set in a training, validation and test set 
  # (5) Set the imputed values in training set to NA - find a match base of date and adm
  # df_imputed[is.na(df_original_training)] = NA ONLY TRAINING
  # (6) Add value lags to the validation and test sets 

}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Implementation of the three stages
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# FIRST STAGE 
first_stage <- function(training_set_na, validation_set, training_set, number_of_bootstraps = 100, threshold_presentation = 0.15, 
                        threshold_selection = 0.5, log_transf = FALSE) {

  set.seed(1234)
  # number_of_covariates <- ncol(training_set) - 1 # -1 as the response is also included in training_set --> I probably have to change this!
  # names_covariates <- colnames(training_set)[-1] # I probably have to change this!
   
  # # (1) Split data in a training and validation set
  # train_test_list <- split_train_test(df = training_set,  smpl_size = 0.85, chronologically = FALSE, 
  #                                     remove_NA = TRUE, remove_adm_date = FALSE)
  # training_set   <- train_test_list$train
  # validation_set <- train_test_list$test

  # (1) Apple bootstrap on the training set
  bootstrap_samples <- bootstrap_samples(number_of_bootstraps = number_of_bootstraps, training_set = training_set_na)
  
  # (2) Apply imputation methods, transform your dependent variable and add lagged values
  complete_samples  <- complete_samples(number_of_bootstraps = number_of_bootstraps, 
                                       threshold_presentation = threshold_presentation, 
                                       bootstrap_samples = bootstrap_samples, log_transf = log_transf)
  
  number_of_covariates <- "..." #exclude response variables 
  names_covariates     <- "..." 
  # (3) Estimate a model for each complete sample, apply variable selection and return list of 'final variables'
  final_covariates <- estimation_and_selection_process(number_of_bootstraps = number_of_bootstraps, 
                                                       threshold_selection = threshold_selection, 
                                                       number_of_covariates = number_of_covariates, 
                                                       names_covariates = names_covariates, 
                                                       complete_samples = complete_samples)
  # (4) Estimate the model with imputed training set from stage 0
  training_set <- add_value_indicator(training_set, cutoff = threshold_presentation)
  logitMod     <- glm(value_indicator ~ . , data = training_set[, final_covariates], family=binomial(link='logit'))
  
  # (5) Determine threshold
  #TO DO add function that selects threshold (ANNA)
  threshold   <- 0.6
  
  # (6) Merge validation and training sets !check if the columns are the same)
  merged_set <- rbind(training_set, validation_set)
  merged_set <- merged_set[, -which(colnames(merged_set) == "value")]
  logitMod   <- glm(value_indicator ~ . , data = merged_set[, final_covariates], family=binomial(link='logit'))
  
  index_of_selected_observations <- logitMod$fitted.values > threshold
  
  selected_data <- merged_set[index_of_selected_observations, ]
  
  return_list <- list()
  return_list$model <- logitMod
  return_list$data  <- selected_data
  
  return(return_list)
}



# SECOND STAGE 
second_stage <- function(number_of_bootstrap = 100, provinces_on_risk, threshold_presentation = 0.15, threshold_selection = 0.5, training_set) { 
  
  # (1) Select the 'sub-sample' from the training_set (with NA's) corresponding to those provinces which are on risk
  
  # (2) For this 'sub-sample' apply bootstrap, impute the missing data, estimate models, do variable selection and determine the final model 
  
  # (3) Return the final model
}




# THIRD STAGE 
prediction_performance <- function(final_model, test_set) {
  
  # Apply the final_model to make predictions for those provinces which are on risk (use test_set)
  
}


# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Required functions to run the first, second and third stages
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Multiple bootstrap samples (with replacement) are generated and returned in one list
bootstrap_samples <- function(number_of_bootstraps = 100, training_set) {
  
  set.seed(1234) # reproducibility bootstrapping
  bootstrap_samples   <- list()
  training_set_n_rows <- nrow(training_set)
  
  for (r in 1:number_of_bootstraps) {
    training_set_indices   <- sample(x = c(1:training_set_n_rows), size = training_set_n_rows, replace = TRUE)
    training_set_bootstrap <- training_set[training_set_indices, ]
    
    bootstrap_samples[[r]] <- training_set_bootstrap
  }
  
  return("bootstrap samples" = bootstrap_samples)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Each bootstrap sample is imputed, the response variable is transformed and lagged values are added
complete_samples <- function(number_of_bootstraps = 100, threshold_presentation = 0.15, bootstrap_samples,
                             log_transf = F) {
  
  set.seed(1234) # reproducibility imputations
  complete_samples <- list()
  
  for (r in 1:number_of_bootstraps) {
    
    bootstrap_sample <- bootstrap_samples[[r]]
    
    # (1) Impute ovitrap data based on the incomplete weather data (KNN)
    imputed_ovitrap <- "..."
    # (2) Impute weather data based on the incomplete weather data (model-based)
    imputed_weather <- "..."
    
    imputed_data <- df
    
    # (3) Merge data and add lagged values to the complete data set
    aggregated_data <- aggregate_data(weather = imputed_weather, ovitrap = imputed_ovitrap, log_transf = log_transf)
    
    # (4) Transform the response variable by using "threshold_presentation = 0.15" (also keep original variable for Second Stage)
    aggregated_data <- add_value_indicator(aggregated_data, cutoff = threshold_presentation)
    #returns data set with value indicator as a last column
  
    # (5) Standardise the explanatory space
    standardized_data <- scale_variables(df, variables_not_to_scale = c("adm", "date", "value", "value_indicator", "longitude", "latitude"))
    
    complete_sample <- standardized_data
    
    complete_samples[[r]] <- complete_sample
  }
  
  return("complete_samples" = complete_samples)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# For each complete sample a model is estimated, variable selection is applied and the 'final' model is returned
estimation_and_selection_process <- function(number_of_bootstraps = 100, threshold_selection = 0.5, 
                                             number_of_covariates, names_covariates, complete_samples) {
  
  # A zero-one matrix indicating which variables were selection in each complete sample
  selected_covariates <- matrix(data = 0, nrow = number_of_covariates, ncol = number_of_bootstraps) 
  row.names(selected_covariates) <- names_covariates
  
  for (r in 1:number_of_bootstraps) {
    complete_sample <- complete_samples[[r]]
    # (1) Estimate a model for the rth sample in complete_samples + 
    # (2) Apply variable selection on the rth estimated model 
    #TODO: remove administration and date columns
    selected_model <- stage1_bootstrap(df = complete_sample, model = "logit", include_two_way_interactions = FALSE,
                                       direction_search = "both")
    
    # (3) Extract the variables which are selected and update the selected_covariates matrix
    selected_variables <- row.names(data.frame(selected_model$coefficients))[-1]   # [-1] to exclude intercept
    #check which variables are in the final 
    index <- !is.na(names_covariates[match(names_covariates, intersect(names_covariates, selected_variables))])
    selected_covariates[index,r] <- 1
  }
  fraction <- rowMeans(selected_covariates)
  final_covariates <- names_covariates[fraction > threshold_selection]
  
  # # Estimate final model with final_covariates and 'an' imputed training_set
  # final_model <- "..."

  return(final_covariates)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

