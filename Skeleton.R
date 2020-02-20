main <- function(df){
  
  weather <- read.delim("~/Desktop/MAPS/Seminar/Code_1/510seminar/new_weather_cleaned.csv", sep = ",", header = TRUE)
  ovitrap_cleaned <- read.delim("~/Desktop/MAPS/Seminar/Code_1/510seminar/monthly_mosquito_per_province.csv", sep = ",", header = TRUE)
  ovitrap_original <- read.delim("~/Desktop/MAPS/Seminar/Code_1/510seminar/ovitrap_data_aggregated_per_month_per_province.csv", sep = ",", header = TRUE)
  
  # (1) Initial full data set with NA values - df
  full_data_NA <- fulldata(weather, ovitrap_cleaned, ovitrap_original)[, -4] # DELETING WEIRD X COLUMN
  
  # (2) Impute total df
  full_data_imputed <- imputations(5, 5, full_data_NA)
  full_data_imputed <- full_data_imputed[order(full_data_imputed$date), ]
  
  # (4) Split full data set in a training, validation and test set --> In order????????
  training_set <- full_data_imputed[(1:(0.8*nrow(full_data_imputed))), ] # 80%
  validation_set <- full_data_imputed[(0.8*nrow(full_data_imputed)+1):(0.9*nrow(full_data_imputed)), ] # 10%
  test_set <- full_data_imputed[((0.9*nrow(full_data_imputed)+1):nrow(full_data_imputed)), ]# # 10%
  
  # (5) Set the imputed values in training set to NA - find a match base of date and adm
  # df_imputed[is.na(df_original_training)] = NA ONLY TRAINING
  training_set_na <- training_set
  rows <- nrow(training_set_na)
  subsss <- full_data_NA[c(1:rows), ]
  indices <- is.na(subsss)
  training_set_na[indices] <- NA # CAN GIVE ERROR DUE TO EXTRA COLUMN X THAT IS CREATED
  
  # (6) Add value lags to the validation and test sets 
  # ANNAAAAAA

}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Implementation of the three stages
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# FIRST STAGE 
first_stage <- function(training_set_na, validation_set, training_set, number_of_bootstraps = 100, threshold_presentation = 0.15, 
                        threshold_selection = 0.5, log_transf = FALSE) {

  set.seed(510)
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
  
  number_of_covariates <- ncol((complete_samples[[1]])[, -c(1,2,3)]) 
  names_covariates     <- colnames(complete_samples[[1]])[-c(1,2,3)] 
  
  # (3) Estimate a model for each complete sample, apply variable selection and return list of 'final variables'
  final_covariates <- estimation_and_selection_process(number_of_bootstraps = number_of_bootstraps, 
                                                       threshold_selection = threshold_selection, 
                                                       number_of_covariates = number_of_covariates, 
                                                       names_covariates = names_covariates, 
                                                       complete_samples = complete_samples,
                                                       model_type = "logit")
  
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
  
  index_of_selected_observations <- logitMod$fitted.values > threshold # DISCUSSION POINT!!!
  
  # ADD NA'S BACK TO THE MERGED DATA SET --> NEED THOSE NA'S AGAIN IN THE SECOND STAGE
  # Return both please! WE NEED THEM BOTH IN THE SECOND STAGE
  selected_data <- merged_set[index_of_selected_observations, ] # risky observations
  
  return_list <- list()
  return_list$model <- logitMod
  return_list$data  <- selected_data
  
  return(return_list)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# SECOND STAGE 
second_stage <- function(number_of_bootstrap = 100, provinces_on_risk, threshold_presentation = 0.15, threshold_selection = 0.5, training_set_na, training_set) { 
  
  # (1) Select the 'sub-sample' from the training_set (with NA's) corresponding to those provinces which are on risk
  # Already done in previous First Stage
  
  # (2) For this 'sub-sample' apply bootstrap, impute the missing data, estimate models, do variable selection and determine the final model 
  
  bootstrap_samples <- bootstrap_samples(number_of_bootstraps = number_of_bootstraps, training_set = training_set_na)
  
  complete_samples <- complete_samples(number_of_bootstraps = number_of_bootstrap, threshold_presentation = threshold_presentation, bootstrap_samples = bootstrap_samples, log_transf = F)
  
  number_of_covariates <- ncol((complete_samples[[1]])[, -c(1,2,ncol(complete_samples[[1]]))]) 
  names_covariates     <- colnames(complete_samples[[1]])[-c(1,2,ncol(complete_samples[[1]]))] 
  
  final_covariates <- estimation_and_selection_process(number_of_bootstraps = number_of_bootstraps, 
                                                       threshold_selection = threshold_selection, 
                                                       number_of_covariates = number_of_covariates, 
                                                       names_covariates = names_covariates, 
                                                       complete_samples = complete_samples,
                                                       model_type = "linear_regression")
  
  # (3) Return the final model
  final_model <- lm(... ~ ..., data = training_set)
  
  return(final_model)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# THIRD STAGE 
prediction_performance <- function(final_model, test_set) {
  
  # Apply the final_model to make predictions for those provinces which are on risk (use test_set)
  prediction_lm <- predict(object = final_model, test_set)
  
  # Comparison with original values in test_set --> compute error measures
  
}


# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Required functions to run the first, second and third stages
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Multiple bootstrap samples (with replacement) are generated and returned in one list
bootstrap_samples <- function(number_of_bootstraps = 100, training_set) {
  
  set.seed(510) # reproducibility bootstrapping
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
  
  set.seed(510) # reproducibility imputations
  complete_samples <- list()
  
  for (r in 1:number_of_bootstraps) {
    
    bootstrap_sample <- bootstrap_samples[[r]]
    
    # (1) Impute ovitrap data based on the incomplete weather data (KNN)
    # (2) Impute weather data based on the incomplete weather data (model-based)
    imputed_data <- imputations(5, 5, bootstrap_sample)
    
    # imputed_ovitrap <- "..."
    # (2) Impute weather data based on the incomplete weather data (model-based)
    # imputed_weather <- "..."
    
    # imputed_data <- df
    
    # (3) Merge data and add lagged values to the complete data set
    # ADJUST FUNCTION SUCH THAT THE INPUT IS JUST ONE DATA SET --> YOU DON'T HAVE TO MERGE IT (FLEUR ALREADY DOES THIS)
    # aggregated_data <- aggregate_data(imputed_data, log_transf = log_transf)
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
                                             number_of_covariates, names_covariates, complete_samples, model_type = "logit") {
  
  # A zero-one matrix indicating which variables were selection in each complete sample
  selected_covariates <- matrix(data = 0, nrow = number_of_covariates, ncol = number_of_bootstraps) 
  row.names(selected_covariates) <- names_covariates
  
  for (r in 1:number_of_bootstraps) {
    complete_sample <- complete_samples[[r]]
    # (1) Estimate a model for the rth sample in complete_samples + 
    # (2) Apply variable selection on the rth estimated model 
    #TODO: remove administration and date columns
    
    if (model_type == "logit") {
      selected_model <- stage1_bootstrap(df = complete_sample, model = "logit", include_two_way_interactions = FALSE,
                                       direction_search = "both")
    } 
    else if (model_type == "linear_regression") {
      # WHAT DOES THE COMPLETE_SAMPLE LOOKL LIKE (IN WHICH ORDER ARE THE VARIABLES)
      lm_model <- lm(... ~ ..., data = complete_sample)
      selected_model <- stepAIC(object = lm_model, direction = c("both"))
    }
    else if (model_type == "beta_regression") {
      
    }
    else {
      print("ERROR")
    }
    
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

fulldata <- function(weather, ovitrap_cleaned, ovitrap_original) {
  
  months <- as.matrix((unique(weather$date))[which((as.Date(unique(weather$date)) >= as.Date("2013-03-01")) & (as.Date(unique(weather$date)) < as.Date("2016-03-01")))])
  no_months <- nrow(months)
  provinces <- as.matrix(unique(ovitrap_cleaned$adm))
  
  full_months <- c(0,0,0)
  
  for (i in 1:length(provinces)) { # Check which provinces have missing values in them regarding dates
    current_province <- provinces[i]
    
    province_month <- cbind(months, current_province)
    full_months <- rbind(full_months, province_month)
  }
  
  full_months <- full_months[-1,]
  full_months <- cbind(full_months, rep(NA, nrow(full_months)))
  colnames(full_months) <- c("date", "adm", "value")
  
  for (i in 1:nrow(full_months)) {
    current_date <- full_months[i,1]
    current_province <- full_months[i,2]
    
    for (j in 1:nrow(ovitrap_cleaned)) {
      if (ovitrap_cleaned[j,1] == current_province & ovitrap_cleaned[j,2] == current_date) {
        value <- ovitrap_cleaned[j,3]
        full_months[i,3] <- value
      }
    }
  }
  
  full_months[,3] <- as.numeric(full_months[,3])
  
  zeroes <- (ovitrap_original$mean_ovi == 0)
  zeroes[is.na(zeroes)] <- FALSE
  zeroes <- ovitrap_original[zeroes,]
  
  ovitrap_original <- ovitrap_original[,-c(3,5,6)]
  
  colnames(ovitrap_original)[3] <- "value"
  
  zeroes_new <- which(as.numeric(full_months[,3]) == 0)
  
  full_months[zeroes_new,3] <- NA
  
  for (i in 1:nrow(zeroes)) {
    province <- (zeroes[i,1])
    date <- (zeroes[i,2])
    
    province_new <- as.matrix(full_months[,2])
    date_new <- as.matrix(full_months[,1])
    
    real_zero <- which((province_new == province) & (date_new == date))
    
    if (length(real_zero) > 0) {
      full_months[real_zero,3] <- 0
    }
  } 
  
  ovitrap_fulldata <- full_months
  
  names(weather)[names(weather) == "adm_level"] <- "adm" 
  
  completedata <- merge(ovitrap_fulldata, weather, by = c("adm", "date"))
  return(completedata)
} 

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

imputations <- function(K, M, completedata) { # I used the standard setup where k = 5 and m = 5
  # completedata <- fulldata(weather, ovitrap_cleaned, ovitrap_original)
  # completedata <- full_data_NA
  
  ######### Imputing ovitrap data #########
  
  ovitrap_imputed <- kNN(completedata, variable = "value", k=K, imp_var = F)
  
  ######### Imputing weather data #########
  weather_missing <- completedata[,-c(3:4)]
  
  imputation_mice <- mice(weather_missing, m = M) # Impute by Multiple Imputations with Chained Equations
  imputed_weather <- complete(imputation_mice)
  
  names(imputed_weather)[names(imputed_weather) == "adm_level"] <- "adm" 
  
  result <- merge(ovitrap_imputed[,1:3], imputed_weather, by = c("adm", "date"))
  return(result)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

