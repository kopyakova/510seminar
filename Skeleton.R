main <- function(df, cutoff = 0.1){
  cutoff = 0.1
  # DO outside function and add them as input parameters
  weather <- read.delim("~/Desktop/MAPS/Seminar/Code_1/510seminar/new_weather_cleaned.csv", sep = ",", header = TRUE) # Not imputed yet
  ovitrap_cleaned <- read.delim("~/Desktop/MAPS/Seminar/Code_1/510seminar/monthly_mosquito_per_province.csv", sep = ",", header = TRUE)
  ovitrap_original <- read.delim("~/Desktop/MAPS/Seminar/Code_1/510seminar/ovitrap_data_aggregated_per_month_per_province.csv", sep = ",", header = TRUE)
  # weather <- read.delim("new_weather_cleaned.csv", sep = ",", header = TRUE) # Not imputed yet
  # ovitrap_cleaned <- read.delim("monthly_mosquito_per_province.csv", sep = ",", header = TRUE)
  # ovitrap_original <- read.delim("ovitrap_data_aggregated_per_month_per_province.csv", sep = ",", header = TRUE)

  set.seed(510)
  
  # (1) Initial full data set with NA values - df
  full_data_NA <- fulldata(weather, ovitrap_cleaned, ovitrap_original)
  # Index column created by merging is dropped
  full_data_NA <- full_data_NA[,!(names(full_data_NA) == "X")]
  full_data_NA <- full_data_NA[,!(names(full_data_NA) == "perc")]
  # (2) Impute total df
  full_data_imputed <- imputations(5, 5, full_data_NA)
  full_data_imputed <- full_data_imputed[order(full_data_imputed$date), ]
  
  # (3) Split full data set in a training, validation and test set --> In order????????
  # Months are not taken correctly into account yet --> Anna does it in her code! --> use it!
  split_sets <- split_train_test(df = full_data_imputed, train = 0.8, validate = 0.1, chronologically = TRUE, 
                                 remove_NA = TRUE, remove_adm_date = F) # remove_adm_date = F --> do not remove them --> needed in bootstrap 
 
  training_set   <- split_sets$train
  validation_set <- split_sets$valid
  test_set       <- split_sets$test
  # training_set <- full_data_imputed[(1:(0.8*nrow(full_data_imputed))), ] # 80%
  # validation_set <- full_data_imputed[(0.8*nrow(full_data_imputed)+1):(0.9*nrow(full_data_imputed)), ] # 10%
  # test_set <- full_data_imputed[((0.9*nrow(full_data_imputed)+1):nrow(full_data_imputed)), ]# # 10%
  
  # (4) Set the imputed values in training set to NA - find a match base of date and adm
  # df_imputed[is.na(df_original_training)] = NA ONLY TRAINING
  training_set_na <- training_set
  rows            <- nrow(training_set_na)
  full_data_NA_2  <- full_data_NA[order(full_data_NA$date), ] # We totally FORGOT to oder the full_data_NA as well!
  subsss          <- full_data_NA_2[1:rows, ]
  indices         <- is.na(subsss)
  training_set_na[indices] <- NA # CAN GIVE ERROR DUE TO EXTRA COLUMN X THAT IS CREATED
  
  # (6) Add value lags to the validation and test sets 
  # ANNAAAAAA --> Not ready yet!!! 
  training_set_lag1 <- make_lags(data = training_set, weather_data = weather, id_index = "adm", date_index = "date",
                                  num_lags = 1)
  training_set_lagged <- make_lags(data = training_set_lag1, weather_data = weather, id_index = "adm", date_index = "date",
                                  num_lags = 2)
  
  valid_lag1 <- make_lags(data = validation_set, weather_data = weather, id_index = "adm", date_index = "date",
                          num_lags = 1)
  valid_lagged <- make_lags(data = valid_lag1, weather_data = weather, id_index = "adm", date_index = "date",
                          num_lags = 2)

  test_lag1 <- make_lags(data = test_set, weather_data = weather, id_index = "adm", date_index = "date",
                          num_lags = 1)
  test_lagged <- make_lags(data = test_lag1, weather_data = weather, id_index = "adm", date_index = "date",
                          num_lags = 2)
  
  # (7) Add indicators to the validation and test sets 
  training_final <- add_value_indicator(training_set_lagged, cutoff = cutoff)
  valid_final  <- add_value_indicator(valid_lagged, cutoff = cutoff) 
  test_final   <- add_value_indicator(test_lagged, cutoff = cutoff)
  
  #(8) Call first stage
  output <- first_stage(training_set_na = training_set_na, validation_set = valid_final, 
                        training_set = training_final, number_of_bootstraps = 2,          # BEFORE training_set_lagged WAS HERE :(
                        threshold_presentation = cutoff, 
                        threshold_selection = 0.5, log_transf = FALSE, weather = weather) 
  
  # THIS IS ONLY HERE FOR CHECKING WHETHER THE CODE RUNS CORRECTLY
  training_set_na = training_set_na
  validation_set = valid_final
  training_set = training_final
  number_of_bootstraps = 20
  threshold_presentation = cutoff
  threshold_selection = 0.5
  log_transf = FALSE
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Implementation of the three stages
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# FIRST STAGE 
first_stage <- function(training_set_na, validation_set, training_set, number_of_bootstraps = 100, threshold_presentation = 0.1, 
                        threshold_selection = 0.5, log_transf = FALSE, weather) {

  set.seed(510)

  # (1) Apply bootstrap on the training set
  bootstrap_samples <- bootstrap_samples(number_of_bootstraps = number_of_bootstraps, 
                                         training_set = training_set_na)
  
  # (2) Apply imputation methods, transform your dependent variable and add lagged values
  complete_samples  <- complete_samples(number_of_bootstraps = number_of_bootstraps, 
                                       threshold_presentation = threshold_presentation, 
                                       bootstrap_samples = bootstrap_samples, log_transf = log_transf, 
                                       weather = weather)
  
  temp <- complete_samples[[1]][,-c(1,2,3)]  #DO maybe fix it 
  only_covariates <- temp[ ,!(names(temp) == "value_indicator")]
  number_of_covariates <- ncol(only_covariates) 
  names_covariates     <- colnames(only_covariates)
  
  # (3) Estimate a model for each complete sample, apply variable selection and return list of 'final variables'
  final_covariates <- estimation_and_selection_process(number_of_bootstraps = number_of_bootstraps, 
                                                       threshold_selection = threshold_selection, 
                                                       number_of_covariates = number_of_covariates, 
                                                       names_covariates = names_covariates, 
                                                       complete_samples = complete_samples,
                                                       model_type = "logit")
  
  # (4) Estimate the model with imputed training set from stage 0
  logitMod     <- glm(value_indicator ~ . , data = training_set[, c(final_covariates, "value_indicator")], 
                      family=binomial(link='logit'))
  
  # (5) Determine threshold
  # (5.1) Get the value 
  predicted_probability <- logitMod$fitted.values
  
  #determine threshold2 based on F1 score or spec / sens
  threshold <- threshold_bootstrap(train = training_set, valid = validation_set,
                                   model = logitMod)
  #determine threshold2 same as threshold1
  threshold <- cutoff
  
  #WORK IN PROGRESS: evaluate threshold
  predicted_index <- ifelse(predicted_probability >= threshold, 1, 0)
  target_index    <- training_set[ ,"value_indicator"]
  accuracy        <- sum(predicted_index == target_index)/nrow(training_set)
  print(paste0("Predicted count of 1: ", sum(predicted_index == 1)))
  print(paste0("Predicted count of 0: ", sum(predicted_index == 0)))
  print(paste0("Accuracy: ", accuracy))
  
  # (5.2) Get a fraction of once per province pased on the predicted_index
  
  # (5.3) Get a fraction of once per province pased on the training_set
  
  # (5.4) Find a threshold that "optimizes" 

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
  
  # OUTPUT: 
  # (1) Provinces on risky 
  # (2) Return final logit model 
  # (3) TRAIN + VALIDATION WITH NA'S
  #   - Order full_data_NA (like what we did we the fill_data_imputed)
  #   - Determine where the NA's are in this ordered full_data_NA
  #   - Shrink data set by removing test_set part
  #   - Return this one for input in Second_Stage --> training_set_na
  return(return_list)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# SECOND STAGE 
second_stage <- function(number_of_bootstrap = 100, provinces_on_risk, threshold_presentation = 0.1, threshold_selection = 0.5, training_set_na, training_set) { 
  
  # (1) Select the 'sub-sample' from the training_set (with NA's) corresponding to those provinces which are on risk
  # Already done in previous First Stage
  training_set_na <- training_set_na[as.character(training_set_na$adm) == provinces_on_risk, ]
  
  # (2) For this 'sub-sample' apply bootstrap, impute the missing data, estimate models, do variable selection and determine the final model 
  
  bootstrap_samples <- bootstrap_samples(number_of_bootstraps = number_of_bootstraps, training_set = training_set_na)
  
  complete_samples <- complete_samples(number_of_bootstraps = number_of_bootstrap, threshold_presentation = threshold_presentation, bootstrap_samples = bootstrap_samples, log_transf = F)
  
  # number_of_covariates <- ncol((complete_samples[[1]])[, -c(1,2,ncol(complete_samples[[1]]))]) 
  # names_covariates     <- colnames(complete_samples[[1]])[-c(1,2,ncol(complete_samples[[1]]))] 
  temp <- complete_samples[[1]][ , -c(1,2,3)]  #DO maybe fix it 
  only_covariates <- temp[ , !(names(temp) == "value_indicator")]
  number_of_covariates <- ncol(only_covariates) 
  names_covariates     <- colnames(only_covariates)
  
  final_covariates <- estimation_and_selection_process(number_of_bootstraps = number_of_bootstraps, 
                                                       threshold_selection = threshold_selection, 
                                                       number_of_covariates = number_of_covariates, 
                                                       names_covariates = names_covariates, 
                                                       complete_samples = complete_samples,
                                                       model_type = "linear_regression")
  
  # (3) Return the final model
  final_model <- lm(value ~ ., data = training_set[, c(final_covariates, "value")]) 
  #klopt het dat we hier alleen maar lm doen + klopt het dat final covariates de value niet include?
  
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
complete_samples <- function(number_of_bootstraps = 100, threshold_presentation = 0.1, bootstrap_samples,
                             log_transf = F, weather) {
  
  set.seed(510) # reproducibility imputations
  complete_samples <- list()
  
  for (r in 1:number_of_bootstraps) {
   
    # r = 1
    bootstrap_sample <- bootstrap_samples[[r]]
    
    # (1) Impute ovitrap data based on the incomplete weather data (KNN)
    # (2) Impute weather data based on the incomplete weather data (model-based)
    imputed_data <- imputations(5, 5, bootstrap_sample)
    
    # (3) Add lagged values to the complete data set
    imputed_data_lag1   <- make_lags(data = imputed_data, weather_data = weather, id_index = "adm", date_index = "date",
                            num_lags = 1)
    imputed_data_lagged <- make_lags(data = imputed_data_lag1, weather_data = weather, id_index = "adm", date_index = "date",
                            num_lags = 2)
    
    # (4) Add indicators to the validation and test sets 
    # imputed_final <- add_value_indicator(imputed_data_lagged, cutoff = threshold_presentation) # ADD THIS ONE AGAIN IN CASE OF RUNNING TWO LAGS --> and remove the sentence below!
    imputed_final <- add_value_indicator(imputed_data_lagged, cutoff = threshold_presentation)
    
    # (5) Standardise the explanatory space
    standardized_data <- scale_variables(imputed_final, variables_not_to_scale = c("adm", "date", "value", "value_indicator", "longitude", "latitude"))
    
    complete_sample   <- standardized_data
    
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
      selected_model <- stage1_logit(df = complete_sample, model = "logit", include_two_way_interactions = FALSE,
                                       direction_search = "backward")
    } else if (model_type == "linear_regression") {
      # WHAT DOES THE COMPLETE_SAMPLE LOOKL LIKE (IN WHICH ORDER ARE THE VARIABLES)
      # DELET THE OVI_INDEX AND LEAVE THE MEAN_OVI
      selected_model <- stage2_regression(df = complete_sample, model = "linear_regression", include_two_way_interactions = FALSE, direction_search = "both")
      
      # lm_model <- lm(... ~ ..., data = complete_sample)
      # selected_model <- stepAIC(object = lm_model, direction = c("both"))
    } else if (model_type == "beta_regression") {
      print("ERROR: no model selcted")
    } else {
      print("ERROR: no model selcted")
    }
    
    # (3) Extract the variables which are selected and update the selected_covariates matrix
    selected_variables <- row.names(data.frame(selected_model$coefficients))[-1]   # [-1] to exclude intercept
    
    #check which variables are in the final 
    index <- !is.na(names_covariates[match(names_covariates, intersect(names_covariates, selected_variables))])
    selected_covariates[index,r] <- 1
  }
  fraction         <- rowMeans(selected_covariates)
  final_covariates <- names_covariates[fraction > threshold_selection]

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
    # print(rbind(full_months, province_month))
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
  # completedata$value <- as.numeric(as.character(completedata$value))
  # completedata$adm <- as.character(completedata$adm)
  # completedata$date <- as.Date(as.character(completedata$date))
  
  return(completedata)
} 

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

imputations <- function(K, M, completedata) { # I used the standard setup where k = 5 and m = 5
  # completedata <- fulldata(weather, ovitrap_cleaned, ovitrap_original)
  # completedata <- full_data_NA
  
  ######### Imputing ovitrap data #########
  
  ovitrap_imputed <- VIM::kNN(completedata, variable = "value", k=K, imp_var = F)
  
  ######### Imputing weather data #########
  weather_missing <- completedata[ , !(names(full_data_NA) == "value")]
  
  imputation_mice <- mice::mice(weather_missing, m = M, print = FALSE) # Impute by Multiple Imputations with Chained Equations
  imputed_weather <- mice::complete(imputation_mice)
  
  names(imputed_weather)[names(imputed_weather) == "adm_level"] <- "adm" 
  
  ovitrap_imputed <- as.data.frame(cbind(ovitrap_imputed, c(1:nrow(ovitrap_imputed))))
  names(ovitrap_imputed)[names(ovitrap_imputed) == "c(1:nrow(ovitrap_imputed))"] <- "index" 
  imputed_weather <- as.data.frame(cbind(imputed_weather, c(1:nrow(imputed_weather))))
  names(imputed_weather)[names(imputed_weather) == "c(1:nrow(imputed_weather))"] <- "index" 
  
  result <- merge(ovitrap_imputed[,c(1:3, ncol(ovitrap_imputed))], imputed_weather, by = c("adm", "date", "index"))
  result <- result[, -which(colnames(result) == "index")]
  result$value <- as.numeric(as.character(result$value))
  # result$value <- as.numeric(as.character(result$value))
  
  return(result)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

stage2_regression <- function(df, model = "linear_regression", include_two_way_interactions = FALSE,
                             direction_search = "both"){
  # direction_search = "both"
  # df <- temp
  df <- df[ ,!(names(df) == "value_indicator")] #exclude value
  df <- df[ ,!(names(df) == "adm")]
  df <- df[ ,!(names(df) == "date")]
  
  if (model == "linear_regression"){
    lsMod <- lm(value ~ ., data = df)
    # logitMod <- glm(value_indicator ~. , data =  df, family = binomial(link='logit'))
    if (include_two_way_interactions){
      selectedLsMod <- step(lsMod, direction = direction_search, scope = list(upper = ~ .+.^2,lower=~.), trace = FALSE)
      # selectedLogitMod <- step(logitMod, direction = direction_search, 
      #                         scope=list(upper = ~ .+.^2,lower=~.), trace = FALSE)
    } else {
      selectedLsMod <- step(lsMod, direction = direction_search, trace = FALSE)
      # selectedLogitMod <- step(logitMod, direction =  direction_search, trace = FALSE)
    }
    
    selected_model <- selectedLsMod
  }
  else if (model == "beta_regression"){
    # are values already [0, 1] scale or [0, 100]?
    value <- value/100
    
    # Transform according to Smithson and Verkuilen 2006, if 0 and 1.
    if (any(value==1)|any(value==0)){
      y.transf.betareg <- function(y){
        n.obs <- sum(!is.na(y))
        (y * (n.obs - 1) + 0.5) / n.obs
      }
    }
    betaMod <- betaselect(y.transf.betareg(value), df, criterion="AIC", method= direction_search)
    #look into link function
    selected_model <- betaMod
  }
  else if (model == "XGBoost"){
    predictions <- XG_fit(df, 0.8, 5, 0.1, 40)
  }
  
  return(selected_model)
}
