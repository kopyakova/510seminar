#' Train an econometric prediction model of ovitrao indices based on weather
#'
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
#' 
#' @return 
main_analysis <- function(ovitrap_original, ovitrap_cleaned, weather, threshold_WHO = 0.1, 
                              threshold_selection = 0.5, number_of_bootstraps = 100, 
                              model_type = "linear_regression", two_stage = FALSE,
                              chronologically = TRUE, save = F) {
  library(VIM)
  library(mice)
  library(betareg)
  library(doParallel)
  registerDoParallel(cores=4)
  # (1) Initial full data set with NA values
  #TODO uncomment
  #full_data_NA <- fulldata(weather, ovitrap_cleaned, ovitrap_original)
  # if(save){
  #   save(full_data_NA, file = "full_data_NA.RData")
  # }
  
  # (2) Impute the full data set 
  
  #TODO uncomment
  # imputed_data      <- imputations(5, 5, full_data_NA, weather)
  # if(save){
  #   save(imputed_data, file = "imputed_data.RData")
  # }
  full_data_imputed <- imputed_data$completedata
  full_data_imputed <- full_data_imputed[order(full_data_imputed$date), ]
  
  # (3) Get full imputed weather data
  n_vars <- dim(weather)[2]
  weather_data_imputed <- imputed_data$imputed_weather
  weather_data_imputed <- weather_data_imputed[, 1:(n_vars)] #skip long and lat n_vars > n_vars -2
  
  # (4) Add value lags to the test sets 
  df_lag1    <- make_lags(data = full_data_imputed, weather_data = weather_data_imputed, 
                          id_index = "adm", date_index = "date", num_lags = 1)
  df_lagged  <- make_lags(data = df_lag1, weather_data = weather_data_imputed, 
                          id_index = "adm", date_index = "date", num_lags = 2)
  
  # (5) Add indicators to the validation and test sets 
  df_lagged_ind <- add_value_indicator(df_lagged, cutoff = threshold_WHO)
  
  # (6) Split full data set in a training, validation and test set  
  split_sets    <- split_train_test(df = df_lagged_ind, train = 0.8, validate = NA, 
                                    chronologically = chronologically, 
                                    remove_NA = TRUE, remove_adm_date = F) # keep adm and date for bootstrap 
  training_final <- split_sets$train
  test_final     <- split_sets$test
  
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
    return_list$stage2 <- output_2
  }
  return(return_list)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# FIRST STAGE OF THE TWO STAGE MODEL
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
first_stage <- function(training_set_na, validation_set, training_set, test_set, weather, 
                        number_of_bootstraps = 20, threshold_WHO = 0.1, threshold_selection = 0.5, 
                        log_transf = FALSE, save = F) {
  
  set.seed(510)
  #TODO uncomment
  # # (1) Apply bootstrap on the training set
  # bootstrap_samples <- bootstrap_samples(number_of_bootstraps = number_of_bootstraps,
  #                                        training_set = training_set_na)
  # 
  # # (2) Apply imputation methods and add lagged values
  # complete_samples  <- complete_samples(number_of_bootstraps = number_of_bootstraps,
  #                                       threshold_WHO = threshold_WHO,
  #                                       bootstrap_samples = bootstrap_samples, log_transf = log_transf,
  #                                       weather = weather)
  # if(save){
  #   save(complete_samples, file = "complete_boot_samples_stage1.RData")
  # }
  
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
  
  # ------------------------------------------------------------------------------------------------------------
  #TODO fix this mess
  # # (5) Determine threshold
   
  # # (5.2) Get a fraction of once per province pased on the predicted_index
  # 
  # # (5.3) Get a fraction of once per province pased on the training_set
  # 
  # # (5.4) Find a threshold that "optimizes" 
  # 
  # # (6) Merge validation and training sets !check if the columns are the same)
  # merged_set <- rbind(training_set, validation_set)
  # merged_set <- merged_set[, -which(colnames(merged_set) == "value")]
  # logitMod   <- glm(value_indicator ~ . , data = merged_set[, final_covariates], family=binomial(link='logit'))
  # 
  # index_of_selected_observations <- logitMod$fitted.values > threshold # DISCUSSION POINT!!!
  # 
  # # ADD NA'S BACK TO THE MERGED DATA SET --> NEED THOSE NA'S AGAIN IN THE SECOND STAGE
  # # Return both please! WE NEED THEM BOTH IN THE SECOND STAGE
  # selected_data <- merged_set[index_of_selected_observations, ] # risky observations
  # (1) Select the 'sub-sample' from the training_set (with NA's) corresponding to those provinces which are on risk
  # Already done in previous First Stage
  # training_set_na <- training_set_na[as.character(training_set_na$adm) == provinces_on_risk, ]
  
  # ------------------------------------------------------------------------------------------------------------
  
  # return_list <- list()
  # return_list$model <- logitMod
  # return_list$data  <- selected_data
  
  # OUTPUT
  # (1) Final model
  # (2) Selected variables
  # (3) Predictions for the training set 
  # (4) Predictions for the testing set 
  
  #TODO add 
  # (1) Provinces at risk 
  # (2) Selected threshold
  # (3) traingin sets + testing sets for the second stage
  #   - Order full_data_NA (like what we did we the fill_data_imputed)
  #   - Determine where the NA's are in this ordered full_data_NA
  #   - Shrink data set by removing test_set part
  #   - Return this one for input in Second_Stage --> training_set_na
  
  return(list("final_logit" = logitMod, 
              "final_covariates" = final_covariates,
              "predictions_train" = predictions_logitMod_train, 
              "predictions_test" = predictions_logitMod_test))
}
# if(save){
#   save(complete_samples, file = "complete_boot_samples_stage1.RData")
# }
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# MODEL TRAINING
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train_regressions <- function(number_of_bootstraps = 20, threshold_WHO = 0.1, 
                              threshold_selection = 0.5, training_set_na, training_set, test_set, 
                              weather, model_type = "linear_regression", save = F) { 
  set.seed(510)
  ## (1) Apply bootstrap on the training set
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
    #Transfrom the values of that are exactly 0 or 100
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

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Multiple bootstrap samples (with replacement) are generated and returned in one list
bootstrap_samples <- function(number_of_bootstraps = 100, training_set) {
  
  set.seed(510) # reproducibility bootstrapping
  training_set_n_rows <- nrow(training_set)
  print("Creating bootstraped samples ...")
  bootstrap_samples <- foreach(r = 1:number_of_bootstraps)  %dopar%  {
    training_set_indices   <- sample(x = c(1:training_set_n_rows), 
                                     size = training_set_n_rows, replace = TRUE)
    training_set_bootstrap <- training_set[training_set_indices, ]
  }
  return("bootstrap samples" = bootstrap_samples)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Each bootstrap sample is imputed and lagged values are added
complete_samples <- function(number_of_bootstraps = 100, threshold_WHO = 0.1, bootstrap_samples,
                             log_transf = F, weather) {
  
  set.seed(510) # reproducibility imputations
  complete_samples <- list()
  print("Completing bootstrapped samples...")
  print("(Imputing and adding laggs)")
  complete_samples <- foreach(r = 1:number_of_bootstraps)  %dopar% {
    bootstrap_sample <- bootstrap_samples[[r]]
    # (1) Impute ovitrap data based on the incomplete weather data (KNN)
    # + Impute weather data based on the incomplete weather data (model-based)
    imputed_data <- (imputations(5, 5, bootstrap_sample, weather))$completedata 
    
    # (2) Add lagged values to the complete data set
    imputed_data_lag1   <- make_lags(data = imputed_data, weather_data = weather, 
                                     id_index = "adm", date_index = "date",
                                     num_lags = 1, imputation = TRUE)
    
    imputed_data_lagged <- make_lags(data = imputed_data_lag1, weather_data = weather, 
                                     id_index = "adm", date_index = "date",
                                     num_lags = 2, imputation = TRUE)
    
    # (4) Add indicators to the validation and test sets 
    imputed_final   <- add_value_indicator(imputed_data_lagged, cutoff = threshold_WHO)
    
    complete_sample <- imputed_final
  }
  
  return("complete_samples" = complete_samples)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# For each complete sample a model is estimated, variable selection is applied and the 'final' model is returned
estimation_and_selection_process <- function(number_of_bootstraps = 100, threshold_selection = 0.5, 
                                             number_of_covariates, names_covariates, complete_samples, 
                                             model_type = "logit") {
  
  # A zero-one matrix indicating which variables were selection in each complete sample
  selected_covariates            <- matrix(data = 0, nrow = number_of_covariates, 
                                           ncol = number_of_bootstraps) 
  row.names(selected_covariates) <- names_covariates
  print("Running selection of variables...")
  for (r in 1:number_of_bootstraps) {
    #print(r)
    complete_sample <- complete_samples[[r]]
    # (1) Estimate a model for the rth sample in complete_samples + 
    # (2) Apply variable selection on the rth estimated model 
    if (model_type == "logit") {
      selected_model <- stage1_logit(df = complete_sample, model = "logit", 
                                     include_two_way_interactions = FALSE,
                                     direction_search = "backward")
      
    } else if (model_type == "linear_regression") {
      selected_model <- stage2_regression(df = complete_sample, model = "linear_regression", 
                                          include_two_way_interactions = FALSE, 
                                          direction_search = "backward")
      
    } else if (model_type == "beta_regression") {
      selected_model <- stage2_regression(df = complete_sample, model = "beta_regression", 
                                          include_two_way_interactions = FALSE, 
                                          direction_search = "backward")
      
    } else {
      print("ERROR: no model selcted")
    }
    
    # (3) Extract the variables which are selected and update the selected_covariates matrix
    if (model_type == "beta_regression") {
      selected_variables <- selected_model$variable
    }
    else {
      selected_variables <- row.names(data.frame(selected_model$coefficients))[-1]   # [-1] to exclude intercept
    }
    #check which variables are in the final 
    index <- !is.na(names_covariates[match(names_covariates, intersect(names_covariates, selected_variables))])
    selected_covariates[index,r] <- 1
  }
  
  fraction         <- rowMeans(selected_covariates)
  final_covariates <- names_covariates[fraction > threshold_selection]
  return(final_covariates)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#create a full data set
fulldata <- function(weather, ovitrap_cleaned, ovitrap_original) {
  
  months <- as.matrix((unique(weather$date))[which((as.Date(unique(weather$date)) >= as.Date("2013-03-01")) & (as.Date(unique(weather$date)) < as.Date("2016-03-01")))])
  no_months <- nrow(months)
  provinces <- as.matrix(unique(ovitrap_cleaned$adm))
  
  full_months <- c(0,0,0)
  print("fulldata")
  for (i in 1:length(provinces)) { # Check which provinces have missing values in them regarding dates
    current_province <- provinces[i]
    print(i)
    province_month <- cbind(months, current_province)
    full_months <- rbind(full_months, province_month)
  }
  
  full_months <- full_months[-1,]
  full_months <- cbind(full_months, rep(NA, nrow(full_months)))
  colnames(full_months) <- c("date", "adm", "value")
  print("months")
  for (i in 1:nrow(full_months)) {
    print(i)
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
  print("zeros")
  for (i in 1:nrow(zeroes)) {
    print(i)
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

imputations <- function(K, M, completedata, weather) { # I used the standard setup where k = 5 and m = 5
  # completedata <- fulldata(weather, ovitrap_cleaned, ovitrap_original)
  # completedata <- full_data_NA
  
  ######### Imputing ovitrap data #########
  
  ovitrap_imputed <- kNN(completedata, variable = "value", k=K, imp_var = F)
  
  ######### Imputing weather data #########
  weather_missing <- completedata[,  !(names(completedata) == "value")]
  weather_missing_full <- weather
  
  imputation_mice <- mice(weather_missing, m = M, print = FALSE) # Impute by Multiple Imputations with Chained Equations
  imputation_mice_full <- mice(weather_missing_full, m = M, print = FALSE) 
  
  imputed_weather <- complete(imputation_mice)
  imputed_weather_full <- complete(imputation_mice_full)
  
  names(imputed_weather)[names(imputed_weather) == "adm_level"] <- "adm" 
  names(imputed_weather_full)[names(imputed_weather_full) == "adm_level"] <- "adm" 
  
  #imputed_weather_threeyears <- imputed_weather[which((as.Date(weather$date) >= as.Date("2013-03-01")) & (as.Date(weather$date) < as.Date("2016-03-01"))),]
  #provinces <- unique(completedata$adm)
  #imputed_weather_threeyears <- imputed_weather_threeyears[imputed_weather_threeyears$adm %in% as.character(provinces), ]
  
  ovitrap_imputed <- as.data.frame(cbind(ovitrap_imputed, c(1:nrow(ovitrap_imputed))))
  names(ovitrap_imputed)[names(ovitrap_imputed) == "c(1:nrow(ovitrap_imputed))"] <- "index" 
  imputed_weather <- as.data.frame(cbind(imputed_weather, c(1:nrow(imputed_weather))))
  names(imputed_weather)[names(imputed_weather) == "c(1:nrow(imputed_weather))"] <- "index" 
  
  result <- merge(ovitrap_imputed[, c(1:3, ncol(ovitrap_imputed))], imputed_weather, by = c("adm", "date", "index"))
  
  result <- result[,  !(names(result) == "index")]
  result <- result[,  !(names(result) == "X")]
  imputed_weather <- imputed_weather[, !(names(imputed_weather) == "X")]
  imputed_weather_full <- imputed_weather_full[, !(names(imputed_weather_full) == "X")]
  
  result$value <- as.numeric(as.character(result$value))
  
  return(list("completedata" = result, "imputed_weather" = imputed_weather_full))
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

stage2_regression <- function(df, model = "linear_regression", include_two_way_interactions = FALSE,
                              direction_search = "both"){
  # direction_search = "both"
  # create replicate of full df (XGBoost does splitting itself)
  df_full <- df
  # df <- temp
  value <- df[,(names(df)== 'value')]
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
    df$value <- df$value/100
    
    # Transform according to Smithson and Verkuilen 2006, if 0 and 1.
    if (any(df$value==1)||any(df$value==0)){
      n.obs <- sum(!is.na(df$value))
      df$value <- (df$value * (n.obs - 1) + 0.5) / n.obs
    }
    betaMod <- betaselect(df[,!(names(df) == "value")], df$value, criterion="AIC", method= direction_search)
    #look into link function
    selected_model <- betaMod
  }
  else if (model == "XGBoost"){
    predictions <- XG_fit(df_full, 0.8, 4, 0.1, 40)
    selected_model <- predictions
  }
  else if (model == "CatBoost"){
    #work in progress
  }
  return(selected_model)
}
