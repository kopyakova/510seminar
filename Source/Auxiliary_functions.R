#' Auxiliary functions for the econometric models

######### BOOTSTRAP SAMPLES #########
#Multiple bootstrap samples (with replacement) are generated and returned in one list 
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

######### COMPLET DATA SETS #########
# Each bootstrap sample is imputed and lagged values are added 
complete_samples <- function(number_of_bootstraps = 100, threshold_WHO = 0.1, bootstrap_samples,
                             log_transf = F, weather) {
  
  set.seed(510) # reproducibility imputations
  complete_samples <- list()
  print("Completing bootstrapped samples... (Imputing and adding laggs)")
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

######### SELECT VARIABLES #########
# For each complete sample a model is estimated, variable selection is applied,
# and the final model is returned
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
######### FULL DATA SET #########
fulldata <- function(weather, ovitrap_cleaned, ovitrap_original) {
  #select the date from 2013-03-01 to 2013-03-01
  months    <- as.matrix((unique(weather$date))[which((as.Date(unique(weather$date)) >= as.Date("2013-03-01"))
                                                      & (as.Date(unique(weather$date)) < as.Date("2016-03-01")))])
  no_months <- nrow(months)
  provinces <- as.matrix(unique(ovitrap_cleaned$adm))
  print("Creating a full data set...")
  full_months <- c(0,0,0)
  
  # Check which provinces have missing values in the selected dates
  for (i in 1:length(provinces)) { 
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
  
  #add relevant zeroes back to the data
  zeroes <- (ovitrap_original$mean_ovi == 0)
  zeroes[is.na(zeroes)] <- FALSE
  zeroes <- ovitrap_original[zeroes,]
  
  ovitrap_original <- ovitrap_original[,-c(3,5,6)]
  
  colnames(ovitrap_original)[3] <- "value"
  
  zeroes_new <- which(as.numeric(full_months[,3]) == 0)
  
  full_months[zeroes_new,3] <- NA
  
  for (i in 1:nrow(zeroes)) {
    province <- (zeroes[i,1])
    date     <- (zeroes[i,2])
    
    province_new <- as.matrix(full_months[,2])
    date_new     <- as.matrix(full_months[,1])
    
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
######### MAKE LAGS #########
# Create a data frame with lagged values
# data matrix or data frame of interest
# id_index name of the variable that has crossectional index
# date_index name of the variable that has time series index
# num_lags indicates the "distance" in time of the lag. Num_lags 1 is the previous month
#' vars_to_be_lagged list of variable that must be lagged
make_lags <- function(data, weather_data, id_index = "adm", date_index = "date", num_lags = 1, imputation = FALSE,
                      vars_to_be_lagged =c("ls_temp_day", "ls_temp_night", "evi", "humid", 
                                           "new_perc", "soil_moist", "soil_temp", "ns_temp", "wind_speed")){
  set.seed(510)

  data  <- data[order(data[,id_index], data[,date_index]),]
  original_data <- data
  original_data <- original_data[!duplicated(original_data[,c(1,2)]), ]
  
  
  #Check if weather has the right label for administration
  if (!("adm" %in% colnames(weather_data))){
    index <- which(colnames(weather_data) == "adm_level")
    colnames(weather_data)[index] <- "adm"
  }
  
  #If lags are made on the bootstrapped data not all month are present in the data set
  #Thus all weather lags are taken from the weather file
  if(imputation){
    #count number of times the obesvations are repeted
    count <- plyr:: count(data[,c(1,2)])
    count <- as.matrix(count$freq)
    data  <- data.frame(data[,c(id_index, date_index, vars_to_be_lagged)]) #select only relevant variables
    data  <- data[!duplicated(data[,c(1,2)]), ]  #remove duplicate rows
    
    p = length(vars_to_be_lagged)
    n = dim(data)[1]
    
    #create data frame that will be filled with lags
    lagged_vars <- data.frame(matrix(NA, n, p + 2))
    colnames(lagged_vars) <- c(id_index, date_index, paste0(vars_to_be_lagged, "_lag_", num_lags))
    lagged_vars[,c(id_index, date_index)] <- data[,c(id_index, date_index)]
    
    #iterare over uniqe observations
    for (i in 1:n){
      adm  <- as.character(data[i,]$adm) 
      date <- as.character(data[i,]$date)
      split_date <- strsplit(date, "-")
      month      <- as.numeric(split_date[[1]][2])
      if (num_lags == 1){
        if (month == 1){ # lag is December previous year
          year <- as.numeric(split_date[[1]][1])
          date_lag_1 <- paste0(year-1, "-", 12, "-", split_date[[1]][3])
        } else if ((month-1) > 9){
          date_lag_1 <- paste0(split_date[[1]][1], "-", month-1, "-", split_date[[1]][3])
        } else {
          date_lag_1 <- paste0(split_date[[1]][1], "-0", month-1, "-", split_date[[1]][3])
        }
        #select weater variables for relevant province
        subset <- weather_data[weather_data$adm == adm,] 
        #select weater variables in the relevant province for relevant date
        subset <- subset[subset$date == date_lag_1, ]
        subset <- subset[, vars_to_be_lagged] #drop unnecessary variables
        lagged_vars[i,3:(p+2)] <- subset
      } else {
        year <- as.numeric(split_date[[1]][1])
        if (month == 1){
          date_lag_2 <- paste0(year-1, "-", 11, "-", split_date[[1]][3])
        } else if (month == 2){
          date_lag_2 <- paste0(year-1, "-", 12, "-", split_date[[1]][3])
        } else if ((month-2) > 9){
          date_lag_2 <- paste0(split_date[[1]][1], "-", month-2, "-", split_date[[1]][3])
        } else {
          date_lag_2 <- paste0(split_date[[1]][1], "-0", month-2, "-", split_date[[1]][3])
        }
        subset <-  weather_data[weather_data$adm == adm,] 
        subset <- subset[subset$date == date_lag_2, ]
        subset <- subset[, vars_to_be_lagged] #drop unnecessary variables
        lagged_vars[i,3:(p+2)] <- subset
      }
    }
    
    combined_data <- merge(original_data, lagged_vars, by=c("adm","date"))
    combined_data <- cbind(combined_data, count)
    combined_data <- combined_data[rep(row.names(combined_data), combined_data$count), ]
    combined_data <- combined_data[, (names(combined_data) != "count")]
  } else {
    data <- data.frame(data[,c(id_index, date_index, vars_to_be_lagged)])
    data <- data[order(data[,id_index], data[,date_index]),]
    p = length(vars_to_be_lagged)
    n = dim(data)[1]
    lagged_vars <- data.frame(matrix(NA, n, p + 2))
    colnames(lagged_vars) <- c(id_index, date_index, paste0(vars_to_be_lagged, "_lag_", num_lags))
    lagged_vars[,c(id_index, date_index)] <- data[,c(id_index, date_index)]
    for (j in 3:(p+2)){
      for (i in 1:n){
        if (i - num_lags > 0){
          if (data[i-num_lags,"adm"] == lagged_vars[i, "adm"]){
            lagged_vars[i, j] <- data[i-num_lags,j]
          }
        }
      }
    }
    ####Fix NA values in the first num_lags ####
    df_NA <- lagged_vars[rowSums(is.na(lagged_vars)) > 2, ]
    
    missing_months <- unique(as.Date(df_NA[,"date"]))
    n_months       <- length(missing_months)
    weather_data   <- weather_data[,c(id_index, date_index, vars_to_be_lagged)]
    dates          <- unique(original_data$date[order((original_data$date))])
    date_1     <- as.character(dates[1])
    date_2     <- as.character(dates[2])
    if(num_lags == 1){
      #Find dates, that correspond to previous months
      split_date <- strsplit(date_1, "-")
      month_1    <- as.numeric(split_date[[1]][2])
      if ((month_1-1) > 9){
        date1_lag_1 <- paste0(split_date[[1]][1], "-", month_1-1, "-", split_date[[1]][3])
      } else if (month_1 == 1){
        year <- as.numeric(split_date[[1]][1])
        date1_lag_1 <- paste0(year-1, "-", 12, "-", split_date[[1]][3])
      } else {
        date1_lag_1 <- paste0(split_date[[1]][1], "-0", month_1-1, "-", split_date[[1]][3])
      }

      #find weather data for the missing month
      weather_month_lag1  <- weather_data[as.Date(weather_data$date) == date1_lag_1, ]
      
      #merge the data set
      missing_data <- df_NA[as.Date(df_NA$date) == date_1,]
      missing_data <- missing_data[,c(1,2)]
      
      merged <- merge(missing_data, weather_month_lag1, by=c("adm"))
      merged <- merged[, -c(3)] #drop second date
      df_NA[as.Date(df_NA$date) == date_1,] = merged
    } else {
      #Find dates, that correspond to two previous months
      split_date <- strsplit(date_2, "-")
      month_2    <- as.numeric(split_date[[1]][2])
      if (month_2 == 2){
        year <- as.numeric(split_date[[1]][1])
        date2_lag_1 <- paste0(year-1, "-", 11, "-", split_date[[1]][3])
        date2_lag_2 <- paste0(year-1, "-", 12, "-", split_date[[1]][3])
      } else if (month_2 == 1){
        year <- as.numeric(split_date[[1]][1])
        date2_lag_1 <- paste0(year-1, "-", 10, "-", split_date[[1]][3])
        date2_lag_2 <- paste0(year-1, "-", 11, "-", split_date[[1]][3])
      } else {
        if ((month_2-2) > 9){
          date2_lag_2 <- paste0(split_date[[1]][1], "-", month_2-2, "-", split_date[[1]][3])
        } else {
          date2_lag_2 <- paste0(split_date[[1]][1], "-0", month_2-2, "-", split_date[[1]][3])
        }
        
        if ((month_2-3) > 9){
          date2_lag_1 <- paste0(split_date[[1]][1], "-", month_2-3, "-", split_date[[1]][3])
        } else {
          date2_lag_1 <- paste0(split_date[[1]][1], "-0", month_2-3, "-", split_date[[1]][3])
        }
      }
      
      for (i in 1:n_months){
        months <- missing_months[i]
        if (months == date_1){

          weather_month_lag1  <- weather_data[as.Date(weather_data$date) == date2_lag_1,]
          
          #merge the data
          missing_data <- df_NA[as.Date(df_NA$date) == date_1,]
          missing_data <- missing_data[,c(1,2)]
          
          merged <- merge(missing_data, weather_month_lag1, by=c("adm"))
          merged <- merged[, -c(3)] #drop second date
          df_NA[as.Date(df_NA$date) == date_1,] = merged
        }
        if (months == date_2){
          
          weather_month_lag2  <- weather_data[as.Date(weather_data$date) == date2_lag_2,]
          
          #merge the data
          missing_data <- df_NA[as.Date(df_NA$date) == date_2,]
          missing_data <- missing_data[,c(1,2)]
          
          merged <- merge(missing_data, weather_month_lag2, by=c("adm"))
          merged <- merged[, -c(3)] #drop second date
          df_NA[as.Date(df_NA$date) == date_2,] = merged
        }
      }
    }
    lagged_vars[rowSums(is.na(lagged_vars)) > 2, ] <- df_NA
    combined_data <- merge(original_data, lagged_vars, by=c("adm","date"))
  }
  
  return(combined_data)
}
######### IMPUTE THE DATA SET #########
imputations <- function(K = 5, M = 5, completedata, weather) {
  print("Imputing the data sets...")
  # Imputing ovitrap data
  ovitrap_imputed <- kNN(completedata, variable = "value", k=K, imp_var = F)
  
  # Imputing weather data by Multiple Imputations with Chained Equations
  weather_missing      <- completedata[,  !(names(completedata) == "value")]
  weather_missing_full <- weather
  
  imputation_mice      <- mice(weather_missing, m = M, print = FALSE) 
  imputation_mice_full <- mice(weather_missing_full, m = M, print = FALSE) 
  
  imputed_weather      <- complete(imputation_mice)
  imputed_weather_full <- complete(imputation_mice_full)
  
  names(imputed_weather)[names(imputed_weather) == "adm_level"]           <- "adm" 
  names(imputed_weather_full)[names(imputed_weather_full) == "adm_level"] <- "adm" 
  
  ovitrap_imputed <- as.data.frame(cbind(ovitrap_imputed, c(1:nrow(ovitrap_imputed))))
  names(ovitrap_imputed)[names(ovitrap_imputed) == "c(1:nrow(ovitrap_imputed))"] <- "index" 
  imputed_weather <- as.data.frame(cbind(imputed_weather, c(1:nrow(imputed_weather))))
  names(imputed_weather)[names(imputed_weather) == "c(1:nrow(imputed_weather))"] <- "index" 
  
  result <- merge(ovitrap_imputed[, c(1:3, ncol(ovitrap_imputed))], 
                  imputed_weather, by = c("adm", "date", "index"))
  
  result <- result[,  !(names(result) == "index")]
  result <- result[,  !(names(result) == "X")]
  imputed_weather      <- imputed_weather[, !(names(imputed_weather) == "X")]
  imputed_weather_full <- imputed_weather_full[, !(names(imputed_weather_full) == "X")]
  
  result$value <- as.numeric(as.character(result$value))
  
  return(list("completedata" = result, "imputed_weather" = imputed_weather_full))
}
######### SPLIT TRAINING AND TESTING SETS #########
# Split data in training and testing set
# df data frame to be split
# train which fraction should be used for training set
# validate which fraction should be used for validation set if NA there will be no validation set in the output
# chronologically should the sample be split chronologically
# remove_NA should rows with NA values (for lags) be removed
# remove_adm_date should the columns with administration and date be removed
# list with two data frames 
split_train_test <- function(df, train = 0.8, validate = NA, chronologically = TRUE, remove_NA = TRUE,
                             remove_adm_date = TRUE){
  set.seed(510)
  return_list <- list()
  n <- nrow(df)
  
  if (remove_NA){ #drop NA
    df <- df[rowSums(is.na(df)) == 0, ] 
  }
  
  #if validation fraction is not provided split the data into training and testing sets only
  if (is.na(validate)){
    smp_size <- floor(train * n)
    #split data in training and testing sets chronologically
    if (chronologically){ 
      df <- df[order(df$date), ]
      split_date <- as.Date(df[smp_size,"date"])
      train <- df[as.Date(df[,"date"]) < as.Date(split_date), ]
      test  <- df[as.Date(df[,"date"]) >= as.Date(split_date), ]
      
    } 
    #split data in training and testing sets randomly
    else{ 
      train_index <- sample(seq_len(n), size = smp_size)
      train       <- df[train_index, ]
      test        <- df[-train_index, ]
      
      
      return_list$train_index <- train_index
    }
  } 
  #if validation fraction is  povided split the data into training, valiation and testing sets only
  else {
    fraction_train <- train
    fraction_valid <- validate
    fraction_test  <- 1 - train - validate
    if (fraction_test < 0){
      stop("fractions must sum up to one")
    }
    
    #compute sample sizes
    sample_train <- floor(fraction_train   * n)
    sample_valid <- floor((fraction_valid) * n)
    sample_test  <- floor((fraction_test)  * n)
    
    #split data in training, validation and testing sets chronologically
    if (chronologically){ 
      sample_valid <- floor((fraction_train + fraction_valid) * n)
      df <- df[order(df$date), ]
      
      #first split
      split_date <- as.Date(df[sample_train,"date"])
      train      <- df[as.Date(df[,"date"]) < as.Date(split_date), ]
      #data frame without training set
      df2   <- df[as.Date(df[,"date"]) >= as.Date(split_date), ]
      
      #second split
      split_date_2 <- as.Date(df[sample_valid,"date"])
      
      valid <- df2[as.Date(df2[,"date"]) < as.Date(split_date_2), ]
      test  <- df2[as.Date(df2[,"date"]) >= as.Date(split_date_2), ]
      
    }
    #split data in training, validation and testing sets randomly
    else{
      #find indices
      #avoid overlapping subsets of indices via setdiff
      ind_train     <- sort(sample(seq_len(n), size=sample_train))
      ind_not_train <- setdiff(seq_len(n), ind_train)
      ind_valid     <- sort(sample(ind_not_train, size=sample_valid))
      ind_test      <- setdiff(ind_not_train, ind_valid)
      
      train <- df[ind_train, ]
      valid <- df[ind_valid, ]
      test  <- df[ind_test, ]
      
      return_list$train_index <- ind_train
    }
  }
  
  if (is.na(validate)){
    
    #remove date and administration
    if(remove_adm_date){
      train[,c("adm","date")] <- list(NULL)
      test[,c("adm","date")] <- list(NULL)
    }
    
    return_list$train <- train
    return_list$test  <- test
    
  } else {
    #remove date and administration
    if(remove_adm_date){
      train[,c("adm","date")] <- list(NULL)
      valid[,c("adm","date")] <- list(NULL)
      test[,c("adm","date")] <- list(NULL)
    }
    return_list$train <- train
    return_list$valid <- valid
    return_list$test  <- test
  }
  return(return_list)
}
######### REGRESSION TRAINING #########
#Run the selection of variables for the linear of beta regressions
stage2_regression <- function(df, model = "linear_regression", 
                              include_two_way_interactions = FALSE, direction_search = "both"){
  # create replicate of full df (XGBoost does splitting itself)
  df_full <- df
  value   <- df[,(names(df)== 'value')]
  df      <- df[ ,!(names(df) == "value_indicator")] #exclude value
  df      <- df[ ,!(names(df) == "adm")]
  df      <- df[ ,!(names(df) == "date")]
  
  #LINEAR REGRESSION
  if (model == "linear_regression"){
    lsMod <- lm(value ~ ., data = df)
    if (include_two_way_interactions){
      selectedLsMod <- step(lsMod, direction = direction_search, scope = list(upper = ~ .+.^2,lower=~.), 
                            trace = FALSE)
    } else {
      selectedLsMod <- step(lsMod, direction = direction_search, trace = FALSE)
    }
    
    selected_model <- selectedLsMod
  }
  
  #BETA REGRESSION
  else if (model == "beta_regression"){
    df$value <- df$value/100
    
    # Transform according to Smithson and Verkuilen 2006, if 0 and 1.
    if (any(df$value==1)||any(df$value==0)){
      n.obs <- sum(!is.na(df$value))
      df$value <- (df$value * (n.obs - 1) + 0.5) / n.obs
    }
    
    betaMod        <- betaselect(df[,!(names(df) == "value")], df$value, criterion="AIC", 
                                 method= direction_search)
    selected_model <- betaMod
  }
  else if (model == "XGBoost"){
    predictions <- XG_fit(df_full, 0.8, 4, 0.1, 40)
    selected_model <- predictions
  }
  
  return(selected_model)
}
######### ADD VALUE INDICATOR #########
add_value_indicator <- function(df, cutoff = 10){
  if (cutoff < 1){
    cutoff <- cutoff*100
  }
  value_ind <- ifelse(df$value >= cutoff, 1, 0)
  prop_ones <- sum(value_ind == 1)/nrow(df)
  #print(paste0("Proportion of ones in the data ", prop_ones))
  df <- cbind(df, value_ind)
  colnames(df)[ncol(df)] <- "value_indicator"
  return(df)
}
######### LOGIT MODEL (STAGE 1 OF THE TWO-STAGE MODEL) #########
stage1_logit <- function(df, model = "logit", include_two_way_interactions = FALSE,
                         direction_search = "both"){
  set.seed(510)
  df <- df[ ,!(names(df) == "value")] #exclude value
  df <- df[ ,!(names(df) == "adm")]
  df <- df[ ,!(names(df) == "date")]
  df <- df[rowSums(is.na(df)) == 0, ] 
  if (model == "logit"){
    logitMod <- glm(value_indicator ~. , data =  df, family = binomial(link='logit'))
    if (include_two_way_interactions){
      selectedLogitMod <- step(logitMod, direction = direction_search, 
                               scope=list(upper = ~ .+.^2,lower=~.), trace = FALSE)
    } else {
      selectedLogitMod <- step(logitMod, direction =  "backward", trace = FALSE)
    }
    selected_model <- selectedLogitMod
  }
  
  return(selected_model)
}

