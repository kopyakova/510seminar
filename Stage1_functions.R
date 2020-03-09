library(ROCR)
#' @description Stage 1: 
#' - add value indicator
#' - split data in training and validation set (optional)
#' - find thresholds based on F1 score or sum of spec/sens
#' - select thresholds based on the prediction accuracy on validation set
#' - return model and threshold
#' @param df data frame
#' @param model type of model to be used (currently only logit)
#' @param split_sample inficates if the validation set be used to find threshold? If FALSE training test is used
#' @param include_two_way_interactions inficates if two way interactions are incuded in the variable selection
#' @param direction_search direction for variable selection: "forward", "backward", "both"
#' @param chronologically if split_sample is TRUE, inficates if the data be split chronologocally
#' @return list. First element selected Logit model. Second element best threshold
stage1 <- function(df, model = "logit", split_sample = TRUE, include_two_way_interactions = FALSE,
                   direction_search = "both", chronologically = FALSE){
  df <- add_value_indicator(df)
  df <- df[ ,-which(names(df) == "value")] #exclude value
  #split sample if needed
  if (split_sample){
    split_data <- split_train_test(df, chronologically = chronologically, smpl_size = 0.9, remove_NA = TRUE)
    train <- data.frame(split_data$train)
    test  <- data.frame(split_data$test)
  } else {
    train <- data.frame(df)
    test  <- data.frame(df)
  }
  
  if (model == "logit"){
    logitMod <- glm(value_indicator ~. , data =  train, family = binomial(link='logit'))
    if (include_two_way_interactions){
      selectedLogitMod <- step(logitMod, direction = direction_search, scope=list(upper = ~ .+.^2,lower=~.), trace = FALSE)
    } else {
      selectedLogitMod <- step(logitMod, direction =  direction_search, trace = FALSE)
    }
    
    #make a prediction on the training test to select cutoffs
    predicted_train <- selectedLogitMod$fitted.values
    target_train    <- train[,"value_indicator"]
    cutoffs   <- t(find_threshold(predicted_train, target_train))  #two thresholds
    
    #find number of misclassified observations
    predicted_test <- predict(selectedLogitMod, test[,-which(names(test) == "value_indicator")], type="response")
    target_test    <- test[,"value_indicator"]
    
    missclassTest <- c()
    for (cutoff in cutoffs){
      missclassTest <- c(missclassTest, sum(ifelse(predicted_test > cutoff,1,0) != target_test))
    }
    #select cut-off that minimizes number of miscalssifications
    bestMethodInd <- which.min(missclassTest)
    if (bestMethodInd == 2){
      method = "F1 score"
    } else {
      method = "Sum of sensitivity and specificity"
    }
    best_cutoff <- cutoffs[bestMethodInd]
    
    accuracy      <- sum(ifelse(predicted_train > best_cutoff,1,0) == target_train)/nrow(train)
    print(paste0("The accuracy (train set) based on the ", method, " is ", accuracy))
    #evaluate test set with the best cut-off
    accuracy      <- sum(ifelse(predicted_test > best_cutoff,1,0) == test[,"value_indicator"])/nrow(test)
    print(paste0("The accuracy (validation set) based on the ", method, " is ", accuracy))
  }
  
  return_list <- list()
  return_list$selected_model <- selectedLogitMod
  return_list$threshold      <- best_cutoff
  return(return_list)
}

#' @description Find a threshold for logistic regression
#' @param train data frame with training set
#' @param valid data frame with validation set
#' @param model object outputted by glm or step function
#' @param dependent_variable name (string)
#' @return best threshold (integer)
threshold_bootstrap <- function(train, valid = NULL, model, 
                                dependent_variable = "value_indicator"){
  if (is.null(valid)){
    stop("Validation set is needed")
  }

  train <- train[ ,!(names(train) == "value")] #exclude value
  valid <- valid[ ,!(names(valid) == "value")]
  train <- train[rowSums(is.na(train))==0,]
  valid <- valid[rowSums(is.na(valid))==0,]
  #make a prediction on the training test to select cutoffs
  predicted_train <- model$fitted.values
  target_train    <- train[ ,dependent_variable]

  cutoffs   <- t(find_threshold(predicted_train, target_train))  #two thresholds
  
  #find number of misclassified observations
  predicted_test <- predict(model, valid[,-which(names(valid) == dependent_variable)], 
                            type="response")
  target_test    <- valid[, dependent_variable]
 
  missclassTest <- c()
  for (cutoff in cutoffs){
    missclassTest <- c(missclassTest, sum(ifelse(predicted_test > cutoff,1,0) != target_test))
  }
  #select cut-off that minimizes number of miscalssifications
  bestMethodInd <- which.min(missclassTest)
  if (bestMethodInd == 2){
    method = "F1 score"
  } else {
    method = "Sum of sensitivity and specificity"
  }
  best_cutoff <- cutoffs[bestMethodInd]
  
  accuracy      <- sum(ifelse(predicted_train > best_cutoff,1,0) == target_train)/nrow(train)
  print(paste0("The accuracy (train set) based on the ", method, " is ", accuracy))
  
  #evaluate test set with the best cut-off
  accuracy      <- sum(ifelse(predicted_test > best_cutoff,1,0) == target_test)/nrow(valid)
  print(paste0("The accuracy (validation set) based on the ", method, " is ", accuracy))
  
  return(best_cutoff)
}

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

#' @description Find threshold function for logistic regression
#' @param predicted predicted values
#' @param target true values
find_threshold <- function(predicted, target) {
  #make a prediction object (library ROCR)
  prediction_obj <- prediction(predicted, target)
  
  #performance baced on specificity and sencitivity
  perfSENS_SPEC <- performance(prediction_obj, measure="sens", x.measure="spec")
  df            <- data.frame(cut = perfSENS_SPEC@alpha.values[[1]], sens = perfSENS_SPEC@x.values[[1]], 
                              spec = perfSENS_SPEC@y.values[[1]])
  cut_SENS_SPEC <- df[which.max(df$sens + df$spec), "cut"]
  plot(perfSENS_SPEC, xlim=rev(c(0, 1)), main = "ROC curve")
  
  #performance baced on precision and recall
  perfPR_REC <- performance(prediction_obj, measure="prec", x.measure="rec")
  df         <- data.frame(cut = perfPR_REC@alpha.values[[1]], prec = perfPR_REC@x.values[[1]], 
                           rec = perfPR_REC@y.values[[1]])
  cut_F1 <- df[which.max(2*(df$prec*df$rec)/(df$prec+df$rec)), "cut"]
  plot(x = perfPR_REC@alpha.values[[1]], y = 2*(df$prec*df$rec)/(df$prec+df$rec), main = "F1 score vs cutoff")
  
  return_list <- c()
  return_list[1]  <- cut_SENS_SPEC
  return_list[2]  <- cut_F1
  return(return_list)
}

#' @description standardize weather variables
#' @param df data frame to be standardaized
#' @param variables_not_to_scale which variables must NOT be standardized
#' @return standardized data frame
scale_variables <- function(df, variables_not_to_scale = c("adm", "date", "value", "value_indicator", "longitude", "latitude")){
  all_variable_names <- colnames(df)
  indices_to_exclude <- match(all_variable_names, intersect(all_variable_names, variables_not_to_scale))
  variables_to_scale <- all_variable_names[is.na(indices_to_exclude)]
  scaled_variables   <- scale(df[ ,variables_to_scale]) #scale explenatory variables
  df_scaled <- df
  df_scaled[,variables_to_scale] <- scaled_variables
  return(df_scaled)
}


#' @description Add the indicator column for value
#' @param df data frame
#' @param cutoff which cut-off should be used 
#' @return df with value_indicator column
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

#' @description Split data in training and testing set
#' @param df data frame to be split
#' @param train which fraction should be used for training set
#' @param validate which fraction should be used for validation set
#'  if NA there will be no validation set in the output
#' @param chronologically should the sample be split chronologically
#' @param remove_NA should rows with NA values (for lags) be removed
#' @param remove_adm_date should the columns with administration and date be removed
#' @return list with two data frames 
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


#' @description Create a data frame with lagged values
#' @param data matrix or data frame of interest
#' @param id_index name of the variable that has crossectional index
#' @param date_index name of the variable that has time series index
#' @param num_lags indicates the "distance" in time of the lag. Num_lags 1 is the previous month
#' @param vars_to_be_lagged list of variable that must be lagged
make_lags <- function(data, weather_data, id_index = "adm", date_index = "date", num_lags = 1, imputation = FALSE,
                      vars_to_be_lagged =c("ls_temp_day", "ls_temp_night", "evi", "humid", 
                                           "new_perc", "soil_moist", "soil_temp", "ns_temp", "wind_speed")){
  set.seed(510)
  # id_index = "adm"
  # date_index = "date"
  # num_lags = 1
  # imputation = T
  # vars_to_be_lagged =c("ls_temp_day", "ls_temp_night", "evi", "humid", 
  #                      "new_perc", "soil_moist", "soil_temp", "ns_temp", "wind_speed")
  # weather_data <- 
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
      # print(date_1)
      # print(date1_lag_1)
      # print(".")
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
     
      # if (month_2 == 1) {
      #   
      # } else if ((month_2-2) > 9) {
      #   date2_lag_2 <- paste0(split_date[[1]][1], "-", month_2-2, "-", split_date[[1]][3])
      # } else {
      #   date2_lag_2 <- paste0(split_date[[1]][1], "-0", month_2-2, "-", split_date[[1]][3])
      # }
      # 
      # if (month_2 == 1) {
      #   year <- as.numeric(split_date[[1]][1])
      #   date2_lag_1 <- paste0(year-1, "-", 10, "-", split_date[[1]][3])
      # } else if ((month_2-3) > 9) {
      #   date2_lag_1 <- paste0(split_date[[1]][1], "-", month_2-3, "-", split_date[[1]][3])
      # } else {
      #   date2_lag_1 <- paste0(split_date[[1]][1], "-0", month_2-3, "-", split_date[[1]][3])
      # }
      
      for (i in 1:n_months){
        months <- missing_months[i]
        if (months == date_1){
          #find weather data for the missing month
          # print(date_1)
          # print(date2_lag_1)
          # print(".")
          weather_month_lag1  <- weather_data[as.Date(weather_data$date) == date2_lag_1,]

          #merge the data
          missing_data <- df_NA[as.Date(df_NA$date) == date_1,]
          missing_data <- missing_data[,c(1,2)]
          
          merged <- merge(missing_data, weather_month_lag1, by=c("adm"))
          merged <- merged[, -c(3)] #drop second date
          df_NA[as.Date(df_NA$date) == date_1,] = merged
        }
        if (months == date_2){
          #find weather data for the missing months
          # print(date_2)
          # print(date2_lag_2)
          # print(".")
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


