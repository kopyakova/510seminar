library(ROCR)
set.seed(510)

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


#' @description Add the indicator column for value
#' @param df data frame
#' @param cutoff which cut-off should be used 
#' @return df with value_indicator column
add_value_indicator <- function(df, cutoff = 10){
  value_ind <- ifelse(df$value >= cutoff, 1, 0)
  prop_ones <- sum(value_ind == 1)/nrow(df)
  print(paste0("Proportion of ones in the data ", prop_ones))
  df <- cbind(df, value_ind)
  colnames(df)[ncol(df)] <- "value_indicator"
  return(df)
}

#' @description standardize weather variables
#' @param df data frame to be standardaized
#' @param variables_not_to_scale which variables must NOT be standardized
#' @return standardized data frame
scale_variables <- function(df, variables_not_to_scale = c("adm", "date", "value", "value_indicator", "longitude", "latitude")){
  variables_to_scale = colnames(df) != variables_not_to_scale
  scaled_variables <- scale(df[ ,variables_to_scale]) #scale explenatory variables
  df_scaled <- df
  df_scaled[,variables_to_scale] <- scaled_variables
  return(df_scaled)
}

#' @description Split data in training and testing set
#' @param df data frame to be split
#' @param smpl_size which sample size should be used for splitting
#' @param chronologically should the sample be split chronologically
#' @param remove_NA should rows with NA values (for lags) be removed
#' @param remove_adm_date should the columns with administration and date be removed
#' @return list with two data frames 
split_train_test <- function(df, smpl_size = 0.9, chronologically = TRUE, remove_NA = TRUE,
                             remove_adm_date = TRUE){
  n <- nrow(df)
  smp_size <- floor(smpl_size * n)
  
  if (remove_NA){ #drop NA
    df <- df[rowSums(is.na(df)) == 0, ] 
  }
  n <- nrow(df)
  #split data in training and testing sets
  if (chronologically){ #chronologically
    df <- df[order(df$date), ]
    split_date <- as.Date(df[smp_size,"date"])
    train <- df[as.Date(df[,"date"]) < as.Date(split_date), ]
    test  <- df_scaled[as.Date(df_scaled[,"date"]) >= as.Date(split_date), ]
  } else{ #random
    train_index <- sample(seq_len(n), size = smp_size)
    train <- df[train_index, ]
    test  <- df[-train_index, ]
  }
  
  #remove date and administration
  if(remove_adm_date){
    train <- train[,-c(1,2)] 
    test  <- test[,-c(1,2)] 
  }
  return_list <- list()
  return_list$train <- train
  return_list$test  <- test
  return(return_list)
}

#Find threshold function for logistic regression
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


#########TEST########
df <- read.csv("/Users/annakopyakova/Desktop/Case Study 510/Final data/Merged transformed lagged data.csv")
df <- scale_variables(df)
stage1_results <- stage1(df)

