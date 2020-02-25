library(xgboost)

set.seed(510)
df <- read_csv('/Users/Yur/Downloads/Ovitrap and weather (with 2 lags) data.csv')

#' @description 
#' - split datafile into the desired lengths of train and test size
#' - Note: not suitable for Cross-Validation
#' @param df data frame
#' @param split_size the fraction of the data that is used for training
#' @return four matrices: y and X both split into train and test
split_data <- function(df, split_size){
  # Remove date and province
  df[1:2] <- NULL 
  n_train <- floor(split_size*nrow(df))
  
  # shuffle df randomly
  df <- df[sample(nrow(df)), ] 
  # split into train and test
  df_train <- df[1:n_train,] 
  df_test  <- df[(n_train+1):nrow(df),]
  
  # split X and y and give as matrix output
  return_list <- list()
  return_list$y_train <- as.matrix(df_train['value'])
  return_list$y_test <- as.matrix(df_test['value'])
  
  return_list$X_train <- as.matrix(df_train[2:ncol(df_train)])
  return_list$X_test <- as.matrix(df_test[2:ncol(df_test)])
  return(return_list)
}

#' @description XGBoost algorithm
#' - Prepare train and test data suitable for XGBoost algorithm
#' - Fits the XGBoost on desired test metric
#' @param df data frame
#' @param split_size the fraction of the data that is used for training
#' @param depth the depth of the trees
#' @param learning_rate lower values are slower learners, help prevent overfitting. Default=0.1
#' @param n_rounds number of iterations
#' @return performance value of test metric
XG_fit <- function(df, split_size, depth, learning_rate, n_rounds){
  split_input = split_data(df, split_size)
  # Make input suitable for XGBoost
  dtrain <- xgb.DMatrix(split_input$X_train, label=split_input$y_train)
  dtest <- xgb.DMatrix(split_input$X_test, label=split_input$y_test)

  # Create list of evaluations that we want to check
  watchlist <- list(train=dtrain, eval=dtest)
  
  # A simple xgb.train example:
  param <- list(max_depth = depth, 
                eta = learning_rate, 
                verbose = 0,
                objective = "reg:squarederror",
                eval_metric = "rmse")
  bst <- xgb.train(param, 
                   dtrain, 
                   nrounds = n_rounds, 
                   watchlist)
  
  e <- data.frame(bst$evaluation_log)
  #plot(e$iter, e$train_rmse, col='blue')
  #lines(e$iter, e$eval_rmse, col='red')
  min(e$test_rmse)
  
  #imp <- xgb.importance(colnames(dtrain),
  #                      model=bst)
  #print(imp)
  #xgb.plot.importance(imp)
  
  p <- predict(bst, newdata=dtest)
  y_test <- unlist(split_input$y_test, use.names=FALSE)

  return(c(p, y_test))
}
predictions <- XG_fit(df, 0.8, 5, 0.1, 40)