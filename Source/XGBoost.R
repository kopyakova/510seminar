library(xgboost)

set.seed(510)
df_test  <- read.csv('../Data/test_data.csv')
df_train <- read.csv('../Data/training_data.csv')

# remove unused variables
df_train <- df_train[,!(names(df_train) == "X")]
df_train <- df_train[,!(names(df_train) == "adm")]
df_train <- df_train[,!(names(df_train) == "date")]
df_train <- df_train[ ,!(names(df_train) == "value_indicator")]

df_test <- df_test[,!(names(df_test) == "X")]
df_test <- df_test[,!(names(df_test) == "adm")]
df_test <- df_test[,!(names(df_test) == "date")]
df_test <- df_test[ ,!(names(df_test) == "value_indicator")]


#---------------------------------------------------------#
# k-Fold cross validation over eta, nrounds and max depth #
#---------------------------------------------------------#
K <- 10
n_frac <- floor(nrow(df_train)/(K+1))
iterations <- 150

smallest_error <- 100
for (depth in seq(1, 4, 1)){
  for (eta in c(0.01, 0.05, 0.1, 0.2)){
    for (iterations in seq(50, 100, 10)){
      total_error <- c()
      for (i in seq(1:K)){
        test_ind   <- c((i*n_frac):(i*n_frac+n_frac))
        data_test  <- df_train[test_ind,]
        data_train <- df_train[-test_ind,]
        
        bst <- xgboost(data = as.matrix(data_train[,!names(data_train)%in%c('value')]),
                       label = data_train[,'value'],
                       max.depth=depth,
                       eta=eta,
                       early_stopping_rounds = 3,
                       nround=iterations,
                       objective="reg:squarederror",
                       verbose=0)
        
        predictions <- predict(bst, as.matrix(data_test[,!names(data_test)%in%c('value')]), outputmargin=TRUE)
        
        err <- rmse(as.numeric(data_test[,'value']), as.numeric(predictions))
        total_error <- c(total_error, err)
      }
      if (mean(total_error) < smallest_error){
        smallest_error <- mean(total_error)
        print(paste(depth, iterations, eta, smallest_error))
        parameters <- list(depth=depth, rounds=iterations, eta=eta, smallest_error)
      }
    }
  }
}

bst <- xgboost(data = as.matrix(df_train[,!names(data_train)%in%c('value')]),
               label = df_train[,'value'],
               max.depth=parameters$depth,
               eta=parameters$eta,
               nround=parameters$rounds,
               objective = "reg:squarederror", 
               verbose=1)

######### XGBoost evaluation ######### 
source("../Results/Auxiliary_functions_results.R", echo = F)
true_train <- as.numeric(df_train[,'value'])
true_test  <- as.numeric(df_test[,'value'])

# Get predictions
xg_pred_train <- predict(bst, as.matrix(df_train[,!names(data_train)%in%c('value')]), outputmargin=TRUE)
xg_pred_test  <- predict(bst, as.matrix(df_test[,!names(data_test)%in%c('value')]), outputmargin=TRUE)

# Evaluate results
xg_results <- evaluate_results(true_train = true_train, pred_train = xg_pred_train, 
                                    true_test  = true_test, pred_test = xg_pred_test, 
                                    method_name = "XGBoost")  

# Evaluate residuls
xg_resid   <- evaluate_residuals(predicted_test = xg_pred_test, true_test = true_test, 
                                    method = "XGBoost")

#Print plots with the results
print(xg_results$plot)
xg_resid$resid_vs_pred
xg_resid$resid_vs_actual
