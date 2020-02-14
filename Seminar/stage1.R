library(leaps)
library(ROCR)


#Find threshold function for logistic regression
find_threshold <- function(predicted, target) {
  prediction_obj <- prediction(predicted, target)
  
  perfSENS_SPEC <- performance(prediction_obj, measure="sens", x.measure="spec")
  df            <- data.frame(cut = perfSENS_SPEC@alpha.values[[1]], sens = perfSENS_SPEC@x.values[[1]], 
                              spec = perfSENS_SPEC@y.values[[1]])
  cut_SENS_SPEC <- df[which.max(df$sens + df$spec), "cut"]
  plot(perfSENS_SPEC, xlim=rev(c(0, 1)), main = "ROC curve")
  
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

df <- read.csv("/Users/annakopyakova/Desktop/Case Study 510/Final data/Merged transformed lagged data.csv")
n <- nrow(df)
p <- ncol(df)

value_ind <- ifelse(df$value >= 10,1,0)
prop_ones <- sum(value_ind == 1)/n #87% if 20 - 80%
df <- cbind(df, value_ind)
colnames(df)[p+1] <- "value_indicator"

variableNames <- colnames(df)[4:p] #don't include data, adm and value and value indicator
#variableNames <- variableNames[-c(11:12)] #drop longitude and latitude
p <- length(variableNames)

#STANDARDIZA THE DATA
scaled_variables <- scale(df[ ,variableNames]) #scale explenatory variables
df_scaled <- df
df_scaled[,variableNames] <- scaled_variables 
head(df_scaled)

######SPLIT DATA INTO TRAIN AND TEST CHRONOLOGICALLY######
## set the seed to make your partition reproducible
set.seed(510)
df_scaled <- df_scaled[order(df_scaled$date),]
df_scaled <- df_scaled[rowSums(is.na(df_scaled)) == 0, ] #drop NA
n <- nrow(df_scaled)
smpl_size <- 0.75
smp_size <- floor(smpl_size * n)

split_date <- as.Date(df_scaled[n*smpl_size,"date"]) #2015-06-01
train <- df_scaled[as.Date(df_scaled[,"date"]) < as.Date(split_date), ]
train <- train[,-c(1,2)]
test <- df_scaled[as.Date(df_scaled[,"date"]) >= as.Date(split_date), ]
test <- test[,-c(1,2)] 
  
#not chronologically
# train_index <- sample(seq_len(n), size = smp_size)
# train <- df_scaled[train_index, ]
# train <- train[,-c(1,2)]
# test <- df_scaled[-train_index, ]
# test <- test[,-c(1,2)] 


######STAGE 1 PREDICT MOSQUITO PRESENCE - Logistic Regression######

#Select variables

#Backward Stepwise Regression based on AIC
logitMod <- glm(value_indicator ~ . , data = train[ -which(names(train) == "value")], family=binomial(link='logit'))
#select using backward and forward selection with two way interactions
selectedLogitMod <- step(logitMod, direction = "both", scope=list(upper = ~ .+.^2,lower=~.))

#evaluate model on the training test and select threshold
predicted <- selectedLogitMod$fitted.values
target    <-  train[,"value_indicator"]
cutoffs   <- t(find_threshold(predicted, target))


missclassTrain <- c()
for (cutoff in cutoffs){
  missclassTrain <- c(missclassTrain, sum(ifelse(predicted > cutoff,1,0) != target))
}

bestMethodInd <- which.min(missclassTrain)
if (bestMethodInd == 2){
  method = "F1 score"
} else {
  method = "Sum of sensitivity and specificity"
}
best_cutoff <- cutoffs[bestMethodInd]

#print ROC curve

accuracy      <- sum(ifelse(predicted > best_cutoff,1,0) == train[,"value_indicator"])/nrow(train)
print(paste0("The accuracy (train set) based on the ", method, " is ", accuracy))
#evaluate test set with the best cut-off
predictedTest <- predict(selectedLogitMod, test[,variableNames], type="response")
accuracy      <- sum(ifelse(predictedTest > best_cutoff,1,0) == test[,"value_indicator"])/nrow(test)
print(paste0("The accuracy (test set) based on the ", method, " is ", accuracy))


# all_vifs <- car::vif(selectedMod)
# print(all_vifs)



# ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
# 
# mod_fit <- train(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own + 
#                    CreditHistory.Critical,  data=GermanCredit, method="glm", family="binomial",
#                  trControl = ctrl, tuneLength = 5)
# 
# pred = predict(mod_fit, newdata=testing)
# confusionMatrix(data=pred, testing$Class)

#select best subset
# regsubsetsObj  <- regsubsets(x = train[, variableNames], 
#                              y =  train[,"value_indicator"], 
#                              nbest = 2, really.big = T)