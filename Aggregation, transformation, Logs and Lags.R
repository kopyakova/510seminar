library(normtest)

#load weather data
weather <- read.csv("/Users/annakopyakova/Desktop/Case Study 510/Final data/median_weather_transformed.csv")
weather <- weather[, -1]
#load rename administration level to match mosquito data
colnames(weather)[1] <- "adm"

#load mosquito data
mosq <- read.csv("/Users/annakopyakova/Desktop/Case Study 510/Final data/Incorrect_Imputations.csv")
mosq <- mosq[order(mosq$adm, mosq$date),]
mosq <- mosq[order(mosq$adm, mosq$date),]
#drop redundant variables
mosq <- mosq[c(2,3,5)]
#merge the data sets
merged_data = merge(mosq, weather, by=c("adm","date"))

######CHECK NORMALITY######
p <- dim(merged_data)[2]
#skip first two columns (adm and date) and skip last two columnd (longitde and latitude)
for (i in 3:(p-2)){
  var_name <- colnames(merged_data)[i]
  variable <- merged_data[,i]
  variable <- variable[!is.na(variable)]
  
  print(var_name)
  print(paste0("Skewness is ", skewness(variable)))
  print(paste0("Kurtosis is ", kurtosis(variable)))
  print("JB test")
  print(jb.norm.test(variable))
  hist(variable, main = paste("Histogram of" , var_name), breaks = 1000)
  hist(variable, main = paste("Histogram of" , var_name), breaks = 1000)
}
######Log transformation######
merged_data$log_ns_temp <- log(merged_data$ns_temp)
merged_data$log_evi <- log(merged_data$evi)
merged_data$log_perc <- log(merged_data$perc + 1)
merged_data$log_ls_temp_day <- log(merged_data$ls_temp_day)
merged_data$log_ls_temp_night <- log(merged_data$ls_temp_night)
merged_data$log_humid <- log(merged_data$humid)
merged_data$log_wind_speed <- log(merged_data$wind_speed)

######ADD LAGS######
#' @description Create a data frame with lagged values
#' @param data matrix or data frame of interest
#' @param id_index name of the variable that has crossectional index
#' @param date_index name of the variable that has time series index
#' @param num_lags how many time periods should be lagges
#' @param vars_to_be_lagged list of variable that must be lagged
make_lags <- function(data, id_index = "adm", date_index = "date", num_lags = 1,
                      vars_to_be_lagged = c("ns_temp", "evi", "perc", "ls_temp_day", "wind_speed", "humid", "ls_temp_night")){
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
  return(lagged_vars)
}
vars_to_be_lagged = c( "ns_temp", "evi", "perc", "ls_temp_day", "wind_speed", "humid", "ls_temp_night",
                       "log_ns_temp", "log_evi", "log_perc", "log_ls_temp_day", "log_wind_speed", "log_humid", "log_ls_temp_night")
lag1 = make_lags(merged_data, num_lags = 1, vars_to_be_lagged = vars_to_be_lagged)
lag2 = make_lags(merged_data, num_lags = 2, vars_to_be_lagged = vars_to_be_lagged)
merged_data2 = merge(merged_data, lag1, by=c("adm","date"))
merged_data3 = merge(merged_data2, lag2, by=c("adm","date"))

#write.csv(merged_data3,'/Users/annakopyakova/Desktop/Case Study 510/Final data/Merged transformed lagged data.csv', row.names = FALSE)

######SPLIT DATA INTO TRAIN AND TEST######
smp_size <- floor(0.75 * nrow(merged_data3))

## set the seed to make your partition reproducible
set.seed(510)
train_index <- sample(seq_len(nrow(merged_data3)), size = smp_size)

training_data <- merged_data3[train_ind, ]
testing_data <- merged_data3[-train_ind, ]

# write.csv(training_data,'/Users/annakopyakova/Desktop/Case Study 510/Final data/Training data.csv', row.names = FALSE)
# write.csv(training_data,'/Users/annakopyakova/Desktop/Case Study 510/Final data/Test data.csv', row.names = FALSE)
