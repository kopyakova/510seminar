weather <- read.csv("/Users/annakopyakova/Desktop/Case Study 510/Final data/new_weather_cleaned.csv")
weather <- weather[, -1] #here first column is an index. must be dropped
ovitrap <- read.csv("/Users/annakopyakova/Desktop/Case Study 510/Final data/Full_data_kNN_imputed.csv")
data <- aggregate_data(weather, ovitrap, F)


#' @description Merge two data sets; add 2 lags; perform log transformation if necessary
#' @param weather data frame for weather with 14 variables (administration, date and all the variables)
#' @param ovitrap data frame for ovitrap with variables. Must include administration (adm), date and value
#' @param log_transf should variables perc and new perc, ls_temp day and night, windspeed be log transform
aggregate_data <- function(weather, ovitrap, log_transf = FALSE){
  all_variable_names <- colnames(weather)
  p <- dim(weather)[2]
  #Check if weather has the right label for administration
  if (!("adm" %in% all_variable_names)){
    index <- which(all_variable_names == "adm_level")
    all_variable_names[index] <- "adm"
    colnames(weather)[index] <- "adm"
  }
  
  #Drop redundant variables from ovitrap data
  ovitrap <- ovitrap[,c("adm","date","value")]
  
  #Merge the data sets
  merged_data = merge(ovitrap, weather, by=c("adm","date"))
  
  all_variable_names <- colnames(merged_data)
  if (log_transf){
    #Log transformation
    merged_data$perc <- log(merged_data$perc + 1) 
    merged_data$ls_temp_day <- log(merged_data$ls_temp_day)
    merged_data$ls_temp_night <- log(merged_data$ls_temp_night)
    merged_data$wind_speed <- log(merged_data$wind_speed)
    merged_data$new_perc <- log(merged_data$new_perc + 1)
    
    all_variable_names[which(all_variable_names == "perc")] <- "log_perc"
    all_variable_names[which(all_variable_names == "ls_temp_day") ] <- "log_ls_temp_day"
    all_variable_names[which(all_variable_names == "ls_temp_night")] <- "log_ls_temp_night"
    all_variable_names[which(all_variable_names == "wind_speed") ] <- "log_wind_speed"
    all_variable_names[which(all_variable_names == "new_perc") ] <- "log_new_perc"
    
    colnames(merged_data) <- all_variable_names
  }

  #add lagsvars_to_be_lagged = colnames(merged_data)[4:(p-2)]
  vars_to_be_lagged <- all_variable_names[4:(p-1)] #omit adm, date, value, longitute and latitude 
  
  print(all_variable_names)
  lag1 = make_lags(merged_data, num_lags = 1, vars_to_be_lagged = vars_to_be_lagged)
  lag2 = make_lags(merged_data, num_lags = 2, vars_to_be_lagged = vars_to_be_lagged)
  
  merged_data2 = merge(merged_data, lag1, by=c("adm","date"))
  merged_data3 = merge(merged_data2, lag2, by=c("adm","date"))
  
  return(merged_data3)
}
######ADD LAGS######
#' @description Create a data frame with lagged values
#' @param data matrix or data frame of interest
#' @param id_index name of the variable that has crossectional index
#' @param date_index name of the variable that has time series index
#' @param num_lags how many time periods should be lagges
#' @param vars_to_be_lagged list of variable that must be lagged
make_lags <- function(data, id_index = "adm", date_index = "date", num_lags = 1, log = FALSE,
                      vars_to_be_lagged = c("ns_temp", "evi", "perc", "ls_temp_day", "wind_speed", "humid", "ls_temp_night",
                                            "total_perc_rate", "soil_moist", "soil_temp")){
  if (log){
    vars_to_be_lagged <- paste0("log_", vars_to_be_lagged)
  }
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
