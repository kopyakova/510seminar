imputations <- function(K, M, completedata, weather) { # I used the standard setup where k = 5 and m = 5
  # completedata <- fulldata(weather, ovitrap_cleaned, ovitrap_original)
  # completedata <- full_data_NA
  
  ######### Imputing ovitrap data #########
  
  ovitrap_imputed <- kNN(completedata, variable = "value", k=K, imp_var = F)
  
  ######### Imputing weather data #########
  weather_missing <- weather
  
  imputation_mice <- mice(weather_missing, m = M, print = FALSE) # Impute by Multiple Imputations with Chained Equations
  imputed_weather <- complete(imputation_mice)
  
  names(imputed_weather)[names(imputed_weather) == "adm_level"] <- "adm" 
  
  result <- merge(ovitrap_imputed[,1:3], imputed_weather, by = c("adm", "date"))
  result$value <- as.numeric(as.character(result$value))
  
  output <- list("completedata" = result, "imputed_weather" = imputed_weather)
  
  return(result)
}