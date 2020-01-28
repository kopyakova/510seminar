install.packages("VGAM")
install.packages("regclass")
install.packages("car")
install.packages("censReg")

library(VGAM)
library(car)
library(regclass)
library(censReg)

data <- complete_threeyears

#tobit_regress <- vglm(mean_ovi~ data$JAXA_GPM_L3_GSMaP_v6_operational_hourlyPrecipRate + data$MODIS_006_MYD13A1_EVI, tobit(Upper = 100, Lower = 0), data = data)
tobit_regress <- censReg(mean_ovi ~ data$JAXA_GPM_L3_GSMaP_v6_operational_hourlyPrecipRate + data$MODIS_006_MOD11A1_LST_Day_1km + data$MODIS_006_MYD13A1_EVI, left = 0, right = 100, data = data)

# Interpretation of Tobit model regression!

VIF(tobit_regress)
# A general guideline is that a VIF larger than 5 or 10 is large, indicating that the model has problems 
# estimating the coefficient. 
# However, this in general does not degrade the quality of predictions. 
# If the VIF is larger than 1/(1-R2), where R2 is the Multiple R-squared of the regression, 
#then that predictor is more related to the other predictors than it is to the response.

AIC(tobit_regress)
# Compare different models to each other: forward selecting or backward elimination