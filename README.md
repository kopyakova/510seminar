# Predicting Mosquito Density in the Philippines using Weather-Related Variables
Code for a Seminar Case Study using OLS, Beta regression and XGBoost to predict ovitrap indices as a proxy for mosquito density.

All software is written in both R (https://www.r-project.org/) and Python (https://www.python.org/). 

## Paper Extract
The World Health Organization estimates that more than 50% of Earth's population is at risk of contracting dengue, a disease with a 0.04% death rate. Currently, it causes an estimated 40,000 people to die every year. The Philippines is one of the 124 countries that suffer from dengue outbreaks, caused by *Aedes aegypti* mosquitoes. We investigate whether it is possible to predict the density of these mosquitoes based on weather conditions and related factors in the Philippines. Several econometric and machine learning prediction techniques are used to examine this relationship. More specifically, we apply OLS, beta regression, a two-stage model, and the XGBoost algorithm. As the information about the population of *Aedes aegypti* mosquitoes is hard to gather, we deal with missing values in the data set. To account for the missing value mechanism and the uncertainty due to the imputation in the variable selection, we develop a new method based on the bootstrap algorithm. This method has shown to improve the predictive performance of econometric models, compared to the classical step-wise backward selection approach. The results indicate that XGBoost has the best predictive performance. This implies that the relationship between weather and *Aedes aegypti* presence is more complex than a linear relationship. The selected variables in the models reveal that no single featurehas a large individual influence on mosquito density.  Instead, it can be assumed that a combination ofweather features affects the density of mosquitoes 

## Software Explanation
Description of the source code used in this project. Note that the data cleaning (part 1) is done in Python and that the imputation, analysis and prediction (part 2) are done in R.

### Part 1: Data Cleaning/Filtering
- School_Filter.py: filters ovitrap observations measured by a wide variety of schools in the Philippines and prepares the dataset that is used for further analysis.
- Weather transformation.py: removes non-plausible weather data from the dataset and scales variables to desirable measurements.

### Part 2: Imputation, Analysis and Prediction
- XGBoost.R: applies the k-Fold cross validation XGBoost algorithm to predict mean ovitrap indices and outputs the desired fitted model, based on the full training set.
- main_analysis.R: The file contains the functions for training the OLS and the beta regressions, the first stage of the two-stage model and the main function, which calls the analysis. The main function loads auxiliary functions, which allows it to combine the ovitrap and weather data, impute the full data sets, create lagged values, and split the data in training and testing sets. Furthermore, it is possible to call OLS or beta regressions with the inclusion of a possible first stage (logistic regression that selects risky provinces for the analysis).
- Auxiliary_functions.R: the working horse of the main_analysis.R. This file contains functions that create bootstrapped samples, complete bootstrapped samples by imputation and select variables for the OLS and beta regressions. It also contains the functions for completing the full data set, creation of lags, data set splitting, actual calls to the regression functions, as well as logit model of stage one in the two-stage model.

## Required packages for R
- betareg      (Version 3.1.3)
- doParallel   (Version 1.0.15)
- frmselection (Version 0.1.0)<sup>1</sup>
- ggplot2      (Version 3.2.1)
- Metrics      (Version 0.1.4)
- mice         (Version 3.8.0)
- VIM          (Version 5.1.0)
- xgboost      (Version 0.90.0.2)

<sup>1</sup> To install this package one needs to call `install.packages("remotes")` in R console, followed by `remotes::install_github("Lydia2kkx/frmselection")`.

## Obtaining the results
To obtain the results described in the research, one should pull this repository. You can run the econometric models (OLS and beta) by sourcing or running line by line *Run_OLS_and_beta.R* from the Results folder. To get the results of XGBoost, it is possible to source or running line by line *XGBoost.R* from Source folder. These files load corresponding data and all relevant functions and documents.

## Data Description
All data used in this research is collected from open-source databases. The ovitrap data is gathered by the Philippines' Dengue Vector Surveillance Program and is publically available at http://dengue.pchrd.dost.gov.ph/. Furthermore, the weather data is gathered using the Google Earth Engine (https://developers.google.com/earth-engine/datasets/catalog/). The used datasets are then described as follows: 

- ovitrap_data_per_month_per_province.csv: ovitrap data that is aggregated on a monthly basis;
- ovitrap_data_per_month_per_province_cleaned.csv: cleaned ovitrap data that is aggregated on a monthly basis. Excluding imputed observations;
- raw_weather_data.csv: raw weather data collected from the Google Earth Engine;
- weather_data.csv: cleaned and imputed weather data that is used for the analysis;
- training_data.csv: the part of the dataset that is used for training, including both the dependent and explanatory variables. Note that the first 2 columns (index and adm) are not included in training;
- test_data.csv: the part of the dataset that is used for testing, including both the dependent and explanatory variables.

## References
Gorelick, N., Hancher, M., Dixon, M., Ilyushchenko, S., Thau, D., & Moore, R. (2017). Google Earth Engine: Planetary-scale geospatial analysis for everyone. Remote Sensing of Environment.
