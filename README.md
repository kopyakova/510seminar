# Predicting Mosquito Density in the Philippines using Weather-Related Variables
Code for a Seminar Case Study using OLS, Beta regression and XGBoost to predict ovitrap indices as a proxy for mosquito density.

All software is written in both R (https://www.r-project.org/) and Python (https://www.python.org/). 

## Paper Extract
The World Health Organization estimates that more than 50% of Earth's population is at risk of contracting dengue, a disease with a 0.04% death rate. Currently, it causes an estimated 40,000 people to die every year. The Philippines is one of the 124 countries that suffer from dengue outbreaks, caused by *Aedes aegypti* mosquitoes. We investigate whether it is possible to predict the density of these mosquitoes based on weather conditions and related factors in the Philippines. Several econometric and machine learning prediction techniques are used to examine this relationship. More specifically, we apply OLS, beta regression, a two-stage model, and the XGBoost algorithm. As the information about the population of *Aedes aegypti* mosquitoes is hard to gather, we deal with missing values in the data set. To account for the missing value mechanism and the uncertainty due to the imputation in the variable selection, we develop a new method based on the bootstrap algorithm. This method has shown to improve the predictive performance of econometric models, compared to the classical step-wise backward selection approach. The results indicate that XGBoost has the best predictive performance. This implies that the relationship between weather and *Aedes aegypti* presence is more complex than a linear relationship. 

## Software Explanation
Description of the source code used in this project. Note that the datacleaning (part 1) is done in Python and that the imputation, analysis and Prediction (part 2) are done in R.

### Part 1: Data Cleaning/Filtering
- School_Filter.py: filters ovitrap observations measured by a wide variety of schools in the Philippines and prepares the dataset that is used for further analysis.
- Weather transformation.py: removes non-plausible weather data from the dataset and scales variables to desirable measurements

### Part 2: Imputation, Analysis and Prediction
- Auxiliary_functions.R: --
- XGBoost.R: applies k-Fold cross validation XGBoost algorithm to predict mean ovitrap indices and outputs the desired fitted model, based on the full training set
- main_analysis.R: --

## Data Description
All data used in this research is positioned in the data folder.

- ovitrap_data_per_month_per_province_cleaned.csv: --
- ovitrap_data_per_month_per_province...
- raw_weather_data.csv: ..
- test_final.csv: ..
- training_data.csv: ..
- weather_data.csv: ..

## References
