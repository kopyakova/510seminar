# Predicting Mosquito Density in the Philippines using Weather-Related Variables
Code for a Seminar Case Study using OLS, Beta regression and XGBoost to predict ovitrap indices as a proxy for mosquito density.

All software is written in both R (https://www.r-project.org/) and Python (https://www.python.org/). 

## Software Explanation

- School_Filter.py: filters ovitrap observations measured by a wide variety of schools in the Philippines and prepares the dataset that is used for further analysis.
- Weather transformation.py: removes non-plausible weather data from the dataset and scales variables to desirable measurements
- Auxiliary_functions.R: --
- XGBoost.R: applies k-Fold cross validation XGBoost algorithm to predict mean ovitrap indices and outputs the desired fitted model, based on the full training set
- main_analysis.R: --

## Directory explanation
