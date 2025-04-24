# Peat Fire Risk (peatfr) Package 🌟
![Build Status](https://img.shields.io/badge/build-passing-brightgreen) ![License](https://img.shields.io/badge/license-MIT-blue)

The peat fire risk package or peatfr package provide comprehensive framework to forecast tropical peatland fire through stochastics and optimisation  methods. The peatfr consists of three main functions, including, data imputation process, time series forecasting, and drought fire index calculation. This package provides four imputation process, which can be employed to estimate the missing values of time series data of water table, soil moisture, rainfall, and air temperature. The forecasting process employs ARIMA stochastics model combined with Box-Cox transformation. The drought fire index is calculated through Peat Fire Vulnerability Index with Nelder-Mead optimisation to obtain the best parameters automatically.

## Functions
- autopeatfr(WT, SM, Rf, Temp, imputation = c("knn", "spline", "linear", "loess"), h = 4, R0 = 3000, dt = 1)
- knn_imputation(WT, SM, Rf, Temp, k = 5)
- spline_interpolation(WT, SM, Rf, Temp)
- loess_interpolation(WT, SM, Rf, Temp, span = 0.5)
- linear_interpolation(WT, SM, Rf, Temp)
- autopredictarima(WT, SM, Rf, Temp, h)
- firepredict(WT, SM, Rf, Temp, R0 = 3000, dt = 1, h)

## Getting Started
### Prerequisites
1. VIM package
2. stats package
3. forecast package
4. ggplot2 package

Install the dependencies using:
```bash
install.packages("devtools") # If not installed yet
devtools::install_github("mellygsln/peatfr", dependencies = TRUE)

library(peatfr)
```

## Project Structure
```
peatfr/
|-- man/                     
    |-- peatfirerisk.Rd/
|-- R                
    |-- Function Auto Peat Fire Risk/
    |-- Function Auto Predict ARIMA/
    |-- Function Fire Predict/
    |-- Function KNN/
    |-- Function Linear/
    |-- Function Loess/
    |-- Function Spline/
|-- .Rbuildignore                
|-- .Rhistory                        
|-- DESCRIPTION
|-- NAMESPACE
|-- peatfirerisk.Rproj
```


