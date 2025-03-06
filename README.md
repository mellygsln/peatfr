# Peat Burn Forecast Package 🌟
![Build Status](https://img.shields.io/badge/build-passing-brightgreen) ![License](https://img.shields.io/badge/license-MIT-blue)

Peat Burn Forecast consists of several functions that assist in predicting peatland fires with a single click using the `autoPeatBurnForecast()` function. Alternatively, users can perform the prediction step by step, allowing for analysis at each stage. The prediction process involves forecasting four key variables—water table, soil moisture, rainfall, and temperature—using ARIMA and Box-Cox transformation. Subsequently, Nelder-Mead optimization is applied to determine the Peat Fire Vulnerability Index (PFVI).

## Functions
- autopeatburnforecast(WT, SM, Rf, Temp, imputation = c("knn", "spline", "linear", "loess"), h = 4, R0 = 3000, dt = 1)
- knn_imputation(WT, SM, Rf, Temp, k = 5)
- spline_interpolation(WT, SM, Rf, Temp)
- loess_interpolation(WT, SM, Rf, Temp, span = 0.5)
- linear_interpolation(WT, SM, Rf, Temp)
- autopredictarima(WT, SM, Rf, Temp, h)
- firepredict(WT, SM, Rf, Temp, R0 = 3000, dt = 1)
- plotpfvi(PFVI, h)

## Getting Started
### Prerequisites
1. VIM package
2. stats package
3. forecast package
4. ggplot2 package

Install the dependencies using:
```bash
pip install -r requirements.txt
```

## Project Structure
```
peatburnforecast/
|-- man/                     
    |-- peatburnforecast.Rd/
|-- R                
    |-- Function Auto Peat Burn Forecast/
    |-- Function Auto Predict ARIMS/
    |-- Function Fire Predict/
    |-- Function KNN/
    |-- Function Linear/
    |-- Function Loess/
    |-- Function Plot PFVI/
    |-- Function Spline/
|-- .Rbuildignore                
|-- .Rhistory                        
|-- DESCRIPTION
|-- NAMESPACE
|-- peatburnforecast.Rproj
```


