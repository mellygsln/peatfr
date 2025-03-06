\name{Peat Burn Forecast}
\alias{Peat Burn Forecast}
\title{Prediction for Fire on the PeatLand}
\description{
Peat Burn Forecast consists of several functions that assist in predicting peatland fires with a single click using the `autoPeatBurnForecast()` function. Alternatively, users can perform the prediction step by step, allowing for analysis at each stage. The prediction process involves forecasting four key variables—water table, soil moisture, rainfall, and temperature—using ARIMA and Box-Cox transformation. Subsequently, Nelder-Mead optimization is applied to determine the Peat Fire Vulnerability Index (PFVI).
}
\usage{
  \item{autopeatburnforecast(WT, SM, Rf, Temp, imputation = c("knn", "spline", "linear", "loess"), h = 4, R0 = 3000, dt = 1)}
  \item{knn_imputation(WT, SM, Rf, Temp, k = 5)}
  \item{spline_interpolation(WT, SM, Rf, Temp)}
  \item{loess_interpolation(WT, SM, Rf, Temp, span = 0.5)}
  \item{linear_interpolation(WT, SM, Rf, Temp)}
  \item{autopredictarima(WT, SM, Rf, Temp, h)}
  \item{firepredict(WT, SM, Rf, Temp, R0 = 3000, dt = 1)}
  \item{plotpfvi(PFVI, h)}
}
\arguments{
  \item{WT}{A numeric vector representing the water wables.}
  \item{SM}{A numeric vector representing the soil moisture levels.}
  \item{Rf}{A numeric vector representing the rainfall amounts.}
  \item{Temp}{A numeric vector representing the temperature values.}
  \item{imputation}{Imputation options.}
  \item{h}{The number of steps to be predicted.}
  \item{R0}{Annual average rainfall.}
  \item{dt}{The time interval of the data.}
  \item{k}{The number of nearest neighbors.}
  \item{span}{The number of surrounding data points considered in estimation.}
}
\details{
The results obtained include the ARIMA models for the four key variables, diagnostic test results, predicted PFVI values, and various plots.

The predicted PFVI values are classified into four categories:
  \item{[0, 75] Low}
  \item{(75, 150] Moderate}
  \item{(150, 225] High}
  \item{(225, 300] Extreme}
}
\references{
[1] Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.
[2] Forsythe, G. E., Malcolm, M. A. and Moler, C. B. (1977). Computer Methods for Mathematical Computations. Wiley.
[3] W. S. Cleveland, E. Grosse and W. M. Shyu (1992) Local regression models. Chapter 8 of Statistical Models in S eds J.M. Chambers and T.J. Hastie, Wadsworth & Brooks/Cole.
[4] A. Kowarik, M. Templ (2016) Imputation with R package VIM. Journal of Statistical Software, 74(7), 1-16.
[5] Taufik, M., Widyastuti, M. T., Sulaiman, A., Murdiyarso, D., Santikayasa, I. P., & Minasny, B. (2022). An improved drought-fire assessment for managing fire risks in tropical peatlands. Agricultural and Forest Meteorology, 312, 108738. https://doi.org/10.1016/j.agrformet.2021.108738
[6] Nelder, J. A., & Mead, R. (1965). A Simplex Method for Function Minimization.
The Computer Journal, https://doi.org/10.1093/comjnl/7.4.308
}
\note{
Ensure that the vim, zoo, stats, forecast and ggplot2 package is installed to use this package.
}
\examples{
## Example usage of KNN_interpolation
WL <- c(-1.021, -0.972, NA, -1.204, -0.906, -0.993, -1.327, -2.001)
SM <- c(35.424, NA, 37.268, 38.453, 31.456, 33.235, 30.168, 30.212)
Rf <- c(0.00012, 0.00024, 0.00011, NA, NA, 0.00046, 0.00052, 0.00041)
Temp <- c(35.4, 35.8, NA, 36.5, 36.3, 37.2, 37.0, 38.1)

autopeatburnforecast(WT, SM, Rf, Temp, imputation = "knn", h = 4, R0 = 2700, dt = 1)
}
\keyword{prediction, peatland, burn}
