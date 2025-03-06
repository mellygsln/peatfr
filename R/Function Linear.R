linear_interpolation <- function(WT, SM, Rf, Temp) {
  if (!requireNamespace("zoo", quietly = TRUE)) {
    install.packages("zoo")
    library(zoo)
  }
  else {library(zoo)}

  combined_data <- data.frame(
    Water_Table = WT,
    Soil_Moisture = SM,
    Rainfall = Rf,
    Temperature = Temp
  )
  original_data <- combined_data

  combined_data$Water_Table <- na.approx(combined_data$Water_Table)
  combined_data$Soil_Moisture <- na.approx(combined_data$Soil_Moisture)
  combined_data$Rainfall <- na.approx(combined_data$Rainfall)
  combined_data$Temperature <- na.approx(combined_data$Temperature)

  par(mfrow = c(2, 2), mar = c(4, 3, 3, 4) + 0.1, oma = c(1, 0, 0, 0))
  i <- 1
  name <- c("Water Table (meter)", "Soil Moisture (%)", "Rainfall (milimeter)", "Temperature (ºC)")
  for (col_name in names(combined_data)) {
    original_values <- original_data[[col_name]]
    imputed_values <- combined_data[[col_name]]
    missing_indices <- which(is.na(original_values))

    y_min <- min(c(original_values, imputed_values), na.rm = TRUE)
    y_max <- max(c(original_values, imputed_values), na.rm = TRUE)

    unit <- c("(meter)", "(%)", "(milimeter)", "(ºC)")
    plot(original_values, type = "p", col = "blue", pch = 16, main = paste("Linear Interpolation -", col_name, unit[i]),
         xlab = "Time", ylab = name[i], ylim = c(y_min, y_max), cex.main = 0.9)
    points(missing_indices, imputed_values[missing_indices], col = "red", pch = 16)
    legend(x = "bottomleft", inset = c(0, -0.3), legend = c("Original", "Interpolated"),
           col = c("blue", "red"), pch = 16, bty = "n", xpd = TRUE)
    i <- i+1
  }

  return(combined_data)
}
