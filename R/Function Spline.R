spline_interpolation <- function(WT, SM, Rf, Temp) {
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

  water_table_interpolated <- na.spline(combined_data$Water_Table, na.rm = FALSE)
  soil_moisture_interpolated <- na.spline(combined_data$Soil_Moisture, na.rm = FALSE)
  rainfall_interpolated <- na.spline(combined_data$Rainfall, na.rm = FALSE)
  temperature_interpolated <- na.spline(combined_data$Temperature, na.rm = FALSE)

  final_result <- data.frame(
    Water_Table = water_table_interpolated,
    Soil_Moisture = soil_moisture_interpolated,
    Rainfall = rainfall_interpolated,
    Temperature = temperature_interpolated
  )

  par(mfrow = c(2, 2), mar = c(4, 3, 3, 4) + 0.1, oma = c(1, 0, 0, 0))
  i <- 1
  name <- c("Water Table (meter)", "Soil Moisture (%)", "Rainfall (milimeter)", "Temperature (ºC)")
  for (col_name in names(final_result)) {
    original_values <- combined_data[[col_name]]
    imputed_values <- final_result[[col_name]]
    missing_indices <- which(is.na(original_values))

    y_min <- min(c(original_values, imputed_values), na.rm = TRUE)
    y_max <- max(c(original_values, imputed_values), na.rm = TRUE)

    unit <- c("(meter)", "(%)", "(milimeter)", "(ºC)")
    plot(original_values, type = "p", col = "blue", pch = 16, main = paste("Spline Interpolation -", col_name, unit[i]),
         xlab = "Time", ylab = name[i], ylim = c(y_min, y_max), cex.main = 0.9)
    points(missing_indices, imputed_values[missing_indices], col = "red", pch = 16)
    legend(x = "bottomleft", inset = c(0, -0.3), legend = c("Original", "Interpolated"),
           col = c("blue", "red"), pch = 16, bty = "n", xpd = TRUE)
    i <- i+1
  }

  return(final_result)
}
