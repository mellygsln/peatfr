loess_interpolation <- function(WT, SM, Rf, Temp, span = 0.5) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    install.packages("stats")
    library(stats)
  }
  else {library(stats)}

  interpolate_loess <- function(column_data, span) {
    valid_index <- !is.na(column_data)
    if (all(valid_index)) {
      return(column_data)
    }
    loess_fit <- loess(column_data[valid_index] ~ seq_along(column_data)[valid_index], span = span)
    interpolated_values <- predict(loess_fit, newdata = seq_along(column_data))
    column_data[is.na(column_data)] <- interpolated_values[is.na(column_data)]
    return(column_data)
  }

  combined_data <- data.frame(
    Water_Table = WT,
    Soil_Moisture = SM,
    Rainfall = Rf,
    Temperature = Temp
  )

  water_table_interpolated <- interpolate_loess(combined_data$Water_Table, span)
  soil_moisture_interpolated <- interpolate_loess(combined_data$Soil_Moisture, span)
  rainfall_interpolated <- interpolate_loess(combined_data$Rainfall, span)
  temperature_interpolated <- interpolate_loess(combined_data$Temperature, span)

  final_result <- data.frame(
    Water_Table = water_table_interpolated,
    Soil_Moisture = soil_moisture_interpolated,
    Rainfall = rainfall_interpolated,
    Temperature = temperature_interpolated
  )

  par(mfrow = c(2, 2), mar = c(4, 3, 3, 4) + 0.1, oma = c(1, 0, 0, 0))
  i <- 1
  name <- c("Water Table (meter)", "Soil Moisture (%)", "Rainfall (milimeter)", "Temperature (ºC)")
  for (col_name in names(combined_data)) {
    original_values <- combined_data[[col_name]]
    imputed_values <- final_result[[col_name]]
    missing_indices <- which(is.na(original_values))

    y_min <- min(c(original_values, imputed_values), na.rm = TRUE)
    y_max <- max(c(original_values, imputed_values), na.rm = TRUE)

    unit <- c("(meter)", "(%)", "(milimeter)", "(ºC)")
    plot(original_values, type = "l", col = "blue", pch = 16, main = paste("LOESS Interpolation -", name[i]),
         xlab = "Time", ylab = name[i], ylim = c(y_min, y_max), cex.main = 0.9)
    points(missing_indices, imputed_values[missing_indices], col = "red", pch = 16)
    legend(x = "bottomleft", inset = c(0, -0.3), legend = c("Original", "Interpolated"),
           col = c("blue", "red"), lty = c(1, NA), pch = c(NA, 16), lwd = c(2, NA), pt.cex = c(NA, 1.5), bty = "n", xpd = TRUE)
    i <- i+1
  }

  return(final_result)
}
