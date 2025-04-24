knn_imputation <- function(WT, SM, Rf, Temp, k = 5) {
  if (!requireNamespace("VIM", quietly = TRUE)) {
    install.packages("VIM")
    library(VIM)
  }
  else {library(VIM)}

  combined_data <- data.frame(
    Water_Table = WT,
    Soil_Moisture = SM,
    Rainfall = Rf,
    Temperature = Temp
  )

  na_count <- colSums(is.na(combined_data))
  if (any(na_count > (nrow(combined_data) / 2))) {
    stop("Too many NA values to calculate KNN. Ensure there is sufficient data.")
  }

  result_knn <- kNN(combined_data, variable = c("Water_Table", "Soil_Moisture", "Rainfall", "Temperature"), k = k)
  final_result <- result_knn[, c("Water_Table", "Soil_Moisture", "Rainfall", "Temperature")]

  par(mfrow = c(2, 2), mar = c(6, 4, 4, 4) + 0.1, oma = c(1, 0, 0, 0))
  i <- 1
  name <- c("Water Table (meter)", "Soil Moisture (%)", "Rainfall (milimeter)", "Temperature (ºC)")
  for (col_name in names(combined_data)) {
    original_values <- combined_data[[col_name]]
    imputed_values <- final_result[[col_name]]
    missing_indices <- which(is.na(original_values))

    y_min <- min(c(original_values, imputed_values), na.rm = TRUE)
    y_max <- max(c(original_values, imputed_values), na.rm = TRUE)

    unit <- c("(meter)", "(%)", "(milimeter)", "(ºC)")
    plot(original_values, type = "l", col = "blue", pch = 16, main = paste("kNN Imputation -", name[i]),
         xlab = "Time", ylab = name[i], ylim = c(y_min, y_max), cex.main = 0.9, cex.lab = 1.2, cex.axis = 1.2)
    points(missing_indices, imputed_values[missing_indices], col = "red", pch = 16)
    legend(x = "bottomleft", inset = c(0, -0.45), legend = c("Original", "Imputed"),
           col = c("blue", "red"), lty = c(1, NA), pch = c(NA, 16), lwd = c(2, NA), pt.cex = c(NA, 1.5), bty = "n", xpd = TRUE)
    i <- i+1
  }

  return(final_result)
}
