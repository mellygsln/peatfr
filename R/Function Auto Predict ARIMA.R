autopredictarima <- function(WT, SM, Rf, Temp, h) {
  data_combined <- data.frame(WT, SM, Rf, Temp)
  columns <- c("WT", "SM", "Rf", "Temp")
  models <- list()
  LjungBoxTest <- list()
  predictions <- list()
  lambda_optimal <- numeric(length(columns))
  model_orders <- list()

  for (i in seq_along(columns)) {
    col_name <- columns[i]

    if (is.numeric(data_combined[[col_name]])) {
      k <- 0
      if (col_name %in% columns) {
        min_value <- min(data_combined[[col_name]], na.rm = TRUE)
        k <- abs(min_value) + 1
        data_combined[[col_name]] <- data_combined[[col_name]] + k
      }
      lambda_optimal[i] <- BoxCox.lambda(data_combined[[col_name]], lower = -2, upper = 2)
      fit <- auto.arima(data_combined[[col_name]], lambda = lambda_optimal[i], biasadj = TRUE)

      models[[col_name]] <- fit
      residuals <- residuals(models[[col_name]])
      LjungBox <- Box.test(residuals, lag = 10, type = "Ljung-Box")$p.value
      LjungBoxTest[[col_name]] <- LjungBox
      model_orders[[col_name]] <- arimaorder(fit)
      forecasted <- forecast(fit, h = h)
      predictions[[col_name]] <- forecasted$mean - k
      data_combined[[col_name]] <- data_combined[[col_name]] - k
      if (col_name == "Rf") {
        predictions[[col_name]][predictions[[col_name]] < 0] <- 0}

      cat("ARIMA Model for", col_name, ":",
          "(", model_orders[[col_name]][1], ",", model_orders[[col_name]][2], ",", model_orders[[col_name]][3], ")\n")
    } else {
      warning(paste("Column", col_name, "is not numeric"))
    }
  }

  results_df <- data.frame(
    WT_result = as.vector(predictions[["WT"]]),
    SM_result = as.vector(predictions[["SM"]]),
    Rf_result = as.vector(predictions[["Rf"]]),
    Temp_result = as.vector(predictions[["Temp"]])
  )

  i <- 1
  for (col_name in columns) {
    name <- c("Water Table (meter)", "Soil Moisture (%)", "Rainfall (milimeter)", "Temperature (ºC)")

    par(mfrow = c(1, 2), mar = c(6, 4, 4, 2) + 0.1, oma = c(3, 0, 0, 0))

    original_data <- data_combined[[col_name]]
    forecasted_data <- as.vector(predictions[[col_name]])
    ymin <- min(c(original_data, forecasted_data), na.rm = TRUE)
    ymax <- max(c(original_data, forecasted_data), na.rm = TRUE)

    plot(original_data, type = "l", col = "blue", lwd = 2,
         main = paste("ARIMA Forecast -", name[i]),
         xlab = "Time",
         ylab = name[i],
         xlim = c(1, length(original_data) + length(forecasted_data)),
         ylim = c(ymin, ymax))
    lines(seq(length(original_data) + 1, length(original_data) + length(forecasted_data)),
          forecasted_data, col = "red", lwd = 2)
    legend("bottomleft", legend = c("Original Data", "Forecasted Data"),
           col = c("blue", "red"), lwd = 2, bty = "n", cex = 0.8, inset = c(0, -0.3), xpd = TRUE)

    total_length <- length(original_data) + length(forecasted_data)
    last_25_original <- rep(NA, total_length)
    last_25_original[(total_length - 24):length(original_data)] <- original_data[(total_length - 24):length(original_data)]
    zoom_ymin <- min(c(last_25_original, forecasted_data), na.rm = TRUE)
    zoom_ymax <- max(c(last_25_original, forecasted_data), na.rm = TRUE)
    plot(last_25_original, type = "l", col = "blue", lwd = 2,
         main = paste("Zoom-in -", name[i]),
         xlab = "Time",
         ylab = name[i],
         xlim = c((total_length - 24), total_length),
         ylim = c(zoom_ymin, zoom_ymax))
    points(seq(length(original_data) + 1, total_length), forecasted_data, col = "red", lwd = 2)
    legend("bottomleft", legend = c("Original Data", "Forecasted Data"),
           col = c("blue", "red"), lwd = 2, bty = "n", cex = 0.8, inset = c(0, -0.3), xpd = TRUE)
    i <- i + 1
  }

  return(list(models = models, LjungBoxTest = LjungBoxTest, predictions = results_df, model_orders = model_orders))
}
