 autopeatfr <- function(WT, SM, Rf, Temp, imputation = c("knn", "spline", "linear", "loess"), model= "ARIMA", look_back=12, layer_units=c(32, 32), epochs=100, h = 4, R0 = 3000, dt = 1) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  library(ggplot2)

  imputation <- match.arg(imputation)
  imputations <- list(
    linear = linear_interpolation,
    knn = knn_imputation,
    spline = spline_interpolation,
    loess = loess_interpolation)

  if (anyNA(WT) || anyNA(SM) || anyNA(Rf) || anyNA(Temp)) {
    message("Missing values detected. Performing imputation using ", imputation, " method.")
    result <- imputations[[imputation]](WT, SM, Rf, Temp)
    WT <- result$Water_Table
    SM <- result$Soil_Moisture
    Rf <- result$Rainfall
    Temp <- result$Temperature
  } else {
    message("No missing values detected. Proceeding without imputation.")
    combined_data <- data.frame(
      Water_Table = WT,
      Soil_Moisture = SM,
      Rainfall = Rf,
      Temperature = Temp
    )
    par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
    for (col_name in names(combined_data)) {
      original_values <- combined_data[[col_name]]
      y_min <- min(original_values, na.rm = TRUE)
      y_max <- max(original_values, na.rm = TRUE)
      plot(original_values, type = "p", col = "blue", pch = 16,
           main = paste("Plot -", col_name),
           xlab = "Time", ylab = col_name, ylim = c(y_min, y_max), cex.main = 0.9)
    }
  }
  
  if (tolower(model)=="arima") {
    arima_results <- autopredictarima(WT, SM, Rf, Temp, h)
    predictions <- arima_results$predictions
    print("Prediction result using ARIMA :")
    print(predictions)
    
    results <- data.frame(
      Variable = character(),
      AIC = numeric(),
      BIC = numeric(),
      LjungBox_p_value = character(),
      stringsAsFactors = FALSE
    )
    columns <- c("WT", "SM", "Rf", "Temp")
    results <- data.frame()
    for (col_name in columns) {
      model <- arima_results$models[[col_name]]
      ljung_box <- arima_results$LjungBoxTest[[col_name]]
      ljung_box_p_value <- gsub(".*p-value: ", "", ljung_box)
      aic_value <- model$aic
      bic_value <- model$bic
      results <- rbind(
        results,
        data.frame(
          Variable = col_name,
          AIC = aic_value,
          BIC = bic_value,
          LjungBox_p_value = ljung_box_p_value
        )
      )
    }
    
    print(results)
  } else if (tolower(model) == "lstm") {
    lstm_results <- autopredictlstm(WT, SM, Rf, Temp, h,
                                    look_back = look_back, 
                                    lstm_layer_units = layer_units, 
                                    epochs = epochs)
    predictions <- lstm_results$predictions
  } else if (tolower(model) == "gru") {
    gru_results <- autopredictgru(WT, SM, Rf, Temp, h,
                                    look_back = look_back, 
                                    gru_layer_units = layer_units, 
                                    epochs = epochs)
    predictions <- gru_results$predictions
  } else {
    print("Model choice : ARIMA, LSTM, GRU")
    stop()
  }

  WT_pred <- predictions$WT_result
  SM_pred <- predictions$SM_result
  Rf_pred <- predictions$Rf_result
  Temp_pred <- predictions$Temp_result

  WT_full <- c(WT, WT_pred)
  Rf_full <- c(Rf, Rf_pred)
  SM_full <- c(SM, SM_pred)
  Temp_full <- c(Temp, Temp_pred)

  PFVI_results <- firepredict(
    WT = WT_full,
    SM = SM_full,
    Rf = Rf_full,
    Temp = Temp_full,
    R0 = R0,
    dt = dt,
    h = h)

  PFVI <- PFVI_results$PFVI
  last_PFVI <- tail(PFVI, h)
  PFVI_labels <- sapply(last_PFVI, function(x) {
    if (x <= 75) {
      return("Low")
    } else if (x <= 150) {
      return("Moderate")
    } else if (x <= 225) {
      return("High")
    } else {
      return("Extreme")
    }
  })
  for (i in 1:h) {
    cat(sprintf("PFVI-%d: %f (%s)\n", length(PFVI) - h + i, last_PFVI[i], PFVI_labels[i]))}

  PFVI_results$plot
}