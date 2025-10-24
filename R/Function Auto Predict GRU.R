#' Forecast Multiple Time Series using Customizable GRU Models
#'
#' This function builds an independent GRU model for each input time series
#' and forecasts a specified number of steps ahead. The model's architecture
#' (number of layers and neurons) is fully customizable.
#'
#' @param WT Numeric vector for the 'Water Table' time series.
#' @param SM Numeric vector for the 'Soil Moisture' time series.
#' @param Rf Numeric vector for the 'Rainfall' time series.
#' @param Temp Numeric vector for the 'Temperature' time series.
#' @param h Integer, the forecast horizon (how many steps to predict).
#' @param look_back Integer, the number of past observations to use for prediction. Default is 12.
#' @param gru_layer_units A numeric vector specifying the number of neurons for each GRU layer. 
#'        For example, c(50) for one layer, or c(100, 50) for two layers. Default is c(50).
#' @param epochs Integer, the number of training epochs. Default is 100.
#'
#' @return A list containing a data frame of the predictions.
autopredictgru <- function(WT, SM, Rf, Temp, h, 
                            look_back = 12, 
                            gru_layer_units = c(50), 
                            epochs = 100) {
  # Check and install the keras R package
  if (!requireNamespace("keras3", quietly = TRUE)) {
    cat("Installing the 'keras3' R package...\n")
    install.packages("keras3")
  }
  library(keras3)
  
  # Check and install the tensorflow R package
  if (!requireNamespace("tensorflow", quietly = TRUE)) {
    cat("Installing the 'tensorflow' R package...\n")
    install.packages("tensorflow")
  }
  library(tensorflow)
  
  # Check if the TensorFlow Python backend is installed and accessible
  if (!reticulate::py_module_available("tensorflow")) {
    cat("TensorFlow Python backend not found.\n")
    cat("Attempting a one-time installation. This may take a few minutes...\n")
    install_tensorflow()
  }
  cat("All required packages are ready.\n")
  
  data_combined <- data.frame(WT, SM, Rf, Temp)
  columns <- c("WT", "SM", "Rf", "Temp")
  predictions <- list()
  
  for (col_name in columns) {
    cat(paste("\n--- Processing column:", col_name, "---\n"))
    
    # Data Preparation (Scaling and Sequencing)
    series_data <- data_combined[[col_name]]
    if (!is.numeric(series_data)) {
      warning(paste("Column", col_name, "is not numeric. Skipping."))
      next
    }
    
    min_val <- min(series_data)
    max_val <- max(series_data)
    scaled_series <- (series_data - min_val) / (max_val - min_val)
    
    split_index <- floor(length(scaled_series) * 0.8)
    train_data <- scaled_series[1:split_index]
    test_data <- scaled_series[(split_index + 1):length(scaled_series)]
    
    create_sequences <- function(dataset, look_back) {
      dataX <- list()
      dataY <- list()
      for (i in 1:(length(dataset) - look_back)) {
        dataX[[i]] <- dataset[i:(i + look_back - 1)]
        dataY[[i]] <- dataset[i + look_back]
      }
      list(x = do.call(rbind, dataX), y = do.call(rbind, dataY))
    }
    
    train_processed <- create_sequences(train_data, look_back)
    train_X <- train_processed$x
    train_Y <- train_processed$y
    
    test_processed <- create_sequences(test_data, look_back)
    test_X <- test_processed$x
    test_Y <- test_processed$y
    
    train_X <- array_reshape(train_X, c(nrow(train_X), look_back, 1))
    test_X <- array_reshape(test_X, c(nrow(test_X), look_back, 1))
    
    # Build the Customizable GRU Model
    inputs <- layer_input(shape = c(look_back, 1))
    
    # Dynamically build the GRU layers based on user input
    x <- inputs
    num_layers <- length(gru_layer_units)
    for (i in 1:num_layers) {
      units <- gru_layer_units[i]
      # All layers except the last one must return sequences
      return_sequences_flag <- (i < num_layers)
      
      x <- x %>% layer_gru(units = units, return_sequences = return_sequences_flag)
    }
    
    outputs <- x %>% layer_dense(units = 1)
    model <- keras_model(inputs = inputs, outputs = outputs)
    
    model %>% compile(loss = 'mean_squared_error', optimizer = 'adam')
    
    cat("Training GRU model with architecture:", gru_layer_units, "\n")
    model %>% fit(train_X, train_Y, epochs = epochs, batch_size = 32, verbose = 0)
    
    # Make Predictions and Evaluate 
    predictions_scaled <- model %>% predict(test_X, verbose = 0)
    
    predictions_test <- predictions_scaled * (max_val - min_val) + min_val
    actuals <- test_Y * (max_val - min_val) + min_val
    
    mse <- mean((predictions_test - actuals)^2)
    rmse <- sqrt(mse)
    mae <- mean(abs(predictions_test - actuals))
    cat("\nTest Set MSE:", mse, "\n")
    cat("\nTest Set RMSE:", rmse, "\n")
    cat("\nTest Set MAE:", mae, "\n")
    
    # Iterative Forecasting
    current_sequence <- tail(scaled_series, look_back)
    forecasted_values_scaled <- numeric(h)
    
    for (i in 1:h) {
      input_for_pred <- array_reshape(current_sequence, c(1, look_back, 1))
      next_pred_scaled <- model %>% predict(input_for_pred, verbose = 0)
      forecasted_values_scaled[i] <- next_pred_scaled
      current_sequence <- c(current_sequence[-1], next_pred_scaled)
    }
    
    # Inverse-transform and store predictions
    forecasted_values <- forecasted_values_scaled * (max_val - min_val) + min_val
    if (col_name == "Rf") {
      forecasted_values[forecasted_values < 0] <- 0
    }
    predictions[[col_name]] <- forecasted_values
    
    # Plotting
    name_map <- c(WT = "Water Table (meter)", SM = "Soil Moisture (%)", 
                  Rf = "Rainfall (millimeter)", Temp = "Temperature (ºC)")
    
    par(mfrow = c(1, 1))
    plot(series_data, type = "l", col = "blue", lwd = 2,
         main = paste("GRU Forecast -", name_map[col_name]),
         xlab = "Time", ylab = name_map[col_name],
         xlim = c(1, length(series_data) + h))
    lines(seq(length(series_data) + 1, length(series_data) + h), 
          forecasted_values, col = "red", lwd = 2, type = "l")
    legend("topleft", legend = c("Original Data", "Forecasted Data"), 
           col = c("blue", "red"), lty = 1, lwd = 2, bty = "n")
  }
  
  # Combine predictions into a final data frame
  results_df <- data.frame(
    WT_result = predictions[["WT"]],
    SM_result = predictions[["SM"]],
    Rf_result = predictions[["Rf"]],
    Temp_result = predictions[["Temp"]]
  )
  
  return(list(predictions = results_df))
}
