firepredict <- function(WT, SM, Rf, Temp, R0 = 3000, dt = 1, h) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  
  DF <- function(nPFVI, Temp, R0, dt) {
    return((300 - nPFVI) * (0.4982 * exp(0.0905 * Temp + 1.6096) - 4.268) * dt * 10^(-3) / (1 + 10.88 * exp(-0.00173582677165354 * R0)))
  }

  RF <- function(Rf, Rf_b) {
    if (is.na(Rf_b) || Rf_b <= 5.1) {
      if (Rf < 5.1) {
        return(0)
      } else {
        return(Rf - 5.1)
      }
    } else {
      return(Rf)
    }
  }

  WTF <- function(aH, bH, n, h, alpha) {
    m <- 1 - 1/n
    theta <- (1 + (h/alpha)^n)^(-m)
    return(aH - bH * ((1 - theta) * 300))
  }

  PFVI <- function(WT, Rf, Rf_b, Temp, aH, bH, n, alpha, R0, dt) {
    Time <- length(WT)
    h <- numeric(Time)
    for (i in 1:Time) {
      h[i] <- ifelse(WT[i] > 0, 0, -WT[i])
    }
    x <- numeric(Time + 1)
    x[1] <- DIobs(SM[1], 40, 70)
    df <- numeric(Time)
    rf <- numeric(Time)
    wtf <- numeric(Time)
    for (i in 1:Time) {
      x0 <- max(min(x[i], 300), 0)
      df[i] <- DF(x0, Temp[i], R0, dt)
      rf[i] <- RF(Rf[i], Rf_b[i])
      wtf[i] <- WTF(aH, bH, n, h[i], alpha)
      x[i + 1] <- x0 + df[i] - rf[i] - wtf[i]
    }
    return(list(PFVI = x[-1], Water_Distribution = df, Rainfall = rf, Soil_Fluctuation = wtf, Water_Depth = h))
  }

  DIobs <- function(SM, fc, sat) {
    return(300 * (1 - ((SM - fc) / (sat - fc))))
  }

  optm_func <- function(data, par) {
    with(data, mean((PFVI(WT, Rf, Rf_b, Temp, par[1], par[2], par[3], par[4], R0, dt)[[1]] - DIobs(SM, 40, 70))^2))
  }

  Rf_b <- c(Rf[-length(Rf)], NA)
  PAR <- c(0.1, 0.1, 0.1, 0.1)
  data <- data.frame(WT, SM, Rf, Rf_b, Temp)

  OptI <- optim(par = PAR, fn = optm_func, data = data)
  Min <- OptI$value
  par1 <- OptI$par[1]
  par2 <- OptI$par[2]
  par3 <- OptI$par[3]
  par4 <- OptI$par[4]

  m <- 1/par3
  for (i in 1:m) {
    for (j in 1:m) {
      for (k in 1:m) {
        for (l in 1:m) {
          opt <- optim(par = c(0.2 * i, 0.2 * j, 0.2 * k, 0.2 * l), fn = optm_func, data = data, hessian = FALSE)
          opt <- optim(par = c(opt$par[1], opt$par[2], opt$par[3], opt$par[4]), fn = optm_func, data = data, hessian = FALSE)
          if ((opt$par[3] >= 0) & (opt$par[4] > 0)) {
            if (opt$value < Min) {
              Min <- opt$value
              par1 <- opt$par[1]
              par2 <- opt$par[2]
              par3 <- opt$par[3]
              par4 <- opt$par[4]
              PAR <- c(0.2*i,0.2*j,0.2*k,0.2*l)
            }
          }
        }
      }
    }
  }

  optim_output <- optim(par = c(par1, par2, par3, par4), fn = optm_func, data = data, hessian = FALSE)
  final_params <- optim_output$par

  result <- PFVI(WT, Rf, Rf_b, Temp, final_params[1], final_params[2], final_params[3], final_params[4], R0, dt)
  result_DIobs <- DIobs(SM, 40, 70)

  result_PFVI <- result$PFVI
  result_PFVI <- pmax(pmin(result_PFVI, 300), 0)
  result_DIobs <- pmax(pmin(result_DIobs, 300), 0)

  if (length(result_PFVI) != length(result_DIobs)) {
    stop("The lengths of PFVI and DIobs must be the same.")
  }

  Time <- seq_along(result_PFVI)
  df_plot <- data.frame(
    Time = Time,
    PFVI = result_PFVI,
    DIobs = result_DIobs
  )

  df_plot_line <- df_plot[1:(length(Time) - h), ]
  df_plot_dot <- df_plot[(length(Time) - h + 1):length(Time), ]

  p <- ggplot() +
    geom_line(data = df_plot_line, aes(x = Time, y = PFVI, color = "PFVI"), linetype = "dashed", linewidth = 1) +
    geom_point(data = df_plot_dot, aes(x = Time, y = PFVI, color = "PFVI Predict"), size = 2) +
    labs(
      x = "Time",
      y = "PFVI (-)",
      title = "PFVI and PFVI Predict Plot"
    ) +
    scale_color_manual(values = c(
      "PFVI" = "blue",
      "PFVI Predict" = "red",
    )) +
    ylim(-100, 400) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14)
    )

  print(p)

  return(list(PFVI = result_PFVI,
              DIobs = result_DIobs,
              TransformationParameters = final_params,
              plot = p))
}
